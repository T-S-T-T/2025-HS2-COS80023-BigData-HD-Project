# -------------------------------------------------
# Research Question:
# Does road geometry (intersections, curves, straight roads) affect crash severity?
# -------------------------------------------------

# Load libraries
library(dplyr)
library(ggplot2)
library(nnet)
library(forcats)
library(cowplot)
library(effects)

# -----------------------------
# 1. Import only required data
# -----------------------------
accident <- read.csv("../data/accident.csv")

# -----------------------------
# 2. Select relevant columns
# -----------------------------
crash_data <- accident %>%
  dplyr::select(ACCIDENT_NO, ROAD_GEOMETRY, SEVERITY)

# -----------------------------
# 3. Clean, recode, and keep only three geometries
# -----------------------------
crash_data <- crash_data %>%
  mutate(ROAD_GEOMETRY = factor(ROAD_GEOMETRY,
                                levels = 1:9,
                                labels = c("Cross intersection", "T intersection", "Y intersection",
                                           "Multiple intersections", "Not at intersection",
                                           "Dead end", "Road closure", "Private property", "Unknown"))) %>%
  filter(!is.na(ROAD_GEOMETRY), !is.na(SEVERITY)) %>%
  filter(ROAD_GEOMETRY %in% c("Not at intersection", "Cross intersection", "T intersection")) %>%
  droplevels()

# Create ordered severity categories
crash_data <- crash_data %>%
  mutate(SEVERITY_CAT = factor(SEVERITY,
                               levels = c(1, 2, 3, 4),
                               labels = c("Fatal", "Serious Injury", "Other Injury", "Non Injury"),
                               ordered = TRUE)) %>%
  mutate(SEVERITY_CAT = droplevels(SEVERITY_CAT))

# >>> Print counts of crashes by geometry and severity <<<
cat("\nCrash counts by road geometry and severity:\n")
print(table(crash_data$ROAD_GEOMETRY, crash_data$SEVERITY_CAT))

# Create binary severe vs non-severe variable
crash_data <- crash_data %>%
  mutate(SEVERE_BIN = ifelse(SEVERITY %in% c(1, 2), 1, 0))

# Set reference for geometry
crash_data <- crash_data %>%
  mutate(ROAD_GEOMETRY = fct_relevel(ROAD_GEOMETRY, "Not at intersection"))

# -----------------------------
# 4. Exploratory visualizations
# -----------------------------
p_counts <- ggplot(crash_data, aes(x = ROAD_GEOMETRY, fill = SEVERITY_CAT)) +
  geom_bar() +
  labs(y = "Count", x = "Road geometry",
       title = "Crash severity counts by road geometry") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_props <- ggplot(crash_data, aes(x = ROAD_GEOMETRY, fill = SEVERITY_CAT)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", x = "Road geometry",
       title = "Crash severity proportions by road geometry") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(p_counts, p_props, ncol = 2)

# -----------------------------
# 5. Chi-square test
# -----------------------------
geom_sev_tab <- table(crash_data$ROAD_GEOMETRY, crash_data$SEVERITY_CAT)
chi_out <- chisq.test(geom_sev_tab)
print(chi_out)

# Note: If the p-value is near 0, it means the difference in severity patterns
# across road geometries is very unlikely to be due to chance → strong evidence
# that road geometry and crash severity are related.

# -----------------------------
# 6. Logistic regression (binary severe vs non-severe) + forest plot
# -----------------------------
logit_model <- glm(SEVERE_BIN ~ ROAD_GEOMETRY,
                   data = crash_data, family = binomial)

summary(logit_model)

coefs <- summary(logit_model)$coefficients
OR <- exp(coefs[,1])
lower <- exp(coefs[,1] - 1.96 * coefs[,2])
upper <- exp(coefs[,1] + 1.96 * coefs[,2])

or_table <- data.frame(
  term = rownames(coefs),
  estimate = OR,
  conf.low = lower,
  conf.high = upper,
  p.value = coefs[,4]
)

geom_terms <- or_table %>%
  dplyr::filter(grepl("^ROAD_GEOMETRY", term)) %>%
  dplyr::mutate(term_clean = gsub("^ROAD_GEOMETRY", "", term),
                term_clean = ifelse(term_clean == "", "(ref)", term_clean))

p_forest <- ggplot(geom_terms,
                   aes(x = reorder(term_clean, estimate),
                       y = estimate,
                       ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_pointrange(size = 0.4) +
  coord_flip() +
  labs(title = "Adjusted odds ratios: severe crash by road geometry",
       x = "Road geometry (reference: Not at intersection)",
       y = "Odds ratio (with 95% Wald CI)") +
  theme_minimal()

print(p_forest)
print(geom_terms %>% dplyr::select(term_clean, estimate, conf.low, conf.high, p.value))

# -----------------------------
# 7. Multinomial regression (4 severity categories) + bar chart of predicted probabilities
# -----------------------------
multi_model <- multinom(SEVERITY_CAT ~ ROAD_GEOMETRY,
                        data = crash_data, trace = FALSE)

summary(multi_model)

# Get predicted probabilities for each observation
pred_probs <- as.data.frame(predict(multi_model, type = "probs"))
pred_probs$ROAD_GEOMETRY <- crash_data$ROAD_GEOMETRY

# Average predicted probabilities by geometry
pred_summary <- pred_probs %>%
  group_by(ROAD_GEOMETRY) %>%
  summarise(across(everything(), mean)) %>%
  tidyr::pivot_longer(-ROAD_GEOMETRY,
                      names_to = "Severity",
                      values_to = "Predicted_Prob")

# Convert to percentages
pred_summary <- pred_summary %>%
  mutate(Percent = Predicted_Prob * 100)

# Bar chart with percentage labels
p_pred_bar <- ggplot(pred_summary,
                     aes(x = ROAD_GEOMETRY, y = Percent, fill = Severity)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f%%", Percent)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  labs(title = "Predicted probability of crash severity by road geometry",
       x = "Road geometry", y = "Predicted probability (%)") +
  theme_minimal()

print(p_pred_bar)

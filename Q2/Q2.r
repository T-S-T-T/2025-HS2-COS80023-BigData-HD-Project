# -------------------------------------------------
# Research Question:
# Does light condition affect crash severity?
# -------------------------------------------------

# Load libraries
library(dplyr)
library(ggplot2)
library(nnet)
library(forcats)
library(cowplot)
library(effects)
library(scales)

# -----------------------------
# 1. Import only required data
# -----------------------------
accident <- read.csv("../data/accident.csv")

# -----------------------------
# 2. Select relevant columns
# -----------------------------
crash_data <- accident %>%
  dplyr::select(ACCIDENT_NO, LIGHT_CONDITION, SEVERITY)

# -----------------------------
# 3. Clean and recode variables
# -----------------------------
crash_data <- crash_data %>%
  mutate(
    LIGHT_CAT = case_when(
      LIGHT_CONDITION %in% c(1, "1") ~ "Daylight",
      LIGHT_CONDITION %in% c(2, "2") ~ "Dawn/Dusk",
      LIGHT_CONDITION %in% c(3, "3") ~ "Dark - Street lit",
      LIGHT_CONDITION %in% c(4, "4") ~ "Dark - No street lights",
      TRUE                           ~ "Other/Unknown"
    ),
    SEVERITY_CAT = factor(SEVERITY,
                          levels = c(1, 2, 3, 4),
                          labels = c("Fatal", "Serious Injury", "Other Injury", "Non Injury"),
                          ordered = TRUE),
    SEVERE_BIN = ifelse(SEVERITY %in% c(1, 2), 1, 0)
  ) %>%
  filter(!is.na(SEVERITY), !is.na(LIGHT_CAT)) %>%
  mutate(LIGHT_CAT = factor(LIGHT_CAT)) %>%
  droplevels()

# >>> Print counts of crashes by lighting and severity <<<
cat("\nCrash counts by lighting and severity:\n")
print(table(crash_data$LIGHT_CAT, crash_data$SEVERITY_CAT))

# -----------------------------
# 4. Exploratory visualizations
# -----------------------------
p_counts <- ggplot(crash_data, aes(x = LIGHT_CAT, fill = SEVERITY_CAT)) +
  geom_bar() +
  labs(y = "Count", x = "Lighting condition",
       title = "Crash severity counts by lighting") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_props <- ggplot(crash_data, aes(x = LIGHT_CAT, fill = SEVERITY_CAT)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", x = "Lighting condition",
       title = "Crash severity proportions by lighting") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(p_counts, p_props, ncol = 2)

# -----------------------------
# 5. Chi-square test
# -----------------------------
light_tab <- table(crash_data$LIGHT_CAT, crash_data$SEVERITY_CAT)
chi_out <- chisq.test(light_tab)
print(chi_out)

# -----------------------------
# 6. Logistic regression (binary severe vs non-severe) + forest plot
# -----------------------------
logit_model <- glm(SEVERE_BIN ~ LIGHT_CAT,
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

light_terms <- or_table %>%
  dplyr::filter(grepl("^LIGHT_CAT", term)) %>%
  dplyr::mutate(term_clean = gsub("^LIGHT_CAT", "", term),
                term_clean = ifelse(term_clean == "", "(ref)", term_clean))

p_forest <- ggplot(light_terms,
                   aes(x = reorder(term_clean, estimate),
                       y = estimate,
                       ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_pointrange(size = 0.4) +
  coord_flip() +
  labs(title = "Adjusted odds ratios: severe crash by lighting",
       x = "Lighting condition (reference: Daylight)",
       y = "Odds ratio (with 95% Wald CI)") +
  theme_minimal()

print(p_forest)
print(light_terms %>% dplyr::select(term_clean, estimate, conf.low, conf.high, p.value))

# -----------------------------
# 7. Multinomial regression (4 severity categories) + predicted probabilities
# -----------------------------
multi_model <- multinom(SEVERITY_CAT ~ LIGHT_CAT,
                        data = crash_data, trace = FALSE)

summary(multi_model)

# Predicted probabilities
pred_probs <- as.data.frame(predict(multi_model, type = "probs"))
pred_probs$LIGHT_CAT <- crash_data$LIGHT_CAT

# Average predicted probabilities by lighting
pred_summary <- pred_probs %>%
  group_by(LIGHT_CAT) %>%
  summarise(across(everything(), mean)) %>%
  tidyr::pivot_longer(-LIGHT_CAT,
                      names_to = "Severity",
                      values_to = "Predicted_Prob") %>%
  mutate(Percent = Predicted_Prob * 100)

# Bar chart with percentage labels
p_pred_bar <- ggplot(pred_summary,
                     aes(x = LIGHT_CAT, y = Percent, fill = Severity)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f%%", Percent)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  labs(title = "Predicted probability of crash severity by lighting",
       x = "Lighting condition", y = "Predicted probability (%)") +
  theme_minimal()

print(p_pred_bar)

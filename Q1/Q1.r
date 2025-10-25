# -------------------------------------------------
# Research Question:
# Does road geometry (intersections, curves, straight roads) affect crash severity?
# -------------------------------------------------

# Load libraries
library(dplyr)
library(ggplot2)
library(nnet)      # multinomial regression
library(broom)     # tidy model outputs
library(effects)   # model effects / predicted probabilities
library(reshape2)  # melt contingency tables for heatmaps
library(forcats)   # factor manipulation
library(cowplot)   # combine plots

# -----------------------------
# 1. Import relevant tables
# -----------------------------
accident <- read.csv("../data/accident.csv")
location <- read.csv("../data/accident_Location.csv")
surface  <- read.csv("../data/road_surface_cond.csv")
weather  <- read.csv("../data/atmospheric_cond.csv")

# -----------------------------
# 2. Select only relevant columns
# -----------------------------
accident_sel <- accident %>%
  dplyr::select(ACCIDENT_NO, ROAD_GEOMETRY, SEVERITY, SPEED_ZONE,
                LIGHT_CONDITION, NO_OF_VEHICLES)

location_sel <- location %>%
  dplyr::select(ACCIDENT_NO, ROAD_TYPE)

surface_sel <- surface %>%
  dplyr::select(ACCIDENT_NO, SURFACE_COND)

# Collapse multiple weather conditions into one row per accident
weather_collapsed <- weather %>%
  group_by(ACCIDENT_NO) %>%
  summarise(ATMOSPH_COND = paste(unique(ATMOSPH_COND), collapse = ", "))

# -----------------------------
# 3. Join all tables
# -----------------------------
crash_data <- accident_sel %>%
  left_join(location_sel, by = "ACCIDENT_NO") %>%
  left_join(surface_sel, by = "ACCIDENT_NO") %>%
  left_join(weather_collapsed, by = "ACCIDENT_NO")

# -----------------------------
# 4. Clean and recode variables
# -----------------------------
# Convert ROAD_GEOMETRY to factor with labels
crash_data <- crash_data %>%
  mutate(ROAD_GEOMETRY = factor(ROAD_GEOMETRY,
                                levels = 1:9,
                                labels = c("Cross intersection", "T intersection", "Y intersection",
                                           "Multiple intersections", "Not at intersection",
                                           "Dead end", "Road closure", "Private property", "Unknown")))

# Remove missing geometry or severity
crash_data <- crash_data %>%
  filter(!is.na(ROAD_GEOMETRY), !is.na(SEVERITY))

# Create ordered severity categories
crash_data <- crash_data %>%
  mutate(SEVERITY_CAT = factor(SEVERITY,
                               levels = c(1, 2, 3, 4),
                               labels = c("Fatal", "Serious Injury", "Other Injury", "Non Injury"),
                               ordered = TRUE))

# Create binary severe vs non-severe variable
crash_data <- crash_data %>%
  mutate(SEVERE_BIN = ifelse(SEVERITY %in% c(1, 2), 1, 0))

# Optional: Set a meaningful reference for geometry (commonly "Not at intersection")
crash_data <- crash_data %>%
  mutate(ROAD_GEOMETRY = fct_relevel(ROAD_GEOMETRY, "Not at intersection"))

# -----------------------------
# 5. Exploratory Visualizations
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

# Show side-by-side
plot_grid(p_counts, p_props, ncol = 2)

# -----------------------------
# 6. Chi-square test + heatmap
# -----------------------------
geom_sev_tab <- table(crash_data$ROAD_GEOMETRY, crash_data$SEVERITY_CAT)
chi_out <- chisq.test(geom_sev_tab)
print(chi_out)

# Heatmap of contingency table
geom_sev_df <- as.data.frame(geom_sev_tab)
colnames(geom_sev_df) <- c("ROAD_GEOMETRY", "SEVERITY_CAT", "Freq")

p_heat <- ggplot(geom_sev_df, aes(x = SEVERITY_CAT, y = ROAD_GEOMETRY, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Crash severity by road geometry (counts)",
       x = "Severity category", y = "Road geometry", fill = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_heat)

# -----------------------------
# 7. Logistic regression (binary severe vs non-severe) + forest plot
# -----------------------------

# Fit logistic regression
logit_model <- glm(SEVERE_BIN ~ ROAD_GEOMETRY + SPEED_ZONE + LIGHT_CONDITION +
                     SURFACE_COND + ATMOSPH_COND + ROAD_TYPE,
                   data = crash_data, family = binomial)

summary(logit_model)

# ---- Compute odds ratios with Wald confidence intervals ----
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

# ---- Filter to geometry terms only ----
geom_terms <- or_table %>%
  dplyr::filter(grepl("^ROAD_GEOMETRY", term)) %>%
  dplyr::mutate(term_clean = gsub("^ROAD_GEOMETRY", "", term),
                term_clean = ifelse(term_clean == "", "(ref)", term_clean))

# ---- Forest plot ----
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

# ---- Also print OR table for geometry ----
print(geom_terms %>% dplyr::select(term_clean, estimate, conf.low, conf.high, p.value))

# -----------------------------
# 8. Multinomial regression (optional, keep 4 categories) + effects plot
# -----------------------------
multi_model <- multinom(SEVERITY_CAT ~ ROAD_GEOMETRY + SPEED_ZONE + LIGHT_CONDITION +
                          SURFACE_COND + ATMOSPH_COND + ROAD_TYPE,
                        data = crash_data, trace = FALSE)

summary(multi_model)

# Predicted probabilities for severity by geometry
multi_eff <- Effect("ROAD_GEOMETRY", multi_model)

# The 'plot' method draws a multipanel plot of predicted probabilities
plot(multi_eff,
     main = "Predicted probability of crash severity by road geometry",
     xlab = "Road geometry", ylab = "Predicted probability")

# -----------------------------
# 9. Optional: Predicted probabilities from logistic model (by geometry)
# -----------------------------
# Create a reference grid holding other covariates at typical values
ref_row <- crash_data %>%
  summarise(
    SPEED_ZONE = median(SPEED_ZONE, na.rm = TRUE),
    LIGHT_CONDITION = levels(LIGHT_CONDITION)[1],
    SURFACE_COND = levels(SURFACE_COND)[1],
    ATMOSPH_COND = levels(factor(ATMOSPH_COND))[1],
    ROAD_TYPE = levels(factor(ROAD_TYPE))[1]
  )

newdat <- crash_data %>%
  distinct(ROAD_GEOMETRY) %>%
  mutate(SPEED_ZONE = ref_row$SPEED_ZONE,
         LIGHT_CONDITION = ref_row$LIGHT_CONDITION,
         SURFACE_COND = ref_row$SURFACE_COND,
         ATMOSPH_COND = ref_row$ATMOSPH_COND,
         ROAD_TYPE = ref_row$ROAD_TYPE)

newdat$pred <- predict(logit_model, newdata = newdat, type = "response")

p_pred <- ggplot(newdat, aes(x = ROAD_GEOMETRY, y = pred)) +
  geom_col(fill = "#377eb8") +
  labs(title = "Adjusted probability of severe crash by road geometry",
       x = "Road geometry", y = "Predicted probability (severe)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_pred)

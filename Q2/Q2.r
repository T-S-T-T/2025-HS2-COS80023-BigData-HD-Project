# Question: How do light condition and time of day affect crash severity?
# 0. Setup --------------------------------------------------------------------
# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(broom)
library(scales)
library(viridis)

# 1. Import data --------------------------------------------------------------
accident <- read.csv("../data/accident.csv", stringsAsFactors = FALSE)

# Quick peek
print(dim(accident))
print(head(accident))

# 2. Select relevant columns --------------------------------------------------
accident_sel <- accident %>%
  dplyr::select(ACCIDENT_NO, ACCIDENT_TIME, LIGHT_CONDITION, SEVERITY)

# Missingness bar plot
missing_df <- accident_sel %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing")

ggplot(missing_df, aes(x = variable, y = missing, fill = variable)) +
  geom_col() +
  labs(title = "Missing values in selected columns", y = "Count missing") +
  theme_minimal() +
  theme(legend.position = "none")

# 3. Parse time and create TIME_OF_DAY ----------------------------------------
accident_sel <- accident_sel %>%
  mutate(
    TIME = as.POSIXct(ACCIDENT_TIME, format = "%H:%M:%S", tz = "UTC"),
    HOUR = as.numeric(format(TIME, "%H")),
    TIME_OF_DAY = case_when(
      !is.na(HOUR) & HOUR >= 6  & HOUR < 12 ~ "Morning",
      !is.na(HOUR) & HOUR >= 12 & HOUR < 18 ~ "Afternoon",
      !is.na(HOUR) & HOUR >= 18 & HOUR < 22 ~ "Evening",
      !is.na(HOUR)                             ~ "Night",
      TRUE                                     ~ NA_character_
    )
  )

# Hourly histogram
ggplot(accident_sel, aes(x = HOUR)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", na.rm = TRUE) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Accident count by hour of day", x = "Hour", y = "Count") +
  theme_minimal()

# Count by TIME_OF_DAY
ggplot(accident_sel, aes(x = TIME_OF_DAY)) +
  geom_bar(fill = "tomato", na.rm = TRUE) +
  labs(title = "Accident count by time of day", y = "Count", x = "Time of Day")

# 4. Map LIGHT_CONDITION to readable categories --------------------------------
accident_sel <- accident_sel %>%
  mutate(
    LIGHT_CAT = case_when(
      LIGHT_CONDITION %in% c(1, "1") ~ "Daylight",
      LIGHT_CONDITION %in% c(2, "2") ~ "Dawn/Dusk",
      LIGHT_CONDITION %in% c(3, "3") ~ "Dark - Street lit",
      LIGHT_CONDITION %in% c(4, "4") ~ "Dark - No street lights",
      TRUE                           ~ "Other/Unknown"
    )
  )

# Lighting counts
ggplot(accident_sel, aes(x = LIGHT_CAT)) +
  geom_bar(fill = "darkgreen", na.rm = TRUE) +
  labs(title = "Accident count by lighting condition", x = "Lighting condition", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Recode severity and create binary severe flag -----------------------------
accident_sel <- accident_sel %>%
  filter(!is.na(SEVERITY)) %>%
  mutate(
    SEVERITY_CAT = factor(SEVERITY,
                          levels = c(1,2,3,4),
                          labels = c("Fatal","Serious Injury","Other Injury","Non Injury"),
                          ordered = TRUE),
    SEVERE_BIN = ifelse(SEVERITY %in% c(1,2), 1, 0)
  )

# Overall severe vs non-severe
ggplot(accident_sel, aes(x = factor(SEVERE_BIN), fill = factor(SEVERE_BIN))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "Non Severe", "1" = "Severe")) +
  labs(title = "Overall proportion severe vs non severe", x = "", y = "Count") +
  theme(legend.position = "none")

# 6. Ensure factors and drop unused levels -------------------------------------
accident_sel <- accident_sel %>%
  mutate(
    TIME_OF_DAY = factor(TIME_OF_DAY, levels = c("Morning","Afternoon","Evening","Night")),
    LIGHT_CAT   = factor(LIGHT_CAT)
  ) %>%
  droplevels()

print(levels(accident_sel$TIME_OF_DAY))
print(levels(accident_sel$LIGHT_CAT))

# 7. Exploratory visuals: severity by TIME_OF_DAY and LIGHTING -----------------
# Proportional stacked bar by TIME_OF_DAY
ggplot(accident_sel, aes(x = TIME_OF_DAY, fill = SEVERITY_CAT)) +
  geom_bar(position = "fill", na.rm = TRUE) +
  labs(y = "Proportion", title = "Crash Severity by Time of Day", x = "Time of Day", fill = "Severity") +
  scale_fill_brewer(palette = "Reds")

# Proportional stacked bar by LIGHT_CAT
ggplot(accident_sel, aes(x = LIGHT_CAT, fill = SEVERITY_CAT)) +
  geom_bar(position = "fill", na.rm = TRUE) +
  labs(y = "Proportion", title = "Crash Severity by Lighting Condition", x = "Lighting", fill = "Severity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Reds")

# Faceted proportion of severe vs non-severe by TIME_OF_DAY and LIGHT_CAT
ggplot(accident_sel, aes(x = factor(SEVERE_BIN), fill = factor(SEVERE_BIN))) +
  geom_bar(position = "fill", na.rm = TRUE) +
  facet_grid(LIGHT_CAT ~ TIME_OF_DAY) +
  scale_x_discrete(labels = c("0" = "Non Severe", "1" = "Severe")) +
  labs(y = "Proportion", title = "Proportion Severe by Lighting and Time of Day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# 8. Chi-square tests and tile (mosaic-like) plots ------------------------------
time_tab <- table(accident_sel$TIME_OF_DAY, accident_sel$SEVERITY_CAT)
light_tab <- table(accident_sel$LIGHT_CAT, accident_sel$SEVERITY_CAT)

time_chi <- chisq.test(time_tab)
light_chi <- chisq.test(light_tab)

print(time_chi)
print(light_chi)

# Tile plot for time x severity proportions
time_df <- as.data.frame(time_tab) %>%
  group_by(Var1) %>%
  mutate(prop = Freq / sum(Freq)) %>%
  ungroup()

ggplot(time_df, aes(x = Var1, y = Var2, fill = prop)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred", labels = percent) +
  labs(x = "Time of Day", y = "Severity", title = "Tile plot of Time of Day by Severity (proportion within time)") +
  theme_minimal()

# Tile plot for lighting x severity proportions
light_df <- as.data.frame(light_tab) %>%
  group_by(Var1) %>%
  mutate(prop = Freq / sum(Freq)) %>%
  ungroup()

ggplot(light_df, aes(x = Var1, y = Var2, fill = prop)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred", labels = percent) +
  labs(x = "Lighting", y = "Severity", title = "Tile plot of Lighting by Severity (proportion within lighting)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 9. Logistic regression (severe vs non-severe) --------------------------------
mod_data <- accident_sel %>%
  filter(!is.na(SEVERE_BIN), !is.na(TIME_OF_DAY), !is.na(LIGHT_CAT))

logit_model <- glm(SEVERE_BIN ~ TIME_OF_DAY + LIGHT_CAT, data = mod_data, family = binomial)
print(summary(logit_model))

# 10. Coefficient and odds ratio plots -----------------------------------------
coef_df <- tidy(logit_model, conf.int = TRUE, exponentiate = FALSE)
coef_df_no_int <- coef_df %>% filter(term != "(Intercept)")

ggplot(coef_df_no_int, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(y = "Log-odds coefficient", x = "Term", title = "Logistic regression coefficients with 95% CI")

coef_df_or <- tidy(logit_model, conf.int = TRUE, exponentiate = TRUE) %>% filter(term != "(Intercept)")

ggplot(coef_df_or, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  labs(y = "Odds Ratio", x = "Term", title = "Odds ratios from logistic regression with 95% CI")

# 11. Predicted probabilities by TIME_OF_DAY and LIGHT_CAT ----------------------
newdata <- expand.grid(
  TIME_OF_DAY = levels(mod_data$TIME_OF_DAY),
  LIGHT_CAT = levels(mod_data$LIGHT_CAT),
  stringsAsFactors = FALSE
)

pred <- predict(logit_model, newdata = newdata, type = "link", se.fit = TRUE)
newdata$pred_logit <- pred$fit
newdata$pred_se   <- pred$se.fit
newdata$pred_prob <- plogis(newdata$pred_logit)
newdata$lower     <- plogis(newdata$pred_logit - 1.96 * newdata$pred_se)
newdata$upper     <- plogis(newdata$pred_logit + 1.96 * newdata$pred_se)

# Heatmap of predicted probabilities
ggplot(newdata, aes(x = TIME_OF_DAY, y = LIGHT_CAT, fill = pred_prob)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Predicted\nprobability") +
  labs(title = "Predicted probability of severe crash by Time of Day and Lighting") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Grouped bar chart with error bars
ggplot(newdata, aes(x = TIME_OF_DAY, y = pred_prob, fill = LIGHT_CAT)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.8), width = 0.2) +
  labs(y = "Predicted probability", title = "Predicted severe crash probability by Time of Day and Lighting") +
  theme_minimal()

# 12. Model diagnostics --------------------------------------------------------
mod_data$fit <- fitted(logit_model)
mod_data$resid_deviance <- residuals(logit_model, type = "deviance")
mod_data$cooks <- cooks.distance(logit_model)

# Residuals vs fitted
ggplot(mod_data, aes(x = fit, y = resid_deviance)) +
  geom_point(alpha = 0.3) +
  labs(x = "Fitted probability", y = "Deviance residual", title = "Residuals vs Fitted")

# Cook's distance plot
ggplot(mod_data, aes(x = seq_along(cooks), y = cooks)) +
  geom_col() +
  labs(x = "Observation index", y = "Cook's distance", title = "Cook's distance for observations")

# 13. Odds ratios table (numeric) ----------------------------------------------
est <- coef(summary(logit_model))
or  <- exp(coef(logit_model))
ci_low  <- exp(coef(logit_model) - 1.96 * est[, "Std. Error"])
ci_high <- exp(coef(logit_model) + 1.96 * est[, "Std. Error"])
or_table <- data.frame(OR = or, CI_low = ci_low, CI_high = ci_high)
print(or_table)

# End -------------------------------------------------------------------------
message("Script complete. Review plots and model outputs to interpret how light condition and time of day affect crash severity.")

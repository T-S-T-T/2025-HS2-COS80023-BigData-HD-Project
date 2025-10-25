# Question: How do light condition and time of day affect crash severity?

# 1. Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# 2. Import accident table
accident <- read.csv("../data/accident.csv")

# 3. Select only the relevant columns
accident_sel <- accident %>%
  dplyr::select(ACCIDENT_NO, ACCIDENT_TIME, LIGHT_CONDITION, SEVERITY)

# 4. Parse time and create TIME_OF_DAY categories
# ACCIDENT_TIME is in "HH:MM:SS" format
accident_sel <- accident_sel %>%
  mutate(
    TIME = as.POSIXct(ACCIDENT_TIME, format = "%H:%M:%S"),
    HOUR = as.numeric(format(TIME, "%H")),
    TIME_OF_DAY = case_when(
      HOUR >= 6  & HOUR < 12 ~ "Morning",
      HOUR >= 12 & HOUR < 18 ~ "Afternoon",
      HOUR >= 18 & HOUR < 22 ~ "Evening",
      TRUE                   ~ "Night"
    )
  )


# 5. Map LIGHT_CONDITION codes into readable categories
# Adjust mapping if your dataset uses different codes
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

# 6. Recode severity into ordered factor and binary severe flag
accident_sel <- accident_sel %>%
  filter(!is.na(SEVERITY)) %>%
  mutate(
    SEVERITY_CAT = factor(SEVERITY,
                          levels = c(1,2,3,4),
                          labels = c("Fatal","Serious Injury","Other Injury","Non Injury"),
                          ordered = TRUE),
    SEVERE_BIN = ifelse(SEVERITY %in% c(1,2), 1, 0)
  )

# 7. Convert predictors to factors
accident_sel <- accident_sel %>%
  mutate(
    TIME_OF_DAY = factor(TIME_OF_DAY, levels = c("Morning","Afternoon","Evening","Night")),
    LIGHT_CAT   = factor(LIGHT_CAT)
  ) %>%
  droplevels()

# 8. Exploratory analysis
# Proportions
prop.table(table(accident_sel$TIME_OF_DAY, accident_sel$SEVERITY_CAT), margin = 1)
prop.table(table(accident_sel$LIGHT_CAT, accident_sel$SEVERITY_CAT), margin = 1)

# Plots
ggplot(accident_sel, aes(x = TIME_OF_DAY, fill = SEVERITY_CAT)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Crash Severity by Time of Day")

ggplot(accident_sel, aes(x = LIGHT_CAT, fill = SEVERITY_CAT)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Crash Severity by Lighting Condition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 9. Chi-square tests
chisq.test(table(accident_sel$TIME_OF_DAY, accident_sel$SEVERITY_CAT))
chisq.test(table(accident_sel$LIGHT_CAT, accident_sel$SEVERITY_CAT))

# 10. Logistic regression (binary severe vs non-severe)
mod_data <- accident_sel %>%
  filter(!is.na(SEVERE_BIN), !is.na(TIME_OF_DAY), !is.na(LIGHT_CAT))

logit_model <- glm(SEVERE_BIN ~ TIME_OF_DAY + LIGHT_CAT,
                   data = mod_data, family = binomial)

summary(logit_model)

# 11. Odds ratios and confidence intervals
est <- coef(summary(logit_model))
or  <- exp(coef(logit_model))
ci_low  <- exp(coef(logit_model) - 1.96 * est[, "Std. Error"])
ci_high <- exp(coef(logit_model) + 1.96 * est[, "Std. Error"])
data.frame(OR = or, CI_low = ci_low, CI_high = ci_high)

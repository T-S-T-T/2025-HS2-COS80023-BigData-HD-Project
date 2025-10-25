# Question 3: How does seatbelt or helmet usage affect hospitalization rates?

# 1. Load libraries
library(dplyr)
library(ggplot2)

# 2. Import Person table
person <- read.csv("../data/person.csv")

# 3. Select relevant columns
person_sel <- person %>%
  dplyr::select(PERSON_ID, HELMET_BELT_WORN, HOSPITALISED, AGE, SEX)

# 4. Recode protective equipment usage
# Adjust mapping if your data dictionary uses different codes
person_sel <- person_sel %>%
  mutate(
    PROTECTION = case_when(
      HELMET_BELT_WORN %in% c(1, "1") ~ "Seatbelt worn",
      HELMET_BELT_WORN %in% c(2, "2") ~ "Helmet worn",
      HELMET_BELT_WORN %in% c(3, "3") ~ "None",
      TRUE                            ~ "Unknown"
    ),
    HOSP_BIN = ifelse(HOSPITALISED %in% c(1, "1"), 1, 0)  # 1 = hospitalised
  )

# 5. Convert to factors and drop unused levels
person_sel <- person_sel %>%
  mutate(
    PROTECTION = factor(PROTECTION),
    SEX = factor(SEX)
  ) %>%
  droplevels()

# 6. Exploratory analysis
# Hospitalisation proportions by protection type
prop.table(table(person_sel$PROTECTION, person_sel$HOSP_BIN), margin = 1)

# Plot proportions
ggplot(person_sel, aes(x = PROTECTION, fill = factor(HOSP_BIN))) +
  geom_bar(position = "fill") +
  labs(x = "Protection", y = "Proportion", fill = "Hospitalised",
       title = "Hospitalisation by Seatbelt/Helmet Usage") +
  theme_minimal()

# 7. Chi-square test
chisq.test(table(person_sel$PROTECTION, person_sel$HOSP_BIN))

# 8. Logistic regression
# Model hospitalisation odds as a function of protection, adjusting for age and sex
logit_model <- glm(HOSP_BIN ~ PROTECTION + AGE + SEX,
                   data = person_sel, family = binomial)

summary(logit_model)

# 9. Odds ratios and 95% confidence intervals
est <- coef(summary(logit_model))
or  <- exp(coef(logit_model))
ci_low  <- exp(coef(logit_model) - 1.96 * est[, "Std. Error"])
ci_high <- exp(coef(logit_model) + 1.96 * est[, "Std. Error"])
or_table <- data.frame(OR = or, CI_low = ci_low, CI_high = ci_high)
print(or_table)

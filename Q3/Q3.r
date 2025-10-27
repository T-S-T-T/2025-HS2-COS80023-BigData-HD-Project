# ============================================================
# Analysis: Effect of seatbelt/helmet usage on hospitalisation
# ============================================================

# 1. Load libraries
library(dplyr)
library(ggplot2)

# 2. Import Person table
person <- read.csv("../data/person.csv")

# 3. Select relevant columns
person_sel <- person %>%
  select(PERSON_ID, HELMET_BELT_WORN, INJ_LEVEL, TAKEN_HOSPITAL, AGE_GROUP, SEX)

# 4. Recode protection variable
person_sel <- person_sel %>%
  mutate(
    PROTECTION = case_when(
      HELMET_BELT_WORN %in% c(1, "1") ~ "Seatbelt worn",
      HELMET_BELT_WORN %in% c(2, "2") ~ "Seatbelt not worn",
      HELMET_BELT_WORN %in% c(3, "3") ~ "Child restraint worn",
      HELMET_BELT_WORN %in% c(4, "4") ~ "Child restraint not worn",
      HELMET_BELT_WORN %in% c(5, "5") ~ "No restraint fitted",
      HELMET_BELT_WORN %in% c(6, "6") ~ "Helmet worn",
      HELMET_BELT_WORN %in% c(7, "7") ~ "Helmet not worn",
      HELMET_BELT_WORN %in% c(8, "8") ~ "Not appropriate",
      HELMET_BELT_WORN %in% c(9, "9") ~ "Not known",
      TRUE                            ~ "Unknown"
    ),
    # Define hospitalisation outcome
    HOSP_BIN = case_when(
      TAKEN_HOSPITAL == "Y" ~ 1,
      INJ_LEVEL %in% c(1,2) ~ 1,   # Fatality or serious injury
      TRUE                  ~ 0
    )
  )

# 5. Convert to factors
person_sel <- person_sel %>%
  mutate(
    PROTECTION = factor(PROTECTION),
    SEX = factor(SEX),
    HOSP_BIN = factor(HOSP_BIN, levels = c(0,1), labels = c("No","Yes"))
  )

# ============================================================
# Exploratory analysis
# ============================================================

# 6. Proportions by protection type
prop.table(table(person_sel$PROTECTION, person_sel$HOSP_BIN), margin = 1)

# 7. Visual: stacked bar chart
ggplot(person_sel, aes(x = PROTECTION, fill = HOSP_BIN)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("No" = "#4CAF50", "Yes" = "#F44336")) +
  labs(
    x = "Protection",
    y = "Hospitalised proportion",
    fill = "Hospitalised",
    title = "Hospitalisation by seatbelt/helmet usage"
  ) +
  theme_minimal(base_size = 13) +
  coord_flip()   # flip for readability if many categories

# ============================================================
# Chi-square test
# ============================================================

tab_prot_hosp <- table(person_sel$PROTECTION, person_sel$HOSP_BIN)
chi_res <- chisq.test(tab_prot_hosp)
print(chi_res)

# ============================================================
# Logistic regression
# ============================================================

# Convert HOSP_BIN back to numeric for glm
person_sel_glm <- person_sel %>%
  mutate(HOSP_BIN_NUM = as.integer(HOSP_BIN) - 1)

logit_model <- glm(HOSP_BIN_NUM ~ PROTECTION + AGE_GROUP + SEX,
                   data = person_sel_glm, family = binomial)

summary(logit_model)

# ============================================================
# Odds ratios and forest plot
# ============================================================

est <- coef(summary(logit_model))
coefs <- coef(logit_model)

or       <- exp(coefs)
ci_low   <- exp(coefs - 1.96 * est[, "Std. Error"])
ci_high  <- exp(coefs + 1.96 * est[, "Std. Error"])
term     <- names(or)

or_table <- data.frame(
  Term   = term,
  OR     = or,
  CI_low = ci_low,
  CI_high= ci_high,
  row.names = NULL
)
print(or_table)

# Forest plot
forest_df <- or_table %>%
  filter(Term != "(Intercept)") %>%
  mutate(
    Term_label = Term,
    Term_label = gsub("^PROTECTION", "Protection: ", Term_label),
    Term_label = gsub("^SEX", "Sex: ", Term_label),
    Term_label = gsub("^AGE_GROUP", "Age group: ", Term_label)
  )

ggplot(forest_df, aes(y = Term_label, x = OR)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 2, color = "#1f77b4") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2, color = "#1f77b4") +
  scale_x_log10() +
  labs(
    title = "Adjusted odds ratios for hospitalisation",
    x = "Odds ratio (log scale)",
    y = ""
  ) +
  theme_minimal(base_size = 13)

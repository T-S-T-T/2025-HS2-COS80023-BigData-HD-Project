# -------------------------------------------------
# Research Question:
# Does seatbelt/helmet usage affect hospitalisation?
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
person <- read.csv("../data/person.csv")

# -----------------------------
# 2. Select relevant columns
# -----------------------------
person_data <- person %>%
  dplyr::select(PERSON_ID, HELMET_BELT_WORN, INJ_LEVEL, TAKEN_HOSPITAL)

# -----------------------------
# 3. Clean, recode protection and outcomes
# -----------------------------
person_data <- person %>%
  mutate(PROTECTION = case_when(
    HELMET_BELT_WORN %in% c(1, "1", 3, "3", 6, "6") ~ "Protection worn",        # seatbelt worn, child restraint worn, helmet worn
    HELMET_BELT_WORN %in% c(2, "2", 4, "4", 5, "5", 7, "7") ~ "Protection not worn", # seatbelt not worn, child restraint not worn, no restraint, helmet not worn
    HELMET_BELT_WORN %in% c(8, "8", 9, "9") ~ "Unknown/Not appropriate",        # not appropriate, not known
    TRUE ~ "Unknown"
  )) %>%
  filter(PROTECTION != "Unknown") %>%
  droplevels()

# Create ordered severity categories
person_data <- person_data %>%
  mutate(SEVERITY_CAT = factor(INJ_LEVEL,
                               levels = c(1, 2, 3, 4),
                               labels = c("Fatal", "Serious Injury", "Other Injury", "Non Injury"),
                               ordered = TRUE)) %>%
  mutate(SEVERITY_CAT = droplevels(SEVERITY_CAT))

# >>> Print counts of cases by protection and severity <<<
cat("\nCounts by protection and severity:\n")
print(table(person_data$PROTECTION, person_data$SEVERITY_CAT))

# Create binary hospitalisation variable
person_data <- person_data %>%
  mutate(HOSP_BIN = ifelse(TAKEN_HOSPITAL == "Y" | INJ_LEVEL %in% c(1,2), 1, 0))

# Set reference for protection
person_data <- person_data %>%
  mutate(PROTECTION = fct_relevel(PROTECTION, "Protection worn"))

# -----------------------------
# 4. Exploratory visualizations
# -----------------------------
p_counts <- ggplot(person_data, aes(x = PROTECTION, fill = SEVERITY_CAT)) +
  geom_bar() +
  labs(y = "Count", x = "Protection",
       title = "Crash severity counts by protection type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_props <- ggplot(person_data, aes(x = PROTECTION, fill = SEVERITY_CAT)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", x = "Protection",
       title = "Crash severity proportions by protection type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(p_counts, p_props, ncol = 2)

# -----------------------------
# 5. Chi-square test
# -----------------------------
prot_sev_tab <- table(person_data$PROTECTION, person_data$SEVERITY_CAT)
chi_out <- chisq.test(prot_sev_tab)
print(chi_out)

# -----------------------------
# 6. Logistic regression (binary hospitalised vs not) + forest plot
# -----------------------------
logit_model <- glm(HOSP_BIN ~ PROTECTION,
                   data = person_data, family = binomial)

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

prot_terms <- or_table %>%
  dplyr::filter(grepl("^PROTECTION", term)) %>%
  dplyr::mutate(term_clean = gsub("^PROTECTION", "", term),
                term_clean = ifelse(term_clean == "", "(ref)", term_clean))

p_forest <- ggplot(prot_terms,
                   aes(x = reorder(term_clean, estimate),
                       y = estimate,
                       ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_pointrange(size = 0.4) +
  coord_flip() +
  labs(title = "Adjusted odds ratios: hospitalisation by protection type",
       x = "Protection (reference: Protection worn)",
       y = "Odds ratio (with 95% Wald CI)") +
  theme_minimal()

print(p_forest)
print(prot_terms %>% dplyr::select(term_clean, estimate, conf.low, conf.high, p.value))

# -----------------------------
# 7. Multinomial regression (4 severity categories) + bar chart of predicted probabilities
# -----------------------------
multi_model <- multinom(SEVERITY_CAT ~ PROTECTION,
                        data = person_data, trace = FALSE)

summary(multi_model)

# Get predicted probabilities for each observation
pred_probs <- as.data.frame(predict(multi_model, type = "probs"))
pred_probs$PROTECTION <- person_data$PROTECTION

# Average predicted probabilities by protection
pred_summary <- pred_probs %>%
  group_by(PROTECTION) %>%
  summarise(across(everything(), mean)) %>%
  tidyr::pivot_longer(-PROTECTION,
                      names_to = "Severity",
                      values_to = "Predicted_Prob")

# Convert to percentages
pred_summary <- pred_summary %>%
  mutate(Percent = Predicted_Prob * 100)

# Bar chart with percentage labels
p_pred_bar <- ggplot(pred_summary,
                     aes(x = PROTECTION, y = Percent, fill = Severity)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f%%", Percent)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  labs(title = "Predicted probability of crash severity by protection type",
       x = "Protection", y = "Predicted probability (%)") +
  theme_minimal()

print(p_pred_bar)

# Question: Does road geometry (intersections, curves, straight roads) affect crash severity?

# Load libraries
library(dplyr)
library(ggplot2)
library(nnet)   # for multinomial regression if you want 4 categories

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
# 4. Clean severity variable
# -----------------------------
# Original coding: 1=Fatal, 2=Serious injury, 3=Other injury, 4=Non injury
crash_data <- crash_data %>%
  filter(!is.na(ROAD_GEOMETRY), !is.na(SEVERITY)) %>%
  mutate(SEVERITY_CAT = factor(SEVERITY,
                               levels = c(1, 2, 3, 4),
                               labels = c("Fatal", "Serious Injury", "Other Injury", "Non Injury"),
                               ordered = TRUE))

# Also create a binary severe vs non-severe variable
crash_data <- crash_data %>%
  mutate(SEVERE_BIN = ifelse(SEVERITY %in% c(1, 2), 1, 0))

# -----------------------------
# 5. Exploratory Data Analysis
# -----------------------------
# Frequency table
table(crash_data$ROAD_GEOMETRY, crash_data$SEVERITY_CAT)

# Proportions by geometry
prop.table(table(crash_data$ROAD_GEOMETRY, crash_data$SEVERITY_CAT), margin = 1)

# Plot severity distribution by geometry
ggplot(crash_data, aes(x = ROAD_GEOMETRY, fill = SEVERITY_CAT)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Crash Severity by Road Geometry") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------
# 6. Chi-square test
# -----------------------------
geom_sev_tab <- table(crash_data$ROAD_GEOMETRY, crash_data$SEVERITY_CAT)
chisq.test(geom_sev_tab)

# -----------------------------
# 7. Logistic regression (binary severe vs non-severe)
# -----------------------------
logit_model <- glm(SEVERE_BIN ~ ROAD_GEOMETRY + SPEED_ZONE + LIGHT_CONDITION +
                     SURFACE_COND + ATMOSPH_COND + ROAD_TYPE,
                   data = crash_data, family = binomial)

summary(logit_model)

# -----------------------------
# 8. Multinomial regression (optional, keep 4 categories)
# -----------------------------
multi_model <- multinom(SEVERITY_CAT ~ ROAD_GEOMETRY + SPEED_ZONE + LIGHT_CONDITION +
                          SURFACE_COND + ATMOSPH_COND + ROAD_TYPE,
                        data = crash_data)

summary(multi_model)

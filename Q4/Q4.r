# Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(caret)        # modeling & CV
library(glmnet)       # regularized logistic/multinomial
library(recipes)      # preprocessing pipelines
library(rsample)      # stratified splits
library(yardstick)    # metrics
# For tree/boosting you might add: ranger, xgboost, lightgbm (optional)

# 1) Load data (adjust paths)
accident   <- read.csv("../data/accident.csv")
surface    <- read.csv("../data/road_surface_cond.csv")
weather    <- read.csv("../data/atmospheric_cond.csv")
location   <- read.csv("../data/accident_Location.csv")
node       <- read.csv("../data/node.csv")
vehicle    <- read.csv("../data/vehicle.csv")
person     <- read.csv("../data/person.csv")
dca        <- read.csv("../data/sub_dca.csv")

# 2) Target setup (binary example)
acc <- accident %>%
  mutate(
    SEVERITY_CAT = factor(SEVERITY, levels = c(1,2,3,4),
                          labels = c("Fatal","Serious","Other","None"),
                          ordered = TRUE),
    SEVERE_BIN = ifelse(SEVERITY %in% c(1,2), 1, 0)
  ) %>%
  filter(!is.na(SEVERITY))

# 3) Temporal features
acc <- acc %>%
  mutate(
    # Parse HH:MM:SS into a time object
    TIME = hms(ACCIDENT_TIME),   # lubridate::hms handles "15:30:00"
    HOUR = hour(TIME),
    TIME_OF_DAY = case_when(
      HOUR >= 6  & HOUR < 12 ~ "Morning",
      HOUR >= 12 & HOUR < 18 ~ "Afternoon",
      HOUR >= 18 & HOUR < 22 ~ "Evening",
      !is.na(HOUR)           ~ "Night",
      TRUE                   ~ NA_character_
    ),
    WEEKEND = ifelse(DAY_OF_WEEK %in% c("Saturday","Sunday"), 1, 0)
  )

# 4) Collapse weather/surface to accident-level
weather_acc <- weather %>%
  group_by(ACCIDENT_NO) %>%
  summarise(WEATHER_CAT = paste(unique(ATMOSPH_COND), collapse = ", "), .groups = "drop")

surface_acc <- surface %>%
  group_by(ACCIDENT_NO) %>%
  summarise(SURFACE_CAT = paste(unique(SURFACE_COND), collapse = ", "), .groups = "drop")

# 5) Location, node (road type, LGA/postcode)
loc_acc <- location %>% select(ACCIDENT_NO, ROAD_TYPE)
node_acc <- node %>% select(ACCIDENT_NO, LGA_NAME, POSTCODE_CRASH)

# 6) Vehicle aggregates
veh_agg <- vehicle %>%
  group_by(ACCIDENT_NO) %>%
  summarise(
    N_VEH = n(),
    PCT_MOTORCYCLE = mean(VEHICLE_TYPE_DESC %in% c("Motorcycle"), na.rm = TRUE),
    PCT_HEAVY = mean(VEHICLE_TYPE_DESC %in% c("Truck","Bus"), na.rm = TRUE),
    MAX_DAMAGE = suppressWarnings(max(as.numeric(LEVEL_OF_DAMAGE), na.rm = TRUE)),
    .groups = "drop"
  )

# 7) Person aggregates
# Person-level aggregates (sex, restraint, serious injury)
per_basic <- person %>%
  group_by(ACCIDENT_NO) %>%
  summarise(
    PCT_MALE = mean(SEX %in% c("M", "Male", 1), na.rm = TRUE),
    PCT_RESTRAINED = mean(HELMET_BELT_WORN %in% c(1, "Seatbelt worn", "Helmet worn"), na.rm = TRUE),
    N_SERIOUS_INJ = sum(INJ_LEVEL %in% c("Serious","Severe", 2), na.rm = TRUE),
    .groups = "drop"
  )

# Age group proportions
per_age <- person %>%
  group_by(ACCIDENT_NO, AGE_GROUP) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(ACCIDENT_NO) %>%
  mutate(PROP = N / sum(N)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = AGE_GROUP,
    values_from = PROP,
    values_fill = 0,
    names_prefix = "PROP_AGE_"
  )

# Final accident-level person features
per_agg <- per_basic %>%
  left_join(per_age, by = "ACCIDENT_NO")

# 8) DCA features (categorical mechanism)
dca_acc <- dca %>% select(ACCIDENT_NO, SUB_DCA_CODE)

# 9) Join all accident-level features
dat <- acc %>%
  select(ACCIDENT_NO, SEVERITY_CAT, SEVERE_BIN, SPEED_ZONE, LIGHT_CONDITION,
         ROAD_GEOMETRY, NO_OF_VEHICLES, ACCIDENT_DATE, DAY_OF_WEEK, TIME_OF_DAY) %>%
  left_join(weather_acc, by = "ACCIDENT_NO") %>%
  left_join(surface_acc, by = "ACCIDENT_NO") %>%
  left_join(loc_acc,     by = "ACCIDENT_NO") %>%
  left_join(node_acc,    by = "ACCIDENT_NO") %>%
  left_join(veh_agg,     by = "ACCIDENT_NO") %>%
  left_join(per_agg,     by = "ACCIDENT_NO") %>%
  left_join(dca_acc,     by = "ACCIDENT_NO")

# 10) Feature cleaning and bins
dat <- dat %>%
  mutate(
    SPEED_BIN = case_when(
      SPEED_ZONE <= 50 ~ "≤50",
      SPEED_ZONE >= 60 & SPEED_ZONE <= 70 ~ "60–70",
      SPEED_ZONE >= 80 & SPEED_ZONE <= 90 ~ "80–90",
      SPEED_ZONE >= 100 ~ "≥100",
      TRUE ~ NA_character_
    ),
    LIGHT_CAT = case_when(
      LIGHT_CONDITION %in% c(1, "1", "Daylight") ~ "Daylight",
      LIGHT_CONDITION %in% c(2, "2", "Dawn/Dusk") ~ "Dawn/Dusk",
      LIGHT_CONDITION %in% c(3, "3") ~ "Dark Lit",
      LIGHT_CONDITION %in% c(4, "4") ~ "Dark Unlit",
      TRUE ~ "Other/Unknown"
    )
  )

# 11) Preprocessing recipe (binary example)
rec <- recipe(SEVERE_BIN ~ SPEED_BIN + LIGHT_CAT + ROAD_GEOMETRY + ROAD_TYPE +
                WEATHER_CAT + SURFACE_CAT + TIME_OF_DAY + WEEKEND +
                PCT_MOTORCYCLE + PCT_HEAVY + N_VEH + MAX_DAMAGE +
                MEAN_DRIVER_AGE + PCT_MALE + PCT_RESTRAINED + LGA + DCA_CODE,
              data = dat) %>%
  update_role(ACCIDENT_NO, new_role = "id") %>%
  step_string2factor(all_nominal(), -all_outcomes()) %>%
  step_unknown(all_nominal(), new_level = "Unknown") %>%
  step_other(all_nominal(), threshold = 0.01) %>%              # group rare categories
  step_dummy(all_nominal_predictors()) %>%                      # one-hot
  step_impute_mean(all_numeric_predictors()) %>%
  step_zv(all_predictors()) %>%                                # remove zero-variance
  step_normalize(all_numeric_predictors())                     # scale numeric

# 12) Train/validation/test split (stratified)
set.seed(42)
split <- initial_split(dat, prop = 0.8, strata = SEVERE_BIN)
train <- training(split)
test  <- testing(split)

cv_folds <- vfold_cv(train, v = 5, strata = SEVERE_BIN)

# 13) Baseline model: regularized logistic (glmnet)
glmnet_spec <- caret::trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Prepare processed data via recipe
prep_rec <- prep(rec, training = train)
train_bake <- bake(prep_rec, new_data = train)
test_bake  <- bake(prep_rec, new_data = test)

# Convert target to factor with labels for caret
train_bake$SEVERE_BIN <- factor(ifelse(train_bake$SEVERE_BIN == 1, "Severe", "NonSevere"),
                                levels = c("NonSevere","Severe"))

set.seed(42)
glmnet_fit <- caret::train(
  SEVERE_BIN ~ .,
  data = train_bake %>% select(-ACCIDENT_NO),
  method = "glmnet",
  metric = "ROC",
  trControl = glmnet_spec,
  tuneLength = 10
)

# 14) Evaluation on test
pred <- predict(glmnet_fit, newdata = test_bake %>% select(-ACCIDENT_NO), type = "prob")
roc_auc <- yardstick::roc_auc(
  bind_cols(test_bake["SEVERE_BIN"],
            tibble(.pred_NonSevere = pred$NonSevere, .pred_Severe = pred$Severe)),
  truth = SEVERE_BIN, .pred_Severe
)
print(roc_auc)

# 15) Global importance (linear model)
# Coefficients from best glmnet model
best <- glmnet_fit$finalModel
best_lambda <- glmnet_fit$bestTune$lambda
coef_mat <- as.matrix(coef(best, s = best_lambda))
imp <- tibble(feature = rownames(coef_mat), coef = as.numeric(coef_mat)) %>%
  arrange(desc(abs(coef)))
print(head(imp, 20))

# 16) Nonlinear model (optional): random forest
# library(ranger)
# rf_fit <- ranger(SEVERE_BIN ~ ., data = train_bake %>% select(-ACCIDENT_NO),
#                  probability = TRUE, num.trees = 500, class.weights = c(NonSevere = 1, Severe = 5))
# rf_pred <- predict(rf_fit, data = test_bake %>% select(-ACCIDENT_NO))$predictions[, "Severe"]
# yardstick::roc_auc(tibble(SEVERE_BIN = test_bake$SEVERE_BIN, .pred_Severe = rf_pred),
#                    truth = SEVERE_BIN, .pred_Severe)

# 17) Calibration (optional)
# yardstick::brier_class(bind_cols(test_bake["SEVERE_BIN"], tibble(.pred_Severe = pred$Severe)),
#                        truth = SEVERE_BIN, .pred_Severe)

# 18) SHAP / permutation importance (optional)
# For tree/boosting models, use iml or fastshap packages to compute SHAP; or caret's varImp/permutation.

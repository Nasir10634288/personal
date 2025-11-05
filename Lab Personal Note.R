
###### Lab 1 ###########################################################################


# 1. Data Setup & Packages

# Install and load required libraries
if (!"librarian" %in% rownames(installed.packages())) install.packages("librarian")
librarian::shelf(tidyverse, Lahman, magrittr, ggplot2, skimr, dplyr, gt, gtExtras, here, readr)

# Set a consistent plot theme
theme_set(theme_bw(base_size = 18) + theme(legend.position = "top"))

# WHEN TO USE:
# Use this block at the very beginning of your script or exam answer file.
# It ensures all necessary libraries are installed and loaded, and sets the look of your plots.
# It avoids repeated install() during exam time.

# 2. Filter + Select + Summarize Example

mets <- Teams %>%
  filter(teamID == "NYN", yearID >= 2004, yearID <= 2012) %>%
  select(yearID, W, L) %>%
  summarise(total_wins = sum(W), total_losses = sum(L))

# WHEN TO USE:
# Use this when the question asks for total wins/losses (or any numeric summary)
# for a specific subset (e.g., team, year range).
# Great for questions like: "How did the Mets perform between 2004 and 2012?"

# 3. Mutate to Create Expected Win % using RS and RA

mets_ben <- Teams %>%
  filter(teamID == "NYN", yearID %in% 2004:2012) %>%
  select(yearID, teamID, W, L, R, RA) %>%
  rename(RS = R) %>%
  mutate(
    WPct = 1 / (1 + (RA / RS)^2),
    W_hat = WPct * (W + L)
  )

# WHEN TO USE:
# When you're asked to calculate expected wins or winning percentage using
# the Pythagorean formula based on runs scored (RS) and allowed (RA).

# 4. Compare Actual vs Expected Wins (Over/Under Performance)

mets_ben <- mets_ben %>%
  mutate(diff_W = W - W_hat)

mets_ben %>%
  summarise(
    better = sum(diff_W > 0),
    worse = sum(diff_W < 0)
  )

# WHEN TO USE:
# If a question asks how many seasons a team over-performed or under-performed
# compared to the expected model based on RS and RA.

# 5. Sort or Rank Based on Performance Residual

mets_rank <- mets_ben %>%
  mutate(
    diff_W = W - W_hat,
    rank_by_diff = min_rank(desc(diff_W))
  ) %>%
  arrange(rank_by_diff)

# WHEN TO USE:
# To list best/worst seasons according to performance vs expectation.
# Or when you're asked: "Which season had the biggest overperformance?"

# 6. Summarize Performance by General Manager (Group)

mets_ben <- mets_ben %>%
  mutate(GM = case_when(
    yearID == 2004 ~ "Jim Duquette",
    yearID >= 2005 & yearID <= 2010 ~ "Omar Minaya",
    yearID >= 2011 & yearID <= 2012 ~ "Sandy Alderson"
  ))

mets_ben %>%
  group_by(GM) %>%
  summarise(
    seasons = n(),
    total_WPct = sum(W) / (sum(W) + sum(L)),
    avg_residual = mean(W - W_hat)
  )

# WHEN TO USE:
# To compare group-level summaries (e.g., different managers or categories).
# Works for any grouped analysis — e.g., customer segments, product lines.

# 7. Tidy Data: Pivot, Separate, and Reshape Columns

df %>%
  pivot_longer(
    starts_with("20"),
    names_to = "date",
    values_to = "quantity"
  ) %>%
  separate_wider_delim(cols = date, delim = "_",
                       names = c("date", "paymentMandate")) %>%
  pivot_wider(names_from = paymentMandate, values_from = quantity) %>%
  mutate(across(contains("date"), as.Date))

# WHEN TO USE:
# When columns contain multiple variables (e.g., "2014-01-01_active"),
# and you need to separate, restructure, or normalize them into tidy format.

# 8. Cross-tab and Proportions for Loyalty & Discounts

# Proportion of customers by discount status
table(data$discount) / length(data$discount)

# Proportion loyal vs not loyal
table(data$is_loyal) / length(data$is_loyal)

# Cross-tab of discount vs loyalty
xtabs(~ discount + is_loyal, data = data)

# WHEN TO USE:
# If you're asked how many customers are loyal vs not, or who got discounts.
# xtabs shows the overlap — e.g., how many loyal customers got no discount.

# 9. Filter and View Top Non-Discounted Customers

data %>%
  mutate(id = row_number(), .before = 1) %>%
  filter(discount == 0) %>%
  arrange(desc(sales)) %>%
  slice_head(n = 10) %>%
  gt() %>%
  tab_header(title = "Top 10 Non-Discount Customers") %>%
  gtExtras::gt_theme_espn()

# WHEN TO USE:
# When asked: "List top customers by sales", especially for a segment (e.g., non-discounted).
# Filtering + sorting + displaying with gt() makes it exam-presentable.

# 10. Display Tables with GT

data %>%
  slice_head(n = 5) %>%
  gt() %>%
  tab_header(title = "Sample Marketing Data") %>%
  gtExtras::gt_theme_espn()

# WHEN TO USE:
# Use GT to cleanly present table outputs during your exam.
# Especially helpful when you need to "show results nicely".

# Exam Tips (Reminder)

# 1. Always start with filter() + select() before mutate() or summarise()
# 2. Use mutate() to calculate new columns like expected wins
# 3. Use pivot_longer + separate_wider_delim + pivot_wider for messy wide data
# 4. xtabs() is perfect for comparing 2 categories (loyalty vs discount)
# 5. gt() is great for clean exam output
# 6. Don’t forget group_by() + summarise() for comparisons by GM, segment, etc.

# End of Cheat Sheet

###### Lab 2 ###########################################################################

if (!"librarian" %in% rownames(installed.packages())) install.packages("librarian")
librarian::shelf(tidyverse, janitor, skimr, recipes, ggplot2, gt, gtExtras, parsnip, lubridate, forcats)

# 1️⃣ LOAD + CLEAN DATA
# 👉 When to use: Any time you need to load, inspect, or clean raw data.

df <- readr::read_csv("data/filename.csv", show_col_types = FALSE) |>
  janitor::clean_names()
skimr::skim(df)


# 2️⃣ PIVOT / RESHAPE DATA
# 👉 When to use: When asked to reshape long data (rows per ingredient)
#     into wide format (one row per item, one column per variable).

cocktails_df <- boston_cocktails |>
  select(-ingredient_number, -row_id, -measure) |>
  pivot_wider(
    names_from = ingredient,
    values_from = measure_number,
    values_fill = 0
  ) |>
  janitor::clean_names() |>
  drop_na()


# 3️⃣ PCA RECIPE
# 👉 When to use: To reduce many numeric predictors into fewer
#     uncorrelated components (PC1, PC2, etc.).

pca_rec <- recipe(~ ., data = cocktails_df) |>
  update_role(name, category, new_role = "id") |>
  step_normalize(all_predictors()) |>
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

# Variance explained
tidy(pca_prep, number = 2, type = "variance")
# Loadings
tidy(pca_prep, number = 2)


# 4️⃣ VISUALIZE PCA LOADINGS
# 👉 When to use: To interpret which ingredients define each component.

bar <- tidy(pca_prep, number = 2)
bar |>
  filter(component %in% paste0("PC", 1:5)) |>
  mutate(component = forcats::fct_inorder(component)) |>
  ggplot(aes(x = value, y = terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  theme_minimal()


# 5️⃣ VIEW NORMALIZATION STATS
# 👉 When to use: To check mean/SD of numeric variables before PCA or scaling.

tidy(pca_prep, 1) |>
  filter(statistic == "mean") |>
  slice_max(n = 1, order_by = value)


# 6️⃣ TIME SERIES – LAG 1 MONTH
# 👉 When to use: For forecasting tasks where current month depends
#     on last month’s value.

df_rec <- df |>
  recipe(sales_volume ~ .) |>
  update_role(region_name, new_role = "id") |>
  step_lag(sales_volume, lag = 1) |>
  step_naomit(lag_1_sales_volume, skip = FALSE) |>
  step_arrange(region_name, date)

baked <- df_rec |>
  prep() |>
  bake(new_data = NULL)


# 7️⃣ FILTER BY DATE
# 👉 When to use: To limit dataset to a specific date range.

baked |>
  filter(date <= lubridate::ymd(20050301))


# 8️⃣ SIMPLE LINEAR REGRESSION
# 👉 When to use: To fit model predicting Y from X
#     (e.g., this month from last month’s lag).

mod_lm <- linear_reg() |>
  set_engine("lm") |>
  fit(sales_volume ~ lag_1_sales_volume, data = baked)
summary(mod_lm$fit)


# 9️⃣ DISCOUNT EXPERIMENT – DESCRIPTIVE PLOT
# 👉 When to use: To visualize sales distribution across groups (0/1).

data <- readr::read_csv("data/sales_dag.csv", show_col_types = FALSE) |>
  mutate(discount = factor(discount))

ggplot(data, aes(x = sales, after_stat(count), fill = discount)) +
  geom_histogram(alpha = 0.3, position = "identity", bins = 30, color = "#e9ecef") +
  geom_density(alpha = 0.3) +
  xlab("Sales") + ylab("Density") +
  theme_minimal()


# 🔟 DIFFERENCE IN MEANS (UPLIFT)
# 👉 When to use: To compare mean sales between discount and no-discount groups.

mean_sales <- data |>
  group_by(discount) |>
  summarize(`mean sales` = mean(sales)) |>
  mutate(`mean sales difference` = `mean sales` - lag(`mean sales`))
mean_sales


# 11️⃣ CHECK CONFOUNDING (SALES vs VISITS)
# 👉 When to use: To check if groups differ in other variables (visits → bias).

data |>
  mutate(discount = factor(discount)) |>
  ggplot(aes(x = visits, y = sales, color = discount)) +
  geom_point() +
  facet_grid(cols = vars(discount))


# 12️⃣ ADJUST FOR EXPOSURE – SALES PER VISIT
# 👉 When to use: To normalize sales by number of visits or other exposure variable.

mean_sales_pv <- data |>
  group_by(discount) |>
  summarize(sales_per_visit = mean(sales_per_visit))
mean_sales_pv

# Plot distributions
ggplot(data, aes(x = sales_per_visit, after_stat(count), fill = discount)) +
  geom_histogram(alpha = 0.3, position = "identity", bins = 30, color = "#e9ecef") +
  xlab("Sales per Visit") + ylab("Count") +
  theme_minimal()

###### Lab 3 ###########################################################################

LINEAR REGRESSION + MULTICOLLINEARITY ####

# WHEN TO USE:
# - Asked to fit a linear regression model and interpret coefficients
# - Question mentions multicollinearity or asks to check for redundant predictors
# - You're told to use VIF to evaluate predictors

lm_model <- lm(y ~ x1 + x2 + x3, data = your_data)
summary(lm_model)

Check multicollinearity
performance::check_collinearity(lm_model)


OMITTED VARIABLE BIAS / CONFOUNDING ####

# WHEN TO USE:
# - Question mentions confounding, bias, or unobserved variables
# - You're comparing two models: one with a confounder, one without
# - You need to demonstrate that a hidden variable affects coefficient estimates

lm1 <- lm(demand ~ price, data = dat1)
lm2 <- lm(demand ~ price + unobserved1, data = dat1)
summary(lm1)
summary(lm2)


DATA CLEANING & SETUP (EPA EXAMPLE) ####

# WHEN TO USE:
# - Asked to clean messy Excel data and prepare for modeling
# - Prompt says: rename variables, handle missing values, convert types
# - You’re selecting specific columns from a larger dataset

cars_23 <- readxl::read_xlsx("data/2023 FE Guide.xlsx") %>%
  dplyr::select(
    "Comb FE (Guide) - Conventional Fuel", "Eng Displ", "# Cyl", Transmission,
    "# Gears", "Air Aspiration Method Desc", "Regen Braking Type Desc",
    "Batt Energy Capacity (Amp-hrs)", "Drive Desc",
    "Fuel Usage Desc - Conventional Fuel", "Cyl Deact?", "Var Valve Lift?"
  ) %>%
  janitor::clean_names()

# Inspect
DataExplorer::introduce(cars_23)
DataExplorer::plot_missing(cars_23)

# Convert types
cars_23 <- cars_23 %>%
  mutate(across(c(comb_fe_guide_conventional_fuel, number_cyl, number_gears), as.integer)) %>%
  replace_na(list(
    batt_energy_capacity_amp_hrs = 0,
    regen_braking_type_desc = ""
  )) %>%
  mutate(across(c(transmission, air_aspiration_method_desc,
                  regen_braking_type_desc, drive_desc,
                  fuel_usage_desc_conventional_fuel,
                  cyl_deact, var_valve_lift), as.factor))


RECIPE CREATION + PREPROCESSING ####

# WHEN TO USE:
# - Question asks to preprocess or standardize data
# - Prompt says: “scale numeric”, “dummy encode factors”, “prepare for modeling”

cars_23_rec <- recipe(comb_fe_guide_conventional_fuel ~ ., data = cars_23) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_factor())

summary(cars_23_rec)


TRAIN / TEST SPLIT + APPLY RECIPE ####

# WHEN TO USE:
# - You need to split data for model training and testing
# - Prompt says: apply recipe consistently, avoid data leakage

set.seed(1966)
train <- cars_23 %>% rowid_to_column("ID") %>% sample_frac(0.75)
test  <- anti_join(cars_23 %>% rowid_to_column("ID"), train, by = "ID")
train <- select(train, -ID)
test  <- select(test,  -ID)

# Prep and bake
cars_23_prep  <- prep(cars_23_rec, training = train, retain = TRUE)
cars_23_train <- bake(cars_23_prep, new_data = NULL)
cars_23_test  <- bake(cars_23_prep, new_data = test)


XGBOOST MODELING (UNTUNED) ####

# WHEN TO USE:
# - Prompt says: fit an XGBoost regression model
# - Asked to compute RMSE / R-squared on test data

untuned_xgb <- xgboost(
  data = cars_23_train %>% select(-comb_fe_guide_conventional_fuel) %>% as.matrix(),
  label = cars_23_train %>% pull(comb_fe_guide_conventional_fuel),
  nrounds = 1000,
  objective = "reg:squarederror",
  early_stopping_rounds = 3,
  max_depth = 6,
  eta = 0.25,
  verbose = FALSE
)

# Evaluate
yhat <- predict(untuned_xgb, cars_23_test %>% select(-comb_fe_guide_conventional_fuel) %>% as.matrix())
y    <- cars_23_test %>% pull(comb_fe_guide_conventional_fuel)
caret::postResample(yhat, y)


HYPERPARAMETER TUNING (XGBOOST) ####

# WHEN TO USE:
# - Prompt says: “tune model”, “cross-validation”, or “find best eta/depth”
# - You need to test multiple parameter combos using CV

hyper_grid <- expand.grid(
  max_depth = seq(3, 6, 1),
  eta = seq(0.2, 0.35, 0.01)
)

xgb_train_rmse <- NULL
xgb_test_rmse  <- NULL

for (j in 1:nrow(hyper_grid)) {
  set.seed(123)
  m_xgb_cv <- xgb.cv(
    data = cars_23_train %>% select(-comb_fe_guide_conventional_fuel) %>% as.matrix(),
    label = cars_23_train %>% pull(comb_fe_guide_conventional_fuel),
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    nfold = 5,
    max_depth = hyper_grid$max_depth[j],
    eta = hyper_grid$eta[j],
    verbose = FALSE
  )
  
  xgb_train_rmse[j] <- m_xgb_cv$evaluation_log$train_rmse_mean[m_xgb_cv$best_iteration]
  xgb_test_rmse[j]  <- m_xgb_cv$evaluation_log$test_rmse_mean[m_xgb_cv$best_iteration]
}

best <- hyper_grid[which.min(xgb_test_rmse), ]


FINAL TUNED XGBOOST MODEL ####

# WHEN TO USE:
# - Asked to train a model using best parameters
# - You're comparing tuned model vs untuned model

tuned_xgb <- xgboost(
  data = cars_23_train %>% select(-comb_fe_guide_conventional_fuel) %>% as.matrix(),
  label = cars_23_train %>% pull(comb_fe_guide_conventional_fuel),
  nrounds = 1000,
  objective = "reg:squarederror",
  early_stopping_rounds = 3,
  max_depth = best$max_depth,
  eta = best$eta,
  verbose = FALSE
)

# Evaluate tuned model
yhat <- predict(tuned_xgb, cars_23_test %>% select(-comb_fe_guide_conventional_fuel) %>% as.matrix())
y    <- cars_23_test %>% pull(comb_fe_guide_conventional_fuel)
caret::postResample(yhat, y)


FEATURE IMPORTANCE (INTERPRET XGBOOST) ####

# WHEN TO USE:
# - Question says: "What are the most important predictors?"
# - Asked to interpret the model or visualize variable importance

importance_matrix <- xgb.importance(model = tuned_xgb)
xgb.plot.importance(importance_matrix[1:10, ], xlab = "Feature Importance")
# ✅ Most important: usually "eng_displ"


###### Lab 4 ###########################################################################

### DECISION HELPER
# Use this section when deciding what to do for each question type

# If the target variable looks skewed or right-tailed → use step_log()
# If numeric variables vary wildly in scale → use step_center() + step_scale()
# If a categorical column has too many rare levels → use step_other()
# If you have categorical predictors and need numeric inputs → use step_dummy()
# If question asks for a quick dataset overview → use skimr::skim()
# If question asks for data partitioning → use rsample::initial_split()
# If question says “evaluate stability” or “use resampling” → use rsample::bootstraps()
# If question says “compare or tune models” → use workflowsets::workflow_map()
# If asked “which model performs best” → use workflowsets::rank_results()
# If asked “finalize and test model” → use tune::finalize_workflow() + fit()
# If asked “interpret generalization gap” → compute (test_MSE/train_MSE) - 1


### 1) SETUP & EDA
# USE WHEN: starting a lab/exam project; need packages, data, and a fast overview.

if (!"librarian" %in% rownames(installed.packages())) install.packages("librarian")
librarian::shelf(tidyverse, tidymodels, modeldata, ranger, broom, skimr, readr)

theme_set(theme_bw(base_size = 18) + theme(legend.position = "top"))

dat <- modeldata::ames
try(cat(readr::read_file("http://jse.amstat.org/v19n3/decock/DataDocumentation.txt")), silent = TRUE)

dat |> skimr::skim()


## 2) TRAIN / TEST SPLIT
# USE WHEN: you’re asked to split data (e.g., 75/25) and avoid leakage.

set.seed(8740)
data_split <- rsample::initial_split(dat, strata = "Sale_Price", prop = 0.75)
ames_train <- rsample::training(data_split)
ames_test  <- rsample::testing(data_split)


## 3) RECIPE: PREPROCESSING
# USE WHEN: you must standardize numerics, log-transform target, pool rare cats, make dummies.

norm_recipe <-
  recipe(Sale_Price ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold,
         data = ames_train) |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  step_log(Sale_Price, base = exp(1)) |>
  step_other(Neighborhood) |>
  step_dummy(all_nominal_predictors())

norm_recipe |> prep(training = ames_train, retain = TRUE) |> broom::tidy(1)
norm_recipe |> prep(training = ames_train, retain = TRUE) |> broom::tidy(2)


### 4) MODEL SPECS (parsnip)
# USE WHEN: declaring which models to try before fitting or tuning.

lm_mod_base <-
  linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

lm_mod_glmnet <-
  linear_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet") |>
  set_mode("regression")

lm_mod_rforest <-
  rand_forest(trees = tune(), min_n = tune()) |>
  set_engine("ranger") |>
  set_mode("regression")


### 5) TRANSLATE (optional sanity check)
# USE WHEN: you need to show which underlying R functions will be called.

lm_mod_base    |> parsnip::translate()
lm_mod_glmnet  |> parsnip::translate()
lm_mod_rforest |> parsnip::translate()


### 6) RESAMPLING: BOOTSTRAPS
# USE WHEN: you must evaluate models via resampling (stability/variance of metrics).

set.seed(8740)
train_resamples <- rsample::bootstraps(ames_train)


### 7) WORKFLOW SET (recipe + models)
# USE WHEN: you want to fit/tune multiple models with the SAME preprocessing.

all_workflows <-
  workflowsets::workflow_set(
    preproc = list(base = norm_recipe),
    models  = list(
      base   = lm_mod_base,
      glmnet = lm_mod_glmnet,
      forest = lm_mod_rforest
    )
  )

all_workflows |> tidyr::unnest(info)


### 8) FIT & TUNE across resamples
# USE WHEN: training all workflows and tuning hyperparameters in one go.

all_workflows <- all_workflows |>
  workflowsets::workflow_map(
    verbose   = TRUE,
    resamples = train_resamples,
    grid      = 5
  )


### 9) COMPARE PERFORMANCE
# USE WHEN: ranking models by RMSE (lower better) or RSQ (higher better).

# Method A: manual unnest
all_workflows |>
  dplyr::select(wflow_id, result) |>
  tidyr::unnest(result) |>
  tidyr::unnest(.metrics) |>
  dplyr::filter(.metric == "rmse") |>
  dplyr::group_by(wflow_id) |>
  dplyr::arrange(.estimate) |>
  dplyr::slice(1)

# Method B: quick leaderboard
workflowsets::rank_results(
  all_workflows,
  rank_metric = "rmse",
  select_best = TRUE
)


### 10) FINALIZE & FIT BEST MODEL
# USE WHEN: you must lock in tuned params, train on TRAIN, then evaluate on TEST.

best_model_workflow <-
  all_workflows |>
  workflowsets::extract_workflow("base_forest")

best_model_workflow <-
  best_model_workflow |>
  tune::finalize_workflow(
    tibble::tibble(trees = 1, min_n = 11)
  )

training_fit <- best_model_workflow |> fit(data = ames_train)
testing_fit  <- best_model_workflow |> fit(data = ames_test)

training_fit
testing_fit

### 11) TEST vs TRAIN ERROR RATIO (optional)
# USE WHEN: asked to show generalization gap (how much worse test is than train).

mse_train <- 0.08474425
mse_test  <- 0.09445689
(test_over_train_gap <- (mse_test / mse_train) - 1)  # ≈ 0.115 → ~11.5% higher on test

###### Lab 5 ###########################################################################

# LAB 5 CHEAT SHEET – Data Analytics Methods & Algorithms (R)
# (Fully copy-paste ready for exam; run section by section)


# 1. EDA & DATA SETUP

# ✅ USE TO FIND:
# • Data structure (rows, columns, variable types)
# • Missing values or imbalanced target
# • Distributions / outliers of numeric variables
# • Which variables may need normalization or dummies
# • Basic visual insights before modeling

if (!"librarian" %in% rownames(installed.packages())) install.packages("librarian")
librarian::shelf(tidyverse, skimr, tidymodels, ggplot2)

data <- read_csv("data/Telco-Customer-Churn.csv", show_col_types = FALSE) |>
  mutate(churn = as.factor(churn))

skimr::skim(data)

ggplot(data, aes(monthly_charges)) + geom_histogram()
ggplot(data, aes(tenure)) + geom_histogram()



# 2. TRAIN-TEST SPLIT & RECIPE CREATION

# ✅ USE TO FIND:
# • How to split dataset into training (70%) & test (30%)
# • How to preprocess predictors before modeling:
#     – step_normalize() → scale numeric variables
#     – step_dummy() → convert categorical → numeric (0/1)
# • Ensures same transformations applied to both train/test

set.seed(8740)

data_split <- rsample::initial_split(data, prop = 0.7)
default_train <- training(data_split)
default_test  <- testing(data_split)

default_recipe <- recipe(churn ~ ., data = default_train) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())



# 3. LOGISTIC REGRESSION MODEL

# ✅ USE TO FIND:
# • Build a binary classification model (Yes/No)
# • Evaluate model with accuracy, precision, recall, F1, ROC/AUC
# • Create workflow combining recipe + model
# • Generate prediction probabilities (.pred_Yes/.pred_No)

default_model <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

default_workflow <- workflow() |>
  add_recipe(default_recipe) |>
  add_model(default_model)

lm_fit <- fit(default_workflow, data = default_train)
training_results <- augment(lm_fit, default_train)

m_set_fn <- metric_set(accuracy, precision, recall, f_meas, spec, sens, ppv, npv)
training_results |> m_set_fn(truth = churn, estimate = .pred_class)

training_results |> roc_auc(truth = churn, .pred_No)
training_results |> roc_curve(truth = churn, .pred_No) |> autoplot()



# 4. MODEL EFFECTS / ODDS RATIOS

# ✅ USE TO FIND:
# • Strength & direction of each predictor
# • Interpret variable impact on churn odds
# • Convert log-odds → odds ratio with exp()

fit_tbl <- tidy(lm_fit) |> arrange(desc(abs(estimate)))

fit_tbl |>
  filter(term == "tenure") |>
  pull(estimate) |>
  exp()



# 5. KNN MODEL (K = 3)

# ✅ USE TO FIND:
# • Classification using similarity (nearest neighbors)
# • Non-linear decision boundaries
# • Compare KNN vs Logistic Regression performance
# • Identify overfitting if accuracy ≈ 1.0 on training

default_model_knn <- nearest_neighbor(neighbors = 3) |>
  set_engine("kknn") |>
  set_mode("classification")

default_workflow_knn <- default_workflow |> update_model(default_model_knn)

lm_fit_knn <- fit(default_workflow_knn, data = default_train)
training_results_knn <- augment(lm_fit_knn, default_train)

training_results_knn |> m_set_fn(truth = churn, estimate = .pred_class)
training_results_knn |> roc_auc(truth = churn, .pred_No)
training_results_knn |> roc_curve(truth = churn, .pred_No) |> autoplot()



# 6. CROSS-VALIDATION (5-FOLD)

# ✅ USE TO FIND:
# • More reliable model accuracy than one train/test split
# • Detect overfitting (compare CV AUC vs training AUC)
# • Average metrics across multiple folds

data_vfold_cv <- vfold_cv(data, v = 5)

rf_fit_rs <- fit_resamples(
  default_workflow_knn,
  resamples = data_vfold_cv,
  control = control_resamples(save_pred = TRUE)
)

rf_fit_rs |> collect_metrics()
rf_fit_rs |> collect_predictions() |> roc_curve(truth = churn, .pred_No) |> autoplot()



# 7. TUNE K FOR KNN (GRID SEARCH)

# ✅ USE TO FIND:
# • Optimal number of neighbors (K)
# • Avoid guessing K; use cross-validation + grid search
# • Compare accuracy and AUC for each K

default_model_knn_tuned <- nearest_neighbor(neighbors = tune()) |>
  set_engine("kknn") |>
  set_mode("classification")

default_workflow_knn <- default_workflow |> update_model(default_model_knn_tuned)

clust_num_grid <- grid_regular(neighbors(), levels = 10)

tune_results <- tune_grid(
  default_workflow_knn,
  resamples = data_vfold_cv,
  grid = clust_num_grid,
  control = control_grid(save_pred = TRUE),
  metrics = metric_set(accuracy, roc_auc)
)



# 8. PLOT TUNING RESULTS

# ✅ USE TO FIND:
# • Visualize performance trend as K increases
# • Identify “sweet spot” where AUC/Accuracy peaks
# • Choose K before model finalization

tune_results |>
  collect_metrics() |>
  ggplot(aes(neighbors, mean)) +
  geom_line(linewidth = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~.metric, scales = "free", nrow = 2)



# 9. FINALIZE BEST KNN MODEL

# ✅ USE TO FIND:
# • Lock in best K (highest AUC) and retrain full model
# • Evaluate on unseen test data only once
# • Report final metrics & ROC curve

best_nn <- tune_results |> select_best(metric = "roc_auc")
final_wf <- default_workflow_knn |> finalize_workflow(best_nn)

final_fit <- final_wf |> last_fit(data_split)

final_fit |> collect_metrics()
final_fit |> collect_predictions() |> roc_curve(truth = churn, .pred_No) |> autoplot()



# 10. K-MEANS CLUSTERING (UNSUPERVISED)

# ✅ USE TO FIND:
# • Discover natural groups without a target variable
# • Visualize clusters and centroids for k = 1…9
# • Identify best k using elbow method
# • Compare k-means groups with original labeled data

labelled_points <- read_csv("data/lab_5_clusters.csv", show_col_types = FALSE)

ggplot(labelled_points, aes(x1, x2, color = cluster)) +
  geom_point(alpha = 0.3) +
  theme(legend.position = "none")

points <- labelled_points |> select(-cluster)

kclusts <- tibble(k = 1:9) |>
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )

clusters    <- kclusts |> unnest(cols = c(tidied))
assignments <- kclusts |> unnest(cols = c(augmented))
clusterings <- kclusts |> unnest(cols = c(glanced))

p <- assignments |>
  ggplot(aes(x1, x2, color = .cluster)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~k) +
  theme(legend.position = "none")

p + geom_point(data = clusters, size = 10, shape = "x")

clusterings |>
  ggplot(aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

# 🔹 ELBOW INTERPRETATION:
# Look for the bend in total within-cluster sum of squares.
# That “elbow” (usually around k = 3) is the optimal number of clusters.

###### Lab 6 ###########################################################################


# LAB 6 EXAM CHEAT SHEET – TIME SERIES FORECASTING (TidyModels + Modeltime)

# 🔍 FIND: “EDA” | “time scaling” | “split train test” | “recipe”
# 🔍 FIND: “model spec” | “workflow fit” | “accuracy” | “forecast”
# 🔍 FIND: “refit future” | “retail monthly” | “ETS additive multiplicative”


# 1. EXPLORATORY DATA ANALYSIS (EDA)

# 🔍 FIND: visualize trend / seasonality / autocorrelation / ACF / diagnostics
# USE THIS WHEN:
# - You need to plot or explore overall time patterns
# - The question says “visualize trend or seasonality before modeling”


library(timetk)

# Plot raw time series
timetk::taylor_30_min |>
  timetk::plot_time_series(date, value,
                           .title = "Electricity Demand (30-min) – Trend + Pattern"
  )

# Plot ACF / PACF to see autocorrelation
timetk::taylor_30_min |>
  timetk::plot_acf_diagnostics(date, value, .lags = 100)

# Plot seasonal diagnostics (hour / day / week patterns)
timetk::taylor_30_min |>
  timetk::plot_seasonal_diagnostics(date, value)




# 2. TIME SCALING / AGGREGATION

# 🔍 FIND: summarise_by_time / downscale / upscale / hourly daily aggregation
# USE THIS WHEN:
# - You need to change time granularity (e.g., 30-min → hourly or daily)
# - The question says “aggregate by hour / day / week”


taylor_60_min <- timetk::taylor_30_min |>
  timetk::summarise_by_time(
    .date_var = date,
    .by = "hour",       # ⬅️ change to "day" or "week" if needed
    value = sum(value)  # ⬅️ can also use mean(), max(), etc.
  )




# 3. TRAIN / TEST SPLIT

# 🔍 FIND: time_series_split / training() / testing() / plot split
# USE THIS WHEN:
# - You must split chronologically (not random)
# - The question says “create training and test datasets”


library(rsample)

splits <- taylor_60_min |>
  timetk::time_series_split(initial = "2 months", assess = "1 weeks")

# Visualize training vs testing period
splits |>
  timetk::tk_time_series_cv_plan() |>
  timetk::plot_time_series_cv_plan(date, value)

# Extract data
train <- rsample::training(splits)
test  <- rsample::testing(splits)




# 4. FEATURE ENGINEERING (RECIPES)

# 🔍 FIND: step_timeseries_signature / step_normalize / step_dummy
# USE THIS WHEN:
# - Asked to create time-based features or preprocess data
# - For regression or ML models needing numeric/categorical inputs


library(recipes)

# Base recipe – for ARIMA / ETS (use date directly)
base_rec <- train |>
  recipes::recipe(value ~ date)

# Linear regression recipe – adds engineered time features
lm_rec <- train |>
  recipes::recipe(value ~ .) |>
  timetk::step_timeseries_signature(date) |>
  recipes::step_select(value, date_index.num, date_month.lbl, date_wday.lbl, date_hour) |>
  recipes::step_normalize(date_index.num) |>
  recipes::step_mutate(date_hour = as.factor(date_hour)) |>
  recipes::step_dummy(all_nominal(), one_hot = TRUE)




# 5. MODEL SPECIFICATIONS

# 🔍 FIND: model specification / exp_smoothing / arima_reg / linear_reg
# USE THIS WHEN:
# - You must define model types before training
# - Question says “create model specs for ETS, ARIMA, or GLMNET”


library(modeltime)
library(parsnip)

model_ets <- modeltime::exp_smoothing() |> parsnip::set_engine("ets")         # Exponential smoothing
model_arima <- modeltime::arima_reg() |> parsnip::set_engine("auto_arima")   # ARIMA automatic
model_lm <- parsnip::linear_reg(penalty = 0.02, mixture = 0.5) |> set_engine("glmnet")  # Regularized LM




# 6. MODEL FITTING (WORKFLOWS)

# 🔍 FIND: workflow / add_recipe / add_model / fit
# USE THIS WHEN:
# - You must “train” the models on the training data
# - Combine recipe + model in one pipeline


library(workflows)

# ETS model
workflow_fit_ets <- workflows::workflow() |>
  workflows::add_recipe(base_rec) |>
  workflows::add_model(model_ets) |>
  parsnip::fit(train)

# ARIMA model
workflow_fit_arima <- workflows::workflow() |>
  workflows::add_recipe(base_rec) |>
  workflows::add_model(model_arima) |>
  parsnip::fit(train)

# Linear regression model
workflow_fit_lm <- workflows::workflow() |>
  workflows::add_recipe(lm_rec) |>
  workflows::add_model(model_lm) |>
  parsnip::fit(train)




# 7. CALIBRATION & ACCURACY

# 🔍 FIND: modeltime_calibrate / modeltime_accuracy
# USE THIS WHEN:
# - Asked “Compare models (RMSE/MAE/RSQ)” or “Which model fits best?”


model_tbl <- modeltime::modeltime_table(
  workflow_fit_ets,
  workflow_fit_arima,
  workflow_fit_lm
)

calibration_tbl <- model_tbl |> modeltime::modeltime_calibrate(test)

# Evaluate performance metrics
calibration_tbl |> modeltime::modeltime_accuracy()




# 8. FORECAST ON TEST PERIOD

# 🔍 FIND: modeltime_forecast / plot_modeltime_forecast
# USE THIS WHEN:
# - You must plot predicted vs actual for test set
# - To visually compare model performance


calibration_tbl |>
  modeltime::modeltime_forecast(
    new_data = test,
    actual_data = taylor_60_min
  ) |>
  modeltime::plot_modeltime_forecast()




# 9. FUTURE FORECAST (REFIT MODELS)

# 🔍 FIND: modeltime_refit / forecast future / next 2 weeks
# USE THIS WHEN:
# - You must refit models on entire dataset and predict ahead
# - The question says “forecast next period(s)”


refit_tbl <- calibration_tbl |>
  modeltime::modeltime_refit(data = taylor_60_min)

refit_tbl |>
  modeltime::modeltime_forecast(
    h = "2 weeks",
    actual_data = taylor_60_min
  ) |>
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 12,
    .interactive = TRUE
  )




# 10. SEASONAL RETAIL FORECAST (MONTHLY ETS MODELS)

# 🔍 FIND: ETS additive multiplicative / monthly data / step_log / damping
# USE THIS WHEN:
# - Data has strong monthly seasonality (e.g., retail sales)
# - You must compare ETS(A,A,A) vs ETS(M,M,M)


# Create base recipe
base_rec <- retail_dat |>
  recipes::recipe(monthly_sales ~ date) |>
  recipes::step_log(monthly_sales)

# ETS (A,A,A) – additive model
model_ets_AAA <- modeltime::exp_smoothing(
  error = "additive",
  trend = "additive",
  season = "additive",
  damping = "none"
) |> parsnip::set_engine("ets")

# ETS (M,M,M) – multiplicative model (with damping)
model_ets_MMM <- modeltime::exp_smoothing(
  error = "multiplicative",
  trend = "multiplicative",
  season = "multiplicative",
  damping = "damped"
) |> parsnip::set_engine("ets")

# Fit both models
workflow_fit_AAA <- workflows::workflow() |>
  workflows::add_recipe(base_rec) |>
  workflows::add_model(model_ets_AAA) |>
  parsnip::fit(retail_dat)

workflow_fit_MMM <- workflows::workflow() |>
  workflows::add_recipe(base_rec) |>
  workflows::add_model(model_ets_MMM) |>
  parsnip::fit(retail_dat)

# Combine and forecast 1 month ahead
model_tbl <- modeltime::modeltime_table(workflow_fit_AAA, workflow_fit_MMM)
calibration_tbl <- model_tbl |> modeltime::modeltime_calibrate(retail_dat)

predictions <- calibration_tbl |>
  modeltime::modeltime_forecast(h = "1 month", actual_data = retail_dat)

# Plot model comparison
predictions |>
  dplyr::slice_tail(n = 2) |>
  ggplot(aes(x = .model_desc, y = .value)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = .conf_lo, ymax = .conf_hi),
                width = 0.2, color = "blue") +
  labs(
    title = "Retail Sales Predictions with Error Bars",
    x = "Model",
    y = "Sales ($millions)"
  ) +
  theme_minimal()

# ✅ Note: Typically the multiplicative ETS performs better for
# monthly retail data with growing seasonal peaks.


# END OF LAB 6 EXAM CHEAT SHEET



# ==========10.5 SUPERVISED LEARNING WORKFLOW – Classification===============================================

# 1. Split data
set.seed(8740)
data_split <- initial_split(data, prop = 0.7)
default_train <- training(data_split)
default_test <- testing(data_split)

# 2. Recipe (preprocessing)
default_recipe <- recipe(churn ~ ., data = default_train) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())

# 3. Logistic regression model
default_model <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

# 4. Workflow
default_workflow <- workflow() |>
  add_recipe(default_recipe) |>
  add_model(default_model)

# 5. Fit
lm_fit <- fit(default_workflow, data = default_train)

# 6. Metrics
training_results <- augment(lm_fit, default_train)
m_set_fn <- metric_set(accuracy, precision, recall, f_meas, sens, spec, ppv, npv)
training_results |> m_set_fn(truth = churn, estimate = .pred_class)

# 7. ROC curve
training_results |> roc_curve(truth = churn, .pred_No) |> autoplot()

# 8. KNN model
default_model_knn <- nearest_neighbor(neighbors = 3) |> 
  set_engine("kknn") |>
  set_mode("classification")
default_workflow_knn <- default_workflow |> update_model(default_model_knn)
lm_fit_knn <- fit(default_workflow_knn, data = default_train)

# 9. Cross-validation
data_vfold_cv <- vfold_cv(data, v = 5)
fit_resample_results <- fit_resamples(
  default_workflow_knn, 
  resamples = data_vfold_cv, 
  control = control_resamples(save_pred = TRUE)
)

# 10. Tuning
default_model_knn_tuned <- nearest_neighbor(neighbors = tune()) |> 
  set_engine("kknn") |> 
  set_mode("classification")
default_workflow_knn <- default_workflow |> update_model(default_model_knn_tuned)
k_grid <- grid_regular(neighbors(), levels = 10)
tune_results <- tune_grid(
  default_workflow_knn,
  resamples = data_vfold_cv,
  grid = k_grid,
  control = control_grid(save_pred = TRUE),
  metrics = metric_set(accuracy, roc_auc)
)

# =========10.5 UNSUPERVISED LEARNING WORKFLOW – K-means Clustering================================================


# 1. Load labeled data (for inspection only)
labelled_points <- read_csv("data/lab_5_clusters.csv")
ggplot(labelled_points, aes(x1, x2, color = cluster)) + geom_point()

# 2. Remove label
points <- labelled_points |> select(-cluster)

# 3. Run K-means for k = 1 to 9
kclusts <- tibble(k = 1:9) |>
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )

# 4. Unnest
clusters     <- kclusts |> unnest(tidied)
assignments  <- kclusts |> unnest(augmented)
clusterings  <- kclusts |> unnest(glanced)

# 5. Plot clusters
p <- assignments |>
  ggplot(aes(x1, x2, color = .cluster)) +
  geom_point() +
  facet_wrap(~k)
p + geom_point(data = clusters, size = 10, shape = "x")

# 6. Elbow plot
clusterings |>
  ggplot(aes(k, tot.withinss)) +
  geom_line() +
  geom_point()


# ============10.6 TIME SERIES FORECASTING – FULL STEP-BY-STEP TEMPLATE (WITH CODE)============================

# STEP 1: LOAD LIBRARIES AND DATA
library(tidyverse)
library(tidymodels)
library(timetk)
library(modeltime)

# data <- read_csv("your_data.csv")


# STEP 2: VISUALIZE PATTERNS (TREND, SEASONALITY, ACF)
data |> 
  plot_time_series(date, value)

data |> 
  plot_seasonal_diagnostics(date, value)

data |> 
  plot_acf_diagnostics(date, value)


# STEP 3: AGGREGATE IF NEEDED
# e.g., convert 30-min to hourly
data_hourly <- data |> 
  summarise_by_time(.date_var = date, .by = "hour", value = sum(value))


# STEP 4: SPLIT INTO TRAIN / TEST
splits <- time_series_split(data_hourly, initial = "2 months", assess = "1 week")
train  <- training(splits)
test   <- testing(splits)


# STEP 5: CREATE RECIPE (IF MODEL NEEDS FEATURES)
lm_rec <- recipe(value ~ ., data = train) |> 
  step_timeseries_signature(date) |> 
  step_select(value, date_index.num, date_month.lbl, date_wday.lbl, date_hour) |> 
  step_normalize(date_index.num) |> 
  step_mutate(date_hour = as.factor(date_hour)) |> 
  step_dummy(all_nominal(), one_hot = TRUE)

base_rec <- recipe(value ~ date, data = train)


# STEP 6: DEFINE MODELS
model_ets <- exp_smoothing() |> set_engine("ets")
model_arima <- arima_reg() |> set_engine("auto_arima")
model_glmnet <- linear_reg(penalty = 0.02, mixture = 0.5) |> set_engine("glmnet")


# STEP 7: COMBINE INTO WORKFLOWS
wf_ets <- workflow() |> add_recipe(base_rec) |> add_model(model_ets)
wf_arima <- workflow() |> add_recipe(base_rec) |> add_model(model_arima)
wf_glmnet <- workflow() |> add_recipe(lm_rec) |> add_model(model_glmnet)

# Fit workflows
fit_ets <- fit(wf_ets, train)
fit_arima <- fit(wf_arima, train)
fit_glmnet <- fit(wf_glmnet, train)


# STEP 8: CALIBRATE AND EVALUATE
model_tbl <- modeltime_table(fit_ets, fit_arima, fit_glmnet)
calibration_tbl <- model_tbl |> modeltime_calibrate(test)
calibration_tbl |> modeltime_accuracy()


# STEP 9: FORECAST ON TEST SET (VISUAL COMPARISON)
calibration_tbl |> 
  modeltime_forecast(new_data = test, actual_data = data_hourly) |> 
  plot_modeltime_forecast()


# STEP 10: REFIT ON FULL DATA AND FORECAST FUTURE
refit_tbl <- calibration_tbl |> modeltime_refit(data_hourly)

refit_tbl |> 
  modeltime_forecast(h = "2 weeks", actual_data = data_hourly) |> 
  plot_modeltime_forecast()


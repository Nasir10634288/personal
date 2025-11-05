---
title: "Exam Solutions: Part 2"
format: html
editor: visual
---

# Part 2

## Q-11

This question uses data for the closing prices of the five major Canadian banks from 2005-08-10 to 2023-09-29.

```{r}
#| label: setup
#| message: false
#| warning: false

library(tidyverse)
library(tidyquant)
library(timetk)
library(modeltime)
library(parsnip)
library(recipes)
library(rsample)
library(workflows)
library(yardstick)
```

```{r}
#| label: read the bank price data

arima_data <- readr::read_csv('data/stock_data.csv', show_col_types = FALSE)
```

::: {#Q11 .callout-note appearance="simple" icon="false"}
## YOUR ANSWER Q11:

**(1)** Plot the data using functions in the timetk package (0.5 point)

```{r}
#| label: plot closing prices

arima_data %>%
  group_by(symbol) %>%
  plot_time_series(
    .date_var = date,
    .value = close,
    .facet_ncol = 2,
    .smooth = FALSE,
    .title = "Closing Prices for Five Major Canadian Banks (2005-2023)"
  )
```

**(2)** Create test/trains splits of the data, where the **initial period is 10 years** and the **assessment period is 1 year**. Plot the test/train series for CIBC (symbol 'CM'). **(0.5 point)**

```{r}
#| label: create test and train splits of the time series

# Filter for CM only
cm_data <- arima_data %>%
  filter(symbol == "CM") %>%
  select(date, close) %>%
  rename(CM = close)

# Create time series splits: 10 years initial, 1 year assessment
splits <- time_series_split(
  cm_data,
  assess = "1 year",
  cumulative = TRUE
)

# Plot the splits
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(
    .date_var = date,
    .value = CM,
    .title = "Train/Test Split for CIBC (CM) Stock Price"
  )
```

**(3)** Define a data preprocessing **recipe** and a **model** definition. The recipe is based on the formula `CM ~ .`, and make sure the data argument uses the training data. The model engine should be **auto_arima**.

Finally, create a **workflow** object containing the recipe and the model spec, and then **fit** the model using the training data. **(1 point)**

```{r}
#| label: create a workflow with a recipe and an ARIMA model spec

# A RECIPE
time_rec <- recipe(CM ~ ., data = training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("iso"), contains("xts"), contains("day"), contains("hour"),
          contains("minute"), contains("second"), contains("am.pm")) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

# A MODEL SPECIFICATION
model_spec_arima <- arima_reg() %>%
  set_engine("auto_arima")

# A FITTED WORKFLOW
workflow_fit_arima <- workflow() %>%
  add_model(model_spec_arima) %>%
  add_recipe(time_rec) %>%
  fit(training(splits))
```

**(4)** Create a **models table** with your fitted model and a **calibration table** that uses the **testing** data. Generate a forecast with the **testing** data and the original **arima_data**. Plot the forecast. **(1 point)**

```{r}
#| label: create both a models table and a calibration table

# A MODELS TABLE
models_tbl <- modeltime_table(
  workflow_fit_arima
)

# A CALIBRATION TABLE
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(testing(splits))

# PLOT OF THE FITTED MODEL FORECAST OF THE TRAINING DATA
calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = cm_data
  ) %>%
  plot_modeltime_forecast(
    .title = "ARIMA Forecast for CIBC (CM) Stock Price",
    .interactive = FALSE
  )
```

**(5)** Compute the accuracy metrics for the forecast. What is the $R^2$ (rsq) metric. **(1 point)**

```{r}
#| label: compute accuracy metrics and report r-squared

# Compute the metrics
accuracy_metrics <- calibration_tbl %>%
  modeltime_accuracy()

# Display metrics
accuracy_metrics
```

```{r}
# Extract R-squared
rsq_value <- accuracy_metrics %>%
  pull(rsq)
```

The rsq metric for the fit of the arima model to the testing data is: **`r round(rsq_value, 4)`**
:::

## Q-12

Execute the following code to create simulated observational data, where `D` is the treatment variable and `Y` is the response variable.

```{r}
#| echo: true
#| message: false
#| error: false

set.seed(8740)

n <- 800
V <- rbinom(n, 1, 0.2)
W <- 3*V + rnorm(n)
D <- V + rnorm(n)
Y <- D + W^2 + 1 + rnorm(n)
Z <- D + Y + rnorm(n)
data_obs <- tibble::tibble(V=V, W=W, D=D, Y=Y, Z=Z)
```

In the code below we fit several different outcome models. Compare the resulting coefficients for `D`. Which regressions appear to lead to unbiased estimates of the causal effect?

```{r}
#| echo: true
#| label: outcome models

# linear model of Y on X
lin_YX <- lm(Y ~ D, data=data_obs)

# linear model of Y on X and V
lin_YV <- lm(Y ~ D + V, data=data_obs)

# linear model Y on X and W
lin_YW <- lm(Y ~ D + W, data=data_obs)
```

```{r}
#| label: compare coefficients

# Display coefficients for comparison
cat("Coefficients for D in each model:\n")
cat("lin_YX (Y ~ D):      ", coef(lin_YX)["D"], "\n")
cat("lin_YV (Y ~ D + V):  ", coef(lin_YV)["D"], "\n")
cat("lin_YW (Y ~ D + W):  ", coef(lin_YW)["D"], "\n")
cat("\nTrue causal effect: 1.0 (from data generation process)\n")
```

**Causal Structure Analysis:**

From the data generation code:

-   V → W (W = 3\*V + noise)
-   V → D (D = V + noise)
-   W → Y (Y = D + W² + 1 + noise)
-   D → Y (Y = D + W² + 1 + noise)
-   D → Z (Z = D + Y + noise)
-   Y → Z (Z = D + Y + noise)

The causal DAG shows:

-   V is a confounder (affects both D and Y through W)
-   W is a mediator on one path and a confounder
-   Z is a collider (D → Z ← Y) and should NOT be adjusted for

::: {#Q12 .callout-note appearance="simple" icon="false"}
## YOUR ANSWER Q12:

1.  Regressions that appear to lead to unbiased estimates of the causal effect are: **lin_YW**

    -   **lin_YX** is biased because it doesn't control for the confounder V (through W)
    -   **lin_YV** is still biased because V affects Y through W, and W is not controlled
    -   **lin_YW** is unbiased because it controls for W, which blocks the backdoor path D ← V → W → Y

2.  Valid adjustment sets for the data used in this question are: **{W}** and **{V, W}**

    -   **{W}** - This blocks the backdoor path D ← V → W → Y
    -   **{V, W}** - This also works but includes redundant control
    -   **Note:** Z should NOT be in any adjustment set (it's a collider)
:::

## Q-13

For this question we'll use the **Spam Classification Dataset** available from the UCI Machine Learning Repository.

```{r}
#| message: false
#| label: read the spam data

library(tidymodels)

spam_data <- readr::read_csv('data/spam.csv', show_col_types = FALSE) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(type = forcats::as_factor(type))
```

::: {#Q13 .callout-note appearance="simple" icon="false"}
## YOUR ANSWER Q13:

**(1)** Split the data into test and training sets, and create a default recipe and a default model specification. Use the ***glmnet*** engine for the model, with **penalty** = 0.05 & **mixture** = 0.5. **(1 point)**

```{r}
#| label: split the data into test and train datasets, and create default recipe and glmnet model spec
#| message: false

set.seed(8740)

# create test/train splits
splits <- initial_split(spam_data, prop = 0.75, strata = type)
train <- training(splits)
test <- testing(splits)

default_recipe <- recipe(type ~ ., data = train) %>%
  step_normalize(all_numeric_predictors())

default_model <- logistic_reg(penalty = 0.05, mixture = 0.5) %>%
  set_engine("glmnet") %>%
  set_mode("classification")
```

**(2)** create a default workflow object with the recipe and the model specification, fit the workflow using `parsnip::fit` and the **training** data, and then generate the testing results by applying the fit to the **testing** data using `broom::augment` . **(1 point)**

```{r}
#| label: create default workflow with recipe and model spec, then fit the model with training data and predict type with test data
#| message: false

default_workflow <- workflow() %>%
  add_recipe(default_recipe) %>%
  add_model(default_model)

lm_fit <- default_workflow %>%
  fit(data = train)

testing_results <- lm_fit %>%
  augment(new_data = test)
```

**(3)** Evaluate the testing results by plotting the **roc_auc curve**, and calculating the **accuracy**. **(1 point)**

```{r}
#| label: plot roc_auc from test fit results
#| message: false

# ROC_AUC PLOT
testing_results %>%
  roc_curve(truth = type, .pred_nonspam) %>%
  autoplot() +
  labs(title = "ROC Curve for Spam Classification Model")
```

```{r}
#| label: calculate metrics from test fit results
#| message: false

# Calculate ROC-AUC value
roc_auc_value <- testing_results %>%
  roc_auc(truth = type, .pred_nonspam)

cat("ROC-AUC:", roc_auc_value$.estimate, "\n\n")

# CALCULATION OF ACCURACY
accuracy_value <- testing_results %>%
  accuracy(truth = type, estimate = .pred_class)

cat("Accuracy:", accuracy_value$.estimate, "\n")
```

**(4)** Is there a way you could improve the accuracy of this **model**? **(1 point)**

-   This model could be made more accurate by:
    -   **Hyperparameter tuning:** Use cross-validation to find optimal values for penalty and mixture parameters
    -   **Try different models:** Random forest, XGBoost, or SVM might perform better
    -   **Feature engineering:** Create interaction terms or polynomial features
    -   **Feature selection:** Remove irrelevant features that add noise
    -   **Handle class imbalance:** Use techniques like SMOTE if classes are imbalanced
    -   **Ensemble methods:** Combine multiple models for better predictions
    -   **More sophisticated preprocessing:** Apply PCA for dimensionality reduction
:::

## Q-14

::: {#Q14 .callout-note appearance="simple" icon="false"}
## YOUR ANSWER Q14:

1.  When preprocessing data for time series models, what is the function `timetk::step_fourier()` used for? **(1 point)**

-   The `timetk::step_fourier()` function is used for:
    -   **Adding Fourier series transformations** to capture seasonal/cyclical patterns in time series data
    -   **Converting periodic time patterns** into sine and cosine features that models can learn from
    -   **Helping models learn complex seasonal patterns** without needing explicit seasonal dummy variables
    -   **Particularly useful** for data with multiple seasonal periods or complex cyclical behavior
    -   The function creates pairs of sin/cos terms (harmonic waves) that together can approximate any periodic pattern

2.  Give an example of its use in a recipe that is engineered for use with weekly data records. **(1 point)**

-   An example of its use in a recipe that is engineered for use with weekly data records is:

```{r}
#| label: an example the use of step_fourier in a recipe
#| eval: false

library(recipes)
library(timetk)

# Example recipe for weekly sales data
weekly_recipe <- recipe(sales ~ date + other_predictors, data = weekly_sales_data) %>%
  # Add time series signature features
  step_timeseries_signature(date) %>%
  # Add Fourier terms for annual seasonality (52 weeks in a year)
  # K=2 means we create 2 pairs of sin/cos terms (4 features total)
  step_fourier(date, period = 52, K = 2) %>%
  # Can also add quarterly patterns if relevant (13 weeks per quarter)
  step_fourier(date, period = 13, K = 1) %>%
  # Remove unnecessary date features
  step_rm(date) %>%
  # Normalize all numeric predictors
  step_normalize(all_numeric_predictors())

# Alternative: For daily data with weekly pattern
daily_recipe <- recipe(value ~ date, data = daily_data) %>%
  step_timeseries_signature(date) %>%
  # Capture weekly seasonality (7 days)
  step_fourier(date, period = 7, K = 1) %>%
  # Capture annual seasonality (365.25 days)
  step_fourier(date, period = 365.25, K = 3) %>%
  step_rm(date) %>%
  step_normalize(all_numeric_predictors())
```

**Key Parameters:**

-   `period`: The length of the seasonal cycle (e.g., 52 for weekly data with annual pattern)
-   `K`: The number of sine/cosine pairs to generate (higher K captures more complex patterns)
:::

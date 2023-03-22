## Hitters: Predict salary based on performance statistics
library(tidyverse)
library(tidymodels)
library(ISLR2)

glimpse(Hitters)

## Penalized least squares (Lasso)

lasso_model <- linear_reg(penalty = tune(), mixture = 1) %>% # mixture = 1 is lasso
  set_engine("glmnet")

lasso_model %>%
  translate()

## preprocess the data and specify a formula
hit_recipe <- recipe(Salary ~ ., data = hit_complete) %>%
  step_zv(all_numeric_predictors()) %>% # throw away zero-variance columns
  step_dummy(all_factor_predictors()) %>% # need to code dummies
  step_normalize(all_numeric_predictors()) # Need to normalize for lasso

hit_recipe %>%
  prep() # Check what happens

hit_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>% # Inspect preprocessed values
  view()

lasso_workflow <- workflow() %>% ## Workflow: bundle model with formula/recipe
  add_model(lasso_model) %>%
  add_recipe(hit_recipe)

# Tune lambda using repeated cross-validation
set.seed(5234)
folds <- vfold_cv(hit_complete, v = 10, repeats = 5)

lambda_grid <- grid_regular(penalty(), levels = 100) # default range -10 0
glimpse(lambda_grid)
summary(lambda_grid)
lambda_grid %>% ggplot(aes(x = penalty)) + geom_histogram()
lambda_grid %>% ggplot(aes(x = penalty)) + 
  geom_histogram(bins = 10) +
  scale_x_log10()

lasso_workflow %>%
  tune_grid(resamples = folds,
            grid = lambda_grid) ->
  tuning_results

## Optional: parallel processing
system.time(
lasso_workflow %>%
  tune_grid(resamples = folds,
            grid = lambda_grid) ->
  tuning_results
)
# user  system elapsed 
# 12.150   0.085  12.228 
doParallel::registerDoParallel()
system.time(
lasso_workflow %>%
  tune_grid(resamples = folds,
            grid = lambda_grid) ->
  tuning_results
)
# user  system elapsed 
# 17.747   0.705   2.716 
###

glimpse(tuning_results)

tuning_results %>%
  collect_metrics %>%
  view()

## Introduce this incrementally
tuning_results %>%
  collect_metrics %>%
  ggplot(aes(x = penalty, y = mean, color = .metric)) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  geom_line() +
  scale_x_log10()

tuning_results %>%
  select_best("rmse") ->
  best_lambda_rmse

## Challenge: ?penalty. Change the range so you get a discernible minimum
## Solution:
lambda_grid <- grid_regular(penalty(range = c(-4, 2)), levels = 100)
# And redo the tuning above

## Finalize workflow with tuned value of lambda
tuned_lasso <- finalize_workflow(lasso_workflow, best_lambda_rmse)

final_fit <- tuned_lasso %>%
  fit(hit_complete) 
tidy(final_fit)

final_fit_glmnet <- extract_fit_engine(final_fit)
plot(final_fit_glmnet)
plot(final_fit_glmnet, xvar = "lambda", label = TRUE)
abline(v = log10(best_lambda_rmse$penalty))

## Challenge: Repeat for ridge regression (mixture = 0)
## Challenge: Compare to linear models.

## Solutions:
## Cross validation:
set.seed(436)
folds <- vfold_cv(hit_complete, v = 10, repeats = 10)
tuned_lasso %>%
  fit_resamples(resamples = folds) %>%
  collect_metrics()

summary(bsMod)
linear_reg() %>%
  set_engine("lm") %>%
  fit_resamples(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + 
                  CRBI + CWalks + Division + PutOuts + Assists,
      resamples = folds) %>%
  collect_metrics()

linear_reg() %>%
  set_engine("lm") %>%
  fit_resamples(Salary ~ AtBat + Hits + Walks + CRuns + CRBI + CWalks,
      resamples = folds) %>%
  collect_metrics()

## Ridge regression
ridge_model <- linear_reg(penalty = tune(), mixture = 0) %>% 
  set_engine("glmnet")
ridge_workflow <- workflow() %>% ## Workflow: bundle model with formula/recipe
  add_model(ridge_model) %>%
  add_recipe(hit_recipe)
# Tune lambda using repeated cross-validation
set.seed(5234)
folds <- vfold_cv(hit_complete, v = 10, repeats = 5)
lambda_grid <- grid_regular(penalty(range = c(-2, 10)), levels = 100)

ridge_workflow %>%
  tune_grid(resamples = folds,
            grid = lambda_grid) ->
  tuning_results
tuning_results %>%
  collect_metrics %>%
  ggplot(aes(x = penalty, y = mean, color = .metric)) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  geom_line() +
  scale_x_log10()

tuning_results %>%
  select_best("rmse") ->
  best_lambda_rmse
## Finalize workflow with tuned value of lambda
tuned_ridge <- finalize_workflow(ridge_workflow, best_lambda_rmse)

final_fit <- tuned_ridge %>%
  fit(hit_complete) 
tidy(final_fit)

final_fit_glmnet <- extract_fit_engine(final_fit)
plot(final_fit_glmnet)
plot(final_fit_glmnet, xvar = "lambda", label = TRUE)
abline(v = log10(best_lambda_rmse$penalty))


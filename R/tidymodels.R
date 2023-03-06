library(tidyverse)
library(tidymodels)

library(palmerpenguins)
set.seed(321)
penCoin <- penguins %>%
  drop_na() %>%
  mutate(coins = round(exp(bill_length_mm * 0.05 + bill_depth_mm * 0.02)) + 
           rpois(n(), 1) -1)

linear_reg() %>%
  set_engine("lm") %>%
  fit(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = penCoin) ->
  pFit1

linear_reg() %>%
  set_engine("glm", family = poisson) %>%
  fit(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = penCoin) ->
  pFit2 

tidy(pFit1)
tidy(pFit2)

oldpFit1 <- lm(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
               data = penCoin)
str(oldpFit1)

## Evaluate with train/test split

set.seed(771)
pen_split <- initial_split(penCoin)
pen_train <- training(pen_split)
pen_test <- testing(pen_split)

linear_reg() %>%
  set_engine("lm") %>%
  fit(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = pen_train) ->
  pFit1

linear_reg() %>%
  set_engine("glm", family = poisson) %>%
  fit(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = pen_train) ->
  pFit2 

predict(pFit1, pen_test) %>%
  bind_cols(pen_test) %>%
  rmse(truth = coins, estimate = .pred)
predict(pFit2, pen_test) %>%
  bind_cols(pen_test) %>%
  rmse(truth = coins, estimate = .pred)

## Experiment: Resplit, and rerun. Do you get different 
# error estimates?

## Cross Validation

set.seed(756)
folds <- vfold_cv(penCoin, v = 10)
folds
linear_reg() %>%
  set_engine("lm") %>%
  fit_resamples(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      resamples = folds) %>%
  collect_metrics()

linear_reg() %>%
  set_engine("glm", family = poisson) %>%
  fit_resamples(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      resamples = folds) %>%
  collect_metrics()

# Experiment:
## Different random seeds?
## Different fold sizes?

library(tidyverse)
library(tidymodels)
library(palmerpenguins)

glimpse(penguins)
penguinsAC <- penguins %>%
  filter(species != "Gentoo") %>%
  drop_na() %>%
  droplevels()
set.seed(6475)
pen_split <- initial_split(penguinsAC)
pen_train <- training(pen_split)
pen_test <- testing(pen_split)
logistic_reg() %>%
  set_engine("glm", family = binomial) %>%
  fit(species ~ body_mass_g + flipper_length_mm, 
      data = pen_train) ->
  pACfit
tidy(pACfit)

predict(pACfit, pen_test) %>%
  bind_cols(predict(pACfit, pen_test, type = "prob")) %>%
  bind_cols(pen_test) ->
  penAT_pred

penAT_pred %>%
  accuracy(truth = species, estimate = .pred_class)
penAT_pred %>%
  roc_auc(truth = species, estimate = .pred_Adelie)
penAT_pred %>%
  roc_curve(truth = species, .pred_Adelie) %>%
  ggplot(aes(x = 1-specificity, y = sensitivity)) +
  geom_point(aes(color = .threshold)) +
  geom_smooth(method = loess, se = FALSE, formula = y~x)

## Challenge: Do 10-fold cross validation and collect metrics.

set.seed(822)
folds <- vfold_cv(penguinsAC, v = 10)
logistic_reg() %>%
  set_engine("glm", family = binomial) %>%
  fit_resamples(species ~ body_mass_g + flipper_length_mm,
      resamples = folds) %>%
  collect_metrics()

folds <- vfold_cv(penguinsAC, v = nrow(penguinsAC))
logistic_reg() %>%
  set_engine("glm", family = binomial) %>%
  fit_resamples(species ~ body_mass_g + flipper_length_mm,
      resamples = folds) %>%
  collect_metrics()

looFolds <- loo_cv(penguinsAC)
looFolds
logistic_reg() %>%
  set_engine("glm", family = binomial) %>%
  fit_resamples(species ~ body_mass_g + flipper_length_mm,
      resamples = looFolds) 


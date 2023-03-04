library(tidyverse)
library(tidymodels)
library(ISLR2)

glimpse(Auto)

set.seed(911)
car_split <- initial_split(Auto, prop = 1/2)
car_train <- training(car_split)
car_test <- testing(car_split)

library(palmerpenguins)
set.seed(321)
penCoin <- penguins %>%
  drop_na() %>%
  mutate(coins = round(exp(bill_length_mm * 0.05 + bill_depth_mm * 0.02)) + 
           rpois(n(), 1) -1)
set.seed(771)
pen_split <- initial_split(penCoin)
pen_train <- training(pen_split)
pen_test <- testing(pen_split)

linear_reg() %>%
  set_engine("lm") %>%
  fit(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = pen_train) ->
  pMod1

linear_reg() %>%
  set_engine("glm", family = poisson) %>%
  fit(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = pen_train) ->
  pMod2 

tidy(pMod1)
tidy(pMod2)

predict(pMod1, pen_test) %>%
  bind_cols(pen_test) %>%
  rmse(truth = coins, estimate = .pred)
predict(pMod2, pen_test) %>%
  bind_cols(pen_test) %>%
  rmse(truth = coins, estimate = .pred)

set.seed(756)
folds <- vfold_cv(pen_train, v = 10)
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

set.seed(822)
folds <- vfold_cv(penCoin, v = 10)
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

### Challenge/HW?

library(tidyverse)
library(tidymodels)
library(palmerpenguins)

glimpse(penguins)
penguinsAT <- penguins %>%
  filter(species != "Gentoo") %>%
  drop_na() %>%
  droplevels()
set.seed(6475)
pen_split <- initial_split(penguinsAT)
pen_train <- training(pen_split)
pen_test <- testing(pen_split)
logistic_reg() %>%
  set_engine("glm", family = binomial) %>%
  fit(species ~ body_mass_g + flipper_length_mm, 
      data = pen_train) ->
  pATfit
tidy(pATfit)

predict(pATfit, pen_test) %>%
  bind_cols(predict(pATfit, pen_test, type = "prob")) %>%
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

set.seed(822)
folds <- vfold_cv(penguinsAT, v = 10)
logistic_reg() %>%
  set_engine("glm", family = binomial) %>%
  fit_resamples(species ~ body_mass_g + flipper_length_mm,
      resamples = folds) %>%
  collect_metrics()

library(palmerpenguins)
library(tidyverse)

set.seed(123)
penCoin <- penguins %>%
  drop_na() %>%
  mutate(coins = round(exp(bill_length_mm * 0.05 + bill_depth_mm * 0.02)) +
           rpois(n(), 1) - 1 )
oldMod <- lm(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
             data = penCoin)
summary(oldMod)

## The tidymodels way
library(tidymodels)

linear_reg() %>%
  set_engine("lm") %>%
  fit(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = penCoin) ->
  pFit1
tidy(pFit1)

linear_reg() %>%
  set_engine("glm", family = poisson) %>%
  fit(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = penCoin) ->
  pFit2
tidy(pFit2)

## Train/test split
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

#Experiment: Try rerandomizing. Is the error changing for different train/test
# splits?

## Cross validation

set.seed(756)
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

## Classification

penguinsAC <- penguins %>%
  filter(species != "Gentoo") %>%
  drop_na() %>%
  droplevels()

pen_split <- initial_split(penguinsAC)
pen_train <- training(pen_split)
pen_test <- testing(pen_split)

logistic_reg() %>%
  set_engine("glm", family = binomial) %>%
  fit(species ~ body_mass_g + flipper_length_mm,
      data = pen_train) ->
  penACfit
tidy(penACfit)

predict(penACfit, pen_test) %>%
  bind_cols(predict(penACfit, pen_test, type = "prob")) %>%
  bind_cols(pen_test) ->
  penAT_pred

penAT_pred %>%
  accuracy(truth = species, estimate = .pred_class)
penAT_pred %>%
  roc_auc(truth = species, estimate = .pred_Adelie)
penAT_pred %>%
  roc_curve(truth = species, estimate = .pred_Adelie) %>%
  ggplot(aes(x = 1-specificity, y = sensitivity)) +
  geom_point()

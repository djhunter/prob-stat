library(tidyverse)
library(tidymodels)

#park <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/telemonitoring/parkinsons_updrs.data")
#view(park)

#park %>%
#  select(-`subject#`, -age, -sex, -test_time, -motor_UPDRS) %>%
#  write_csv("data/speech.csv")

#Reference:
# https://doi.org/10.1109/TBME.2009.2036000
speech <- read_csv("https://djhunter.github.io/prob-stat/data/speech.csv")
glimpse(speech)

cor(speech)
library(corrplot) ## Need to install
corrplot(cor(speech))

## Train/test split
set.seed(232)
speech_split <- initial_split(speech)
speech_train <- training(speech_split)
speech_test <- testing(speech_split)

speech_recipe <- recipe(total_UPDRS ~ ., data = speech_train) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 4)

speech_recipe %>%
  prep() ->
  speech_rec_trained
speech_rec_trained

speech_rec_trained %>%
  bake(new_data = NULL)

speech_rec_trained %>%
  bake(new_data = NULL) %>%
  cor() %>%
  round(4) %>% 
  corrplot()

tidy(speech_rec_trained, 3) %>% ## Step 3 was PCA
  view() 

tidy(speech_rec_trained, 3) %>% 
  filter(component %in% c("PC1", "PC2", "PC3", "PC4")) %>%
  ggplot(aes(x = value, y = terms, fill = terms)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~component, nrow = 1)

## Challenge: Tune for an optimal value of num_comp. Start with the following 
## recipe. See shrinkage.R or your code for Assignment #16.
speech_recipe <- recipe(total_UPDRS ~ ., data = speech_train) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

## Solution:
speech_workflow <- workflow() %>%
  add_model(linear_reg()) %>%
  add_recipe(speech_recipe)

set.seed(5892)
folds <- vfold_cv(speech_train, v = 10, repeats = 5)
doParallel::registerDoParallel()
pc_grid <- tibble(num_comp = c(1:16))
speech_workflow %>% 
  tune_grid(resamples = folds, 
            grid = pc_grid) ->
  tuning_results

tuning_results %>%
  collect_metrics() %>%
  view()

tuning_results %>% 
  collect_metrics() %>%
  ggplot(aes(x = num_comp, y = mean, color = .metric)) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  geom_line()

## Compare with linear regression
finalize_workflow(speech_workflow, tibble(num_comp = 9)) %>%
  fit(speech_train) %>%
  predict(speech_test) %>%
  bind_cols(speech_test) %>%
  rmse(truth = total_UPDRS, estimate = .pred)

linear_reg() %>%
  fit(total_UPDRS ~ ., data = speech_train) %>%
  predict(speech_test) %>%
  bind_cols(speech_test) %>%
  rmse(truth = total_UPDRS, estimate = .pred)

## Compare both methods on the whole data set?

finalize_workflow(speech_workflow, tibble(num_comp = 9)) %>%
  fit(speech) %>%
  glance()

linear_reg() %>%
  fit(total_UPDRS ~ ., data = speech) %>%
  glance()

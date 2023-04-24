library(tidyverse)
wine <- read_csv("https://djhunter.github.io/prob-stat/data/wine.csv")

library(xgboost)

redwine <- wine %>% dplyr::slice(1:1599) 
trainSize <- round(0.80 * nrow(redwine))
set.seed(1234) 
trainIndex <- sample(nrow(redwine), trainSize)
trainDF <- redwine %>% dplyr::slice(trainIndex)
testDF <- redwine %>% dplyr::slice(-trainIndex)
dtrain <- xgb.DMatrix(data = as.matrix(select(trainDF, -quality)), label = trainDF$quality)
dtest <- xgb.DMatrix(data = as.matrix(select(testDF, -quality)), label = testDF$quality)

## Built-in xgboost cross validation
?xgb.cv

## Challenge/HW: White Wine
# Build an XGBoost model for the white wine data (rows 1600-6497) 
# of the wine data frame. Also build a random forest model. 
# Compare the RMSE and variable importance between the two models.

set.seed(524)
rwCV <- xgb.cv(params = list(eta = 0.1, gamma = 0), ## Note: runs in parallel?
               data = dtrain, 
               nfold = 10,
               nrounds = 500,
               early_stopping_rounds = 10,
               print_every_n = 5)
rwCV

# Try several values of eta
paramDF <- tibble(eta = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4))
split(paramDF, 1:nrow(paramDF)) ## List of rows
as.list(split(paramDF, 1:nrow(paramDF))[[1]]) ## Row 1 as a list
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list) ## List of all rows as lists

## Use a loop:

bestResults <- tibble()
set.seed(708)
pb <- txtProgressBar(style = 3) 
for(i in seq(length(paramList))) {
  rwCV <- xgb.cv(params = paramList[[i]], 
                 data = dtrain, 
                 nrounds = 500, 
                 nfold = 10,
                 early_stopping_rounds = 10,
                 verbose = FALSE)
  bestResults <- bestResults %>% 
    bind_rows(rwCV$evaluation_log[rwCV$best_iteration])
  gc() # Free unused memory after each loop iteration
  setTxtProgressBar(pb, i/length(paramList))
}
close(pb) # done with the progress bar

paramDF %>%
  bind_cols(bestResults) %>%
  arrange(test_rmse_mean) %>%
  view()

## Let's use eta = 0.1 for tuning.

## Grid search

paramDF <- expand.grid(
  max_depth = seq(15, 29, by = 2),
  max_leaves = c(63, 127, 255, 511, 1023, 2047, 4095),
  eta = 0.1)
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list) 
bestResults <- tibble()
set.seed(312)
pb <- txtProgressBar(style = 3) 
for(i in seq(length(paramList))) {
  rwCV <- xgb.cv(params = paramList[[i]], 
                 data = dtrain, 
                 nrounds = 500, 
                 nfold = 10,
                 early_stopping_rounds = 10,
                 verbose = FALSE)
  bestResults <- bestResults %>% 
    bind_rows(rwCV$evaluation_log[rwCV$best_iteration])
  gc() # Free unused memory after each loop iteration
  setTxtProgressBar(pb, i/length(paramList))
}
close(pb) # done with the progress bar
paramDF %>%
  bind_cols(bestResults) %>%
  arrange(test_rmse_mean) %>%
  view()

## Challenge (optional): Make a function called GridSearch to avoid code
# duplication. 

## Challenge: See if you can improve things by tuning subsample and colsample_bytree.

## Challenge: use dtest to test your final tuned model.

set.seed(805)
rwMod <- xgb.train(data = dtrain, verbose = 0,
                   watchlist = list(train = dtrain, test = dtest), 
                   nrounds = 10000,
                   early_stopping_rounds = 50,
                   max_depth = 21,
                   max_leaves = 4095,
                   subsample = 0.8,
                   colsample_bytree = 0.7,
                   eta = 0.05)
rwMod$evaluation_log %>% 
  pivot_longer(cols = c(train_rmse, test_rmse), names_to = "RMSE") %>% 
  ggplot(aes(x = iter, y = value, color = RMSE)) + geom_line()
print(rwMod)

## Challenge/HW: White Wine
# Build an XGBoost model for the white wine data (rows 1600-6497) 
# of the wine data frame. Also build a random forest model. 
# Compare the RMSE and variable importance between the two models.


library(tidyverse)
wine <- read_csv("https://djhunter.github.io/prob-stat/data/wine.csv")

library(xgboost)

wine <- read_csv(here("data", "wine.csv"))
redwine <- wine %>% dplyr::slice(1:1599) 
trainSize <- round(0.80 * nrow(redwine))
set.seed(1234) 
trainIndex <- sample(nrow(redwine), trainSize)
trainDF <- redwine %>% dplyr::slice(trainIndex)
testDF <- redwine %>% dplyr::slice(-trainIndex)

## Note: input format for xgboost is different
?xgboost
?xgb.DMatrix
## No categorical predictors!

dtrain <- xgb.DMatrix(data = as.matrix(select(trainDF, -quality)), label = trainDF$quality)
dtest <- xgb.DMatrix(data = as.matrix(select(testDF, -quality)), label = testDF$quality)

redwineXGB <- xgb.train(data = dtrain, nrounds = 10)

pQuality <- predict(redwineXGB, dtest)
errors <- pQuality - testDF$quality
sqrt(mean(errors^2)) #RMSE

redwineXGB <- xgb.train(data = dtrain, watchlist = list(test = dtest), nrounds = 10)
redwineXGB$evaluation_log %>%
  ggplot(aes(x = iter, y = test_rmse)) +
  geom_line()

## Challenge: Experiment with different values of nrounds in the above call to xgb.train. 
# Does the accuracy of the model improve with more iterations? 
# Is there a point after which the model ceases to improve?

## Learning Rate (eta)

redwineXGB <- xgb.train(data = dtrain, 
                        params = list(eta = 0.3),
                        watchlist = list(test = dtest), 
                        nrounds = 1000,
                        early_stopping_rounds = 10,
                        print_every_n = 5)

## Challenge: Experiment with different values of eta in the above call to xgb.train. 
# Notice how smaller values of eta require more iterations. 
# Can you find a value of eta that results in a lower testing set RMSE than the default?

## Variable Importance

xgb.importance(model = redwineXGB)

## Training error vs. Testing Error

redwineXGB <- xgb.train(data = dtrain, 
                        params = list(eta = 0.1),
                        watchlist = list(train = dtrain, test = dtest), 
                        nrounds = 1000,
                        early_stopping_rounds = 10,
                        print_every_n = 15)

redwineXGB$evaluation_log %>% 
  pivot_longer(cols = c(train_rmse, test_rmse), names_to = "RMSE") %>% 
  ggplot(aes(x = iter, y = value, color = RMSE)) + geom_line()

## Challenge: White Wine
# Build an XGBoost model for the white wine data (rows 1600-6497) 
# of the wine data frame. Also build a random forest model. 
# Compare the RMSE and variable importance between the two models.






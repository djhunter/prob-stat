library(tidyverse)
wine <- read_csv("https://djhunter.github.io/prob-stat/data/wine.csv")
glimpse(wine)

ggplot(wine, aes(x = quality)) + geom_histogram(binwidth = 1)

## Classification Model for Red Wine

redwineClass <- wine %>%
  slice(1:1599) %>%  # just the red wine samples
  mutate(grade = as_factor(if_else(quality < 5.5, "bad", "good"))) %>%  
  select(-quality) # get rid of the quality variable
summary(redwineClass$grade)

trainSize <- round(0.80 * nrow(redwineClass))
set.seed(1234) 
trainIndex <- sample(nrow(redwineClass), trainSize)
trainDF <- redwineClass %>% slice(trainIndex)
testDF <- redwineClass %>% slice(-trainIndex)

## Challenge: Use the rpart function to create a decision tree model 
# for predicting the grade variable from the remaining columns in 
# the redwineClass data frame. Use the training and testing sets 
# defined above. Compute the testing set accuracy.

## Random Forest

library(randomForest)
set.seed(4567)
redwineForest <- randomForest(grade ~ ., data = trainDF)

rwpred2 <- predict(redwineForest, testDF)
head(rwpred2)

## Challenge:
# Compute the accuracy and compare to the above tree.

## Out of bag error
print(redwineForest)

?randomForest

## Challenge:
# 1. Try different values of mtry. Does it affect OOB error?
# 2. Try more trees.
randomForest(grade ~ ., data = trainDF, ntree = 1000)
randomForest(grade ~ ., data = trainDF, mtry = 2)

## Variable Importance

importance(redwineForest)

importance(redwineForest) %>%
  as_tibble(rownames = "Variable") %>%
  arrange(desc(MeanDecreaseGini))

## Red Wine Regression Model
redwine <- wine %>% slice(1:1599) 
trainSize <- round(0.80 * nrow(redwine))
set.seed(1234) 
trainIndex <- sample(nrow(redwine), trainSize)
trainDF <- redwine %>% slice(trainIndex)
testDF <- redwine %>% slice(-trainIndex)

## Decision tree for regression

library(rpart)
library(rpart.plot)
rwtree <- rpart(quality ~ ., data = trainDF, method = "anova")
rpart.plot(rwtree)

predictedQuality <- predict(rwtree, testDF)
head(predictedQuality)
errors <- predictedQuality - testDF$quality
decTreeRMSE <- sqrt(mean(errors^2))
decTreeRMSE

## Challenge: Make a random forest regression model. Compute the RMSE. 
## Compare with the OOB RMSE. Investigate variable importance.

## Challenge: Try a linear regression model. Who wins?

## Challenge: White wine are rows 1600-6497. Are the same variables important?

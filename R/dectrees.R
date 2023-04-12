library(tidyverse)
set.seed(456)
exam <- tibble(score = sample(80:100, 200, replace = TRUE)) %>%
  mutate(grade = as_factor(ifelse(score < 90, "B", "A")))
head(exam)
summary(exam)

library(rpart)
library(rpart.plot)
examTree <- rpart(grade ~ score, data = exam)
rpart.plot(examTree)

rpart.plot(examTree, extra = 2)

glimpse(kyphosis)

# Challenge:
# Use the rpart function to create a decision tree using the kyphosis data set. 
# As in the previous episode, the response variable is Kyphosis, and the 
# explanatory varables are the remaining columns Age, Number, and Start.
# 
# 1. Use rpart.plot to plot your tree model.
# 2. Use this tree to predict the value of Kyphosis when 
#    Start is 12, Age is 59, and Number is 6.
# 3. How many of the 81 cases in the data set does this tree classify 
#    incorrectly?
  
## Supervised learning

trainSize <- round(0.75 * nrow(kyphosis))
set.seed(6789) # same seed as in the last episode
trainIndex <- sample(nrow(kyphosis), trainSize)
trainDF <- kyphosis %>% slice(trainIndex)
testDF <- kyphosis %>% slice(-trainIndex)

treeModel <- rpart(Kyphosis ~ Age + Number + Start, data = trainDF)
rpart.plot(treeModel, extra = 2)

## Challenge:
# What proportion of cases in the training set were classified correctly?

predict(treeModel, testDF)
predMatrix <- predict(treeModel, testDF)
predDF <- testDF %>% 
  bind_cols(predMatrix)

## Challenge:
# Compare the results in the predDF data frame with the plot of treeModel. 
# Can you explain how the model is calculating the predicted probabilites?

## Testing Set Accuracy

predDF <- predDF %>%
  mutate(Prediction = ifelse(predDF$absent > 0.5, "absent", "present"))
accuracy <- sum(predDF$Kyphosis == predDF$Prediction)/nrow(predDF)
cat("Proportion of correct predictions: ", accuracy, "\n")

## Challenge: Change the training set
# Repeat the construction of the decision tree model for the kyphosis data, 
# but experiment with different values of the random seed to obtain 
# different testing and training sets. Does the shape of the tree change? 
# Does the testing set accuracy change?

## Classification models

wine <- read_csv("https://djhunter.github.io/prob-stat/data/wine.csv")
redwine <- wine %>%
  slice(1:1599)
rwtree <- rpart(quality ~ ., data = redwine, method = "anova")
rpart.plot(rwtree)

## Challenge: experiment with different settings:
# 1. See ?rpart.control. Experiment with different values for control
#    and see how the tree is affected
# 2. See ?prune. Experiment with different values of cp and see what happens.

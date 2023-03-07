library(tidyverse)
library(ISLR2)
library(class)

glimpse(Default)
?Default

## Challenge: Make some sort of plot relating the three variables 
# balance, income, and default. (default is the response) (Bonus: 
# can you also incorporate student?)

ggplot(Default, aes(x = income, y = balance, color = default, shape = student)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_d()
summary(Default)

## Predict default from balance and income

## Make a train/test split
trainSize <- round(0.75*nrow(Default))
set.seed(2341)
trainIndex <- sample(nrow(Default), trainSize)
Dtrain <- Default %>% slice(trainIndex)
Dtest <- Default %>% slice(-trainIndex)

## Use knn from class library
?knn
Dtrain[, c(3,4)] %>%
  head(5)
Dtrain[, -c(1,2)] %>%
  head(5)
Dpred <- knn(Dtrain[,c(3,4)], Dtest[,c(3,4)], Dtrain$default, k = 2)

## Challenge: Dpred is supposed to match Dtest$default. 
# Compute the error rate.
# Try some other values of k, to try to get a lower error rate.

sum(Dpred != Dtest$default)/length(Dpred)
Dpred <- knn(Dtrain[,c(3,4)], Dtest[,c(3,4)], Dtrain$default, k = 9)
sum(Dpred != Dtest$default)/length(Dpred)
summary(Dtest$default)
75/2500
sum(Dpred == "Yes" & Dtest$default == "Yes")
sum(Dpred == "No" & Dtest$default == "Yes")
sum(Dpred == "No" & Dtest$default == "No")
sum(Dpred == "Yes" & Dtest$default == "No")


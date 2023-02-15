## How to do p. 54/7 in R

library(class)
cdf <- data.frame(x1 = c(0,2,0,0,-1,1), x2=c(3,0,1,1,0,1), x3=c(0,0,3,2,1,1), y=factor(c("Red", "Red", "Red", "Green", "Green", "Red")))
cdftest <- data.frame(x1 = 0, x2 = 0, x3 = 0)
knn(cdf[,1:3], cdftest, cdf$y, k=1, prob = TRUE)
knn(cdf[,1:3], cdftest, cdf$y, k=3, prob = TRUE)

library(ISLR2)
library(tidyverse)
library(class)
glimpse(Default)

## Challenge: Create a plot that relates balance, income, and default. 
# Can you also include student on your plot?

Default %>%
  ggplot(aes(x = balance, y = income, color = default)) +
    geom_point(alpha = 0.5)
summary(Default$default)
summary(Default$student)

Default %>%
  ggplot(aes(x = balance, y = income, color = student)) +
    geom_point(alpha = 0.5)

Default %>%
  ggplot(aes(x = balance, y = income, color = default, shape = student)) +
    geom_point(alpha = 0.5)

Default %>%
  ggplot(aes(x = balance, y = income, color = student:default)) +
    geom_point(alpha = 0.5) +
    scale_color_viridis_d()

## Use KNN to predict default from income and balance

## Train/Test split
trainSize <- round(0.75 * nrow(Default))
set.seed(2341)
trainIndex <- sample(nrow(Default), trainSize)
Dtrain <- Default %>%
  slice(trainIndex)
Dtest <- Default %>%
  slice(-trainIndex)

Dtrain[,c(3,4)] %>%
  head(5)

## k = 2
tPred <- knn(Dtrain[,c(3,4)], Dtest[,c(3,4)], Dtrain$default, k = 2)
sum(tPred != Dtest$default)/nrow(Dtest)

## Challenges
#1. Can you get a lower error rate by changing K?
#2. How many defaults were there in the testing set?
#3. How many defaults were classified incorrectly?
#4. How many non-defaults were classified incorrectly?

tPred <- knn(Dtrain[,c(3,4)], Dtest[,c(3,4)], Dtrain$default, k = 10)
sum(tPred == Dtest$default)/nrow(Dtest)

tPred <- knn(Dtrain[,c(3,4)], Dtest[,c(3,4)], Dtrain$default, k = 20)
sum(tPred == Dtest$default)/nrow(Dtest)

## Penguins

library(palmerpenguins)
penComplete <- penguins %>%
  drop_na()
trainSize <- round(0.7 * nrow(penComplete))
set.seed(12341)
trainIndex <- sample(nrow(penComplete), trainSize)
Ptrain <- penComplete %>%
  slice(trainIndex)
Ptest <- penComplete %>%
  slice(-trainIndex)

pPred <- knn(Ptrain[,c(3,4,5,6)], Ptest[,c(3,4,5,6)], Ptrain$species, k = 3)
sum(pPred != Ptest$species)/nrow(Ptest)

knn(Ptrain[,c(3,4,5,6)], penToPredict, Ptrain$species, k = 3)

## Carpentry Script for data basics

library(tidyverse)
library(ISLR2)

view(Auto)
glimpse(Auto)
?Auto

# rows/columns

## Scatterplots

ggplot(Auto, aes(x = displacement, y = mpg)) +
  geom_point()

Auto %>%
  ggplot(aes(x = displacement, y = mpg)) +
  geom_point()

Auto %>%
  filter(origin == 3) %>%
  ggplot(aes(x = displacement, y = mpg)) +
  geom_point()

Auto %>%
  ggplot(aes(x = displacement, y = mpg, color = origin)) +
  geom_point()

Auto %>%
  ggplot(aes(x = displacement, y = mpg, color = factor(origin))) +
  geom_point() +
  geom_smooth()

Auto %>%
  ggplot(aes(x = displacement, y = mpg, color = factor(origin))) +
  geom_point() +
  geom_smooth(span = 0.75, se = FALSE)

## Challenges: 
# 1. Experiment with span. What does it do? 
# 2. Eliminate american cars from the plot.
# 3. Try method = lm in geom_smooth.

Auto %>%
  filter(origin != 1) %>%
  ggplot(aes(x = displacement, y = mpg, color = factor(origin))) +
  geom_point() +
  geom_smooth(span = 0.3, se = FALSE)

Auto %>%
  filter(displacement < 200) %>%
  ggplot(aes(x = displacement, y = mpg, color = factor(origin))) +
  geom_point() +
  geom_smooth(span = 0.75, se = FALSE)

## Summarizing data

summary(Auto$mpg)
ggplot(Auto, aes(x = mpg)) + geom_histogram()

Auto %>%
  summarize(
    AveMPG = mean(mpg),
    medianMPG = median(mpg),
    minMPG = min(mpg),
    maxMPG = max(mpg)
  )

Auto %>%
  group_by(origin) %>%
  summarize(
    AveMPG = mean(mpg),
    medianMPG = median(mpg),
    minMPG = min(mpg),
    maxMPG = max(mpg)
  )

which.min(Auto$mpg)
Auto[which.min(Auto$mpg),]

Auto %>%
  arrange(mpg) %>%
  head(5)

Auto %>%
  mutate(carNum = row_number()) %>%
  arrange(mpg) %>%
  head(5)

## Challenges:
# 1. Which car has the most horsepower?
# 2. Compare the horsepower of 8 cylinder cars to 4 cylinder cars.
# 3. Add a column HPperC, that calculates horsepower per cylinder, and 
#    find the top 5 cars.


## Tidy solution to Exercise 10, page 57

library(tidyverse)
library(ISLR2)

view(Boston)

ggplot(Boston, aes(x = dis, y = crim)) +
  geom_point()

ggplot(Boston, aes(x = medv, y = crim)) +
  geom_point()

ggplot(Boston, aes(x = medv, y = crim, color = factor(chas))) +
  geom_point(alpha = 0.4)

ggplot(Boston, aes(x = crim)) + geom_histogram()
summary(Boston$crim)

ggplot(Boston, aes(x = tax)) + geom_histogram()
summary(Boston$tax)

ggplot(Boston, aes(x = ptratio)) + geom_histogram()
summary(Boston$ptratio)

summary(Boston$chas)
summary(factor(Boston$chas))
sum(Boston$chas == 1)
Boston %>%
  summarize(
    numOnRiver = sum(chas == 1)
  )
Boston %>%
  summarize(
    MinCrime = min(crim),
    MaxCrime = max(crim),
    MinTax = min(tax),
    MaxTax = max(tax),
    MinPTR = min(ptratio),
    MaxPTR = max(ptratio),
  )

which.min(Boston$medv)
Boston[which.min(Boston$medv),]

Boston %>%
  mutate(tractNum = row_number()) %>%
  top_n(5, -medv)

Boston %>% 
  filter(rm >= 7)

Boston %>% 
  filter(rm >= 7) %>%
  summarize(
    numTracts = n(),
    meanCrime = mean(crim),
    medianCrime = median(crim)
    )

Boston %>% 
  filter(rm >= 8) %>%
  summarize(
    numTracts = n(),
    meanCrime = mean(crim),
    medianCrime = median(crim)
    )


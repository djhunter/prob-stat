library(tidyverse)
library(ISLR2)

## Inspect Auto data set
Auto
glimpse(Auto)
view(Auto)
?Auto

## Scatterplots

ggplot(Auto, aes(x = displacement, y = mpg)) +
  geom_point()

ggplot(Auto, aes(x = displacement, y = mpg, shape = factor(origin))) +
  geom_point()

AutoNoah <- Auto %>%
  mutate(OriginCountry = factor(case_when(
    origin == 1 ~ "US",
    origin == 2 ~ "Europe",
    origin == 3 ~ "Japan"
  )))
view(AutoNoah)

ggplot(AutoNoah, aes(x = displacement, y = mpg, color = OriginCountry)) +
  geom_point()

AutoNoah %>%
  ggplot(aes(x = displacement, y = mpg, color = OriginCountry)) +
    geom_point()

AutoNoah %>%
  filter(OriginCountry != "US") %>%
  ggplot(aes(x = displacement, y = mpg, color = OriginCountry)) +
  geom_point() +
  geom_smooth(se = FALSE, span = 0.75)

## Challenge:
#1. Experiment with different spans.  What does it do?
#2. Try method = lm in geom_smooth.

## Histograms
Auto %>%
  ggplot(aes(x = mpg)) +
  geom_histogram()
AutoNoah %>%
  ggplot(aes(x = mpg, fill = OriginCountry)) +
  geom_histogram(binwidth = 5, position = "dodge")

## More data wrangling

## Making data summaries

AutoNoah %>%
  group_by(OriginCountry) %>%
  summarize(
    AveMPG = mean(mpg),
    medianMPG = median(mpg),
    minMPG = min(mpg),
    maxMPG = max(mpg)
  )

AutoNoah <- AutoNoah %>%
  mutate(HPperDISP = horsepower/displacement)

AutoNoah %>%
  arrange(-HPperDISP) %>%
  view()
AutoNoah %>%
  arrange(-HPperDISP) %>%
  head(7)

Auto[which(Auto$mpg == 9),]
which.max(AutoNoah$HPperDISP)

## Challenge:
# 1. Which car has the most horsepower?
# 2. Compare the horsepower of 8 cyl to 4 cyl cars.

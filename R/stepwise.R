library(tidyverse)
library(tidymodels)
library(ISLR2)

glimpse(Hitters)

fullMod <- lm(Salary ~ ., data = Hitters)
summary(fullMod)

hit_complete <- Hitters %>%
  drop_na()

fullMod <- lm(Salary ~ ., data = hit_complete)
summary(fullMod)

## Backward selection (default)
step(fullMod)

## Challenge: Why are we getting this error? (Hint: try ?step)
## Solution: 
stats::step(fullMod)
backStep <- stats::step(fullMod)
summary(backStep)

## Forward selection

nullMod <- lm(Salary ~ 1, data = hit_complete)
summary(nullMod)

?step
stats::step(nullMod, direction = "forward")

fullForm <- formula(fullMod)
fullForm

stats::step(nullMod, scope = fullForm, direction = "forward")
forwardStep <- stats::step(nullMod, scope = fullForm, direction = "forward")

formula(backStep)
formula(forwardStep)

## Challenge: Try the hybrid version, starting with Salary ~ RBI
## Solution:
startMod <- lm(Salary ~ RBI, data = hit_complete)
summary(startMod)
hybridStep <- stats::step(startMod, scope = list(lower = formula(nullMod), upper = (fullMod)))
formula(hybridStep)
formula(backStep)
summary(hybridStep)

set.seed(953)
boots <- bootstraps(hit_complete, times = 10)

step_on_boot <- function(split) {
  fullMod <- lm(Salary ~ ., data = analysis(split))
  return(formula(stats::step(fullMod, trace = 0)))
}

step_on_boot(boots$splits[[1]])
step_on_boot(boots$splits[[2]])
map(boots$splits, step_on_boot)
## gives lots of different models!


library(tidyverse)
library(tidymodels)
library(ISLR2)

glimpse(Hitters)

## Goal: predict salary from other variables (p = 19)

fullMod <- lm(Salary ~ ., data = Hitters) ## dot means "everything else"
summary(fullMod)

hit_complete <- Hitters %>%
  drop_na()

fullMod <- lm(Salary ~ ., data = hit_complete)
summary(fullMod)
AIC(fullMod)
BIC(fullMod)

## Backward selection
step(fullMod)
## Challenge: Diagnose the error. (Hint: ?step)
?step
stats::step(fullMod)
backwardModel <- stats::step(fullMod)
summary(backwardModel)
summary(fullMod)

## Forward selection
nullMod <- lm(Salary ~ 1, data = hit_complete)
summary(nullMod)
stats::step(nullMod, direction = "forward")
formula(fullMod)
forwardModel <- stats::step(nullMod, direction = "forward", scope = formula(fullMod))
formula(forwardModel)
formula(backwardModel)

## Challenge: Do hybrid forward/backward, start with Salary ~ RBI
RBImod <- lm(Salary ~ RBI, data = hit_complete)
hybridModel <- stats::step(RBImod, direction = "both", scope = formula(fullMod))
hybridModel <- stats::step(RBImod, direction = "both", 
                           scope = list(lower = formula(nullMod),
                                        upper = formula(fullMod)))
formula(hybridModel)
# Note: ending model is different


set.seed(161)
boots <- bootstraps(hit_complete, times = 10)

step_on_boot <- function(split) {
  sMod <- lm(Salary ~ ., data = analysis(split))
  return(formula(stats::step(sMod, trace = 0)))
}

step_on_boot(boots$splits[[1]])
step_on_boot(boots$splits[[2]])

map(boots$splits, step_on_boot)

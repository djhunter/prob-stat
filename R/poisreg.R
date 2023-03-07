library(tidyverse)
library(palmerpenguins)
set.seed(321)
penCoin <- penguins %>%
  drop_na() %>%
  mutate(coins = round(exp(bill_length_mm * 0.05 + bill_depth_mm * 0.02)) + rpois(n(), 1) -1)
view(penCoin)

## Challenge: fit two models: linear regression and poisson regression.
# predict coins from bill_length_mm, bill_depth_mm, flipper_length_mm
# Compare

coinMod1 <- lm(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = penCoin)
summary(coinMod1)
coinMod2 <- glm(coins ~ bill_length_mm + bill_depth_mm + flipper_length_mm, 
                data = penCoin, family = poisson)
summary(coinMod2)

testDF <- data.frame(bill_length_mm = c(30,35,45,50),
                     bill_depth_mm = c(13,15,19,21),
                     flipper_length_mm = c(200,210,220,230))
view(testDF)
predict(coinMod1, testDF)
predict(coinMod2, testDF, type = "response")
exp(testDF$bill_length_mm*.05 + testDF$bill_depth_mm*0.02)

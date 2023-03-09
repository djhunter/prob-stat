library(tidyverse)
library(tidymodels)
library(palmerpenguins)

pen_complete <- penguins %>%
  filter(!is.na(body_mass_g))

ggplot(pen_complete, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

oldPMod <- lm(body_mass_g ~ flipper_length_mm * species, data = pen_complete)
summary(oldPMod)

## Make bootstrap resamples
set.seed(234)
boots <- bootstraps(pen_complete, 1000)

## Challenge: What is boots? Investigate.
glimpse(boots)
str(boots)
boots$splits
boots$splits[[1]]
str(boots$splits[[1]])
class(boots$splits[[1]])
?analysis
analysis(boots$splits[[1]])

## Create a function to fit the model on a split
fit_lm_on_boot <- function(split) {
  lm(body_mass_g ~ flipper_length_mm * species, data = analysis(split))
}

boot_models <- boots %>%
  mutate(model = map(splits, fit_lm_on_boot),
         coef_info = map(model, tidy))
boot_models

boot_coefs <- boot_models %>%
  unnest(coef_info)
glimpse(boot_coefs)

boot_coefs %>%
  select(id, term, estimate) %>%
  view()
summary(oldPMod)

boot_coefs %>%
  select(id, term, estimate) %>%
  group_by(term) %>%
  summarize(SEboot = sd(estimate))

int_pctl(boot_models, coef_info)
confint(oldPMod)
plot(oldPMod)

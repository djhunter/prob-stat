library(tidyverse)
library(tidymodels)
library(palmerpenguins)
pen_complete <- penguins %>%
  filter(!is.na(flipper_length_mm),
         !is.na(body_mass_g),
         !is.na(species))
pModTheory <- lm(body_mass_g ~ flipper_length_mm * species, data = pen_complete)

summary(pModTheory)
confint(pModTheory)

ggplot(pen_complete, aes(x = flipper_length_mm, y=body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

## Make bootstrap resamples

set.seed(234)
boots <- bootstraps(pen_complete, 1000)

## Challenge: What kind of object is boots? Investigate it.

# Solution:
boots
boots$splits[[1]]
str(boots$splits[[1]])
class(boots$splits[[1]])
?analysis
analysis(boots$splits[[1]])

# Create a function to fit a model on a given split
fit_lm_on_boot <- function(split) {
  lm(body_mass_g ~ flipper_length_mm * species, data = analysis(split))
}

boot_models <- boots %>%
  mutate(model = map(splits, fit_lm_on_boot),
         coef_info = map(model, tidy))

boot_models
# Nested tibble (tibble of tibbles)

boot_coefs <- boot_models %>%
  unnest(coef_info)
boot_coefs
glimpse(boot_coefs)

boot_coefs %>%
  select(term, estimate, std.error) %>%
  group_by(term) %>%
  summarize(SEboot = sd(estimate))
summary(pModTheory)$coefficients

?int_pctl
int_pctl(boot_models, coef_info)
confint(pModTheory)

## Challenge: Do exercise 6, page 221 this way.



## Bootstraps and t-tests using infer package

# bees <- data.frame(
#   species = factor(c("emf", "emf", "emf", "emf",
#                      "eic", "eic", "eic", "eic", "eic", "eic")),
#   wing_speed = c(235, 225, 190, 188,
#                  180, 169, 180, 185, 178, 183)
# )
# write_csv(bees, "data/bees.R")

## Read bees data from repo
bees <- read_csv("https://djhunter.github.io/prob-stat/data/bees.R") %>%
  mutate(species = factor(species))
bees

## Intercept model:
intMod <- lm(wing_speed ~ 1, data = bees)
summary(intMod)
confint(intMod)

## Same as t-interval
t.test(bees$wing_speed)

## tidymodels way

bees %>%
  specify(response = wing_speed) %>%
  calculate(stat = "mean") ->
  xBar

bees %>%
  specify(response = wing_speed) %>%
  assume("t") %>%
  get_confidence_interval(point_estimate = xBar)

bees %>%
  t_test(response = wing_speed) 

## Use bootstrap instead

bees %>%
  specify(response = wing_speed) %>%
  generate(reps = 10000, type = "bootstrap") %>%
  calculate(stat = "mean") %>%
  get_confidence_interval()

## regression model is the same as t-test, assuming equal variance!
difMod <- lm(wing_speed ~ species, data = bees)
t.test(wing_speed ~ species, data = bees, var.equal = TRUE)
t.test(wing_speed ~ species, data = bees, var.equal = FALSE)

bees %>%
  t_test(formula = wing_speed ~ species, var.equal = TRUE) 
bees %>%
  t_test(formula = wing_speed ~ species) 

bees %>%
  specify(wing_speed ~ species) %>%
  calculate(stat = "diff in means") ->
  diffxBar

bees %>%
  specify(wing_speed ~ species) %>%
  assume("t") %>%
  get_confidence_interval(point_estimate = diffxBar)

set.seed(654)
bees %>%
  specify(wing_speed ~ species) %>%
  generate(reps = 10, type = "bootstrap") %>%
  calculate(stat = "diff in means") %>%
  get_confidence_interval()

## Challenge: get the warnings and errors to stop
bees %>%
  specify(wing_speed ~ species) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means") %>%
  get_confidence_interval()

## Solution
bees %>%
  specify(wing_speed ~ species) %>%
  generate(reps = 10000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("eic", "emf")) %>%
  drop_na() %>%
  get_confidence_interval()

## More general approach (slower)

bboots <- bees %>%
  bootstraps(times = 10000)

meanspeed <- function(x) {
  mean(analysis(x)$wing_speed)
}
    
map_dbl(bboots$splits, meanspeed) %>%
  quantile(c(0.025, 0.975))

fit_int_model <- function(split) {
  lm(wing_speed ~ 1, data = analysis(split))
}

fit_diff_model <- function(split) {
  lm(wing_speed ~ species, data = analysis(split))
}

bboot_models <- bboots %>%
  mutate(model = map(splits, fit_int_model),
         coef_info = map(model, tidy))
bboot_coefs <- bboot_models %>%
  unnest(coef_info)
bboot_coefs %>%
  select(term, estimate, std.error) %>%
  group_by(term) %>%
  summarize(SEboot = sd(estimate))
int_pctl(bboot_models, coef_info)

## Challenge: Why are we getting an error?
bboot_models <- bboots %>%
  mutate(model = map(splits, fit_diff_model),
         coef_info = map(model, tidy))
bboot_coefs <- bboot_models %>%
  unnest(coef_info)
bboot_coefs %>%
  select(term, estimate, std.error) %>%
  group_by(term) %>%
  summarize(SEboot = sd(estimate))
int_pctl(bboot_models, coef_info)

## Try again with strata
bboots <- bees %>%
  bootstraps(times = 10000, strata = species)
bboot_models <- bboots %>%
  mutate(model = map(splits, fit_diff_model),
         coef_info = map(model, tidy))
bboot_coefs <- bboot_models %>%
  unnest(coef_info)
bboot_coefs %>%
  select(term, estimate, std.error) %>%
  group_by(term) %>%
  summarize(SEboot = sd(estimate))
int_pctl(bboot_models, coef_info)
t.test(wing_speed ~ species, data = bees)


library(tidyverse)
library(ISLR2)
glimpse(Credit)
cMod1 <- lm(Balance ~ Income, data = Credit)
summary(cMod1)
cMod2 <- lm(Balance ~ Income + Student, data = Credit)
summary(cMod2)
ggplot(Credit, aes(x = Income, y = Balance, color = Student)) +
  geom_point() +
  geom_smooth(method = lm)
cMod3 <- lm(Balance ~ Income * Student, data = Credit)
summary(cMod3)

library(palmerpenguins)
ggplot(penguins, aes(x = flipper_length_mm, y=body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
glimpse(penguins)
penguins %>%
  filter(is.na(flipper_length_mm))
pen_complete <- penguins %>%
  filter(!is.na(flipper_length_mm),
         !is.na(body_mass_g),
         !is.na(species))
ggplot(pen_complete, aes(x = flipper_length_mm, y=body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
## Challenges
#1. Predict body mass from flipper length and species, with and without interactions.
#2. Use filter to do a single-variable model for each species. Compare? 
pen_complete %>%
  lm(body_mass_g ~ flipper_length_mm + species, data = .) %>%
  summary()
pen_complete %>%
  lm(body_mass_g ~ flipper_length_mm * species, data = .) %>%
  summary()
pen_complete %>%
  filter(species == "Adelie") %>%
  lm(body_mass_g ~ flipper_length_mm, data = .) %>%
  summary()
pen_complete %>%
  filter(species == "Chinstrap") %>%
  lm(body_mass_g ~ flipper_length_mm, data = .) %>%
  summary()
pen_complete %>%
  filter(species == "Gentoo") %>%
  lm(body_mass_g ~ flipper_length_mm, data = .) %>%
  summary()

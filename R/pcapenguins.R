library(tidyverse)
library(palmerpenguins)
glimpse(penguins)

pen_num <- penguins %>%
  drop_na() %>%
  select(where(is.numeric))
# pen.pca <- prcomp(pen_num) # Do first; what's wrong?
pen.pca <- prcomp(pen_num, scale. = TRUE)
summary(pen.pca)
pen.pca
## Challenge: Examine each of the first 3 components. What are they
## telling us?

## Note: center and scale manually
pen_num_scaled <- scale(pen_num)
pen.pca2 <- prcomp(pen_num_scaled, center = FALSE)
pen.pca2 ## Same result

## Challenge: Locate the rotation matrix. Can you multiply it by the 
## data to get the scores?

plot(pen.pca)
plot(pen.pca, type = "line")
biplot(pen.pca)

## Challenge: There seem to be three (maybe six) groupings in the biplot. 
## Can you figure out what they are?
## Hint: attach data to scores
pen_complete <- penguins %>%
  drop_na()
library(broom)
pen_scores <- augment(pen.pca, pen_complete) 
view(pen_scores)

## Challenge: Take away year and redo. Now what groupings are found?


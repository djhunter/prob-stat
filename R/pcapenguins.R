library(tidyverse)
library(palmerpenguins)
glimpse(penguins)
penguins %>%
  select(where(is.numeric)) %>%
  select(-year) %>%
  drop_na() %>%
  prcomp(scale. = TRUE) ->
  pen.pca
summary(pen.pca)

penguins %>%
  filter(species == "Gentoo") %>%
  select(where(is.numeric)) %>%
  select(-year) %>%
  drop_na() %>%
  prcomp(scale. = TRUE) ->
  gentoo.pca
summary(gentoo.pca)
gentoo.pca

penguins %>%
  filter(species != "Gentoo") %>%
  select(where(is.numeric)) %>%
  select(-year) %>%
  drop_na() %>%
  prcomp(scale. = TRUE) ->
  small.pca
summary(small.pca)
small.pca
biplot(small.pca)

penguins %>%
  select(where(is.numeric)) %>%
  select(-year) %>%
  drop_na() ->
  pen.numeric

pen.pca <- prcomp(pen.numeric, scale. = TRUE) 
summary(pen.pca)

pen.pca$x * pen.pca$scale + pen.pca$center
as.matrix(pen.numeric) %*% pen.pca$rotation

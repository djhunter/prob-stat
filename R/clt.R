library(ISLR2)
library(tidyverse)

# data frames in R
glimpse(Bikeshare)
view(Bikeshare)

# ggplot syntax
ggplot(Bikeshare, aes(x = bikers)) +
  geom_histogram()

ggplot(Bikeshare, aes(x = bikers)) +
  geom_boxplot()

ggplot(Bikeshare, aes(x = bikers)) +
  geom_density()

ggplot(Bikeshare, aes(x = bikers, color = weathersit)) +
  geom_density()

levels(Bikeshare$weathersit)
sum(Bikeshare$weathersit == "heavy rain/snow")

## Construct a data frame
repSize <- 100000
simPts <- tibble() # make a blank data frame
for (n in c(1, 2, 5, 10, 20)) {
  simPts <- simPts %>% 
    bind_rows(
      tibble(xbar = replicate(repSize, mean(runif(n))),
             sample_size = factor(rep(n, repSize)))
    )
}

## Challenge: Make some plots. Compare the distributions. Try boxplots and density
# plots, and histograms if you want.

ggplot(simPts, aes(x = xbar, color = sample_size)) +
  geom_density(adjust = 2)

ss <- 20
simPts %>%
  filter(sample_size == ss) %>%
  ggplot(aes(x = xbar)) + geom_density(adjust=1.5) +
  stat_function(fun = dnorm,
                args = list(mean = 0.5,
                            sd = sqrt(1/12/ss)),
                color = "purple")

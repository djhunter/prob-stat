set.seed(1234)
pwinnings <- replicate(1000000, sum(sample(c(-1,1), 100, replace = TRUE)))
probs <- table(abs(pwinnings))/1000000
plot(probs)
mean(abs(pwinnings))
sqrt(mean(pwinnings^2)) # should be sqrt of number of steps

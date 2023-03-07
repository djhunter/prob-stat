n <- 10
runif(n)
mean(runif(n)) # X-bar

## Simulate the distribution of X-bar
n <- 3000
n <- 16667
xBars <- replicate(10000, mean(runif(n)))
hist(xBars)

## Challenge: Find the smallest n that gets you 95% within 0.01 of 0.5
quantile(xBars, c(0.025, 0.975))


##  Monte Carlo integrals revisited

g <- function(x) {exp(-x^2)}
# Old way: random darts
n <- 100
x <- runif(n)
y <- runif(n)
sum(y < g(x))/n

# New way: ave over a sample
X <- runif(n)
mean(g(X))

# Actual Answer
integrate(g, 0, 1)

## p = 0.95, epsilon = 0.01
n <- 1/(0.01)^2/(0.05)

approxInts <- replicate(10000, mean(g(runif(n))))
hist(approxInts)

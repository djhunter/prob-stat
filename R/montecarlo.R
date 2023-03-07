## Monte Carlo estimation of an integral

set.seed(4365)
n <- 1000000 # number of points
x <- runif(n)
y <- runif(n)
sum(x^2 > y)/n

## Buffon's Needle simulation

set.seed(5367)
n <- 1000000
d <- runif(n, min = 0, max = 0.5)
theta <- runif(n, min = 0, max = pi/2)
sum(d < 0.5*sin(theta))/n


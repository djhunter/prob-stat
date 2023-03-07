## n choose k

choose(3, 2)
choose(4,2)

# binomial distribution function has 2 parameters: n, p and 1 input: k

bin <- function(n, p, k) {
  choose(n, k) * p^k * (1 - p)^(n-k)
}
bin(3, 0.5, 2)

## Challenge: Do the Poisson
factorial(4)
exp(1) # e^1

poi <- function(lambda, k) {
  lambda^k/factorial(k)*exp(-lambda)
}
poi(1.5, 2)
bin(3, 0.5, 2)
bin(100, 0.1, 4)
poi(10, 4)
bin(100, 0.1, 0:100)
plot(bin(100, 0.1, 0:100))
plot(poi(10, 0:100))

## Standard normal PDF

f <- function(x) {
  1/sqrt(2*pi) * exp(-x^2/2)
}
integrate(f, -1, 1)
integrate(f, -2, 2)
integrate(f, -3, 3)
integrate(f, -Inf, Inf)
integrate(f, -Inf, 1.645) # CDF (left tail)
pnorm(1.645)
?pnorm

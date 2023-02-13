## How to do p. 54/7 in R

library(class)
cdf <- data.frame(x1 = c(0,2,0,0,-1,1), x2=c(3,0,1,1,0,1), x3=c(0,0,3,2,1,1), y=factor(c("Red", "Red", "Red", "Green", "Green", "Red")))
cdftest <- data.frame(x1 = 0, x2 = 0, x3 = 0)
knn(cdf[,1:3], cdftest, cdf$y, k=1, prob = TRUE)
knn(cdf[,1:3], cdftest, cdf$y, k=3, prob = TRUE)

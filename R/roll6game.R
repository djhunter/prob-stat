totwin <- replicate(10000, {
  winnings <- 1
  while(sample(c(1,2,3,4,5,6), 1) != 6) {
  winnings <- winnings + 1
  }
  winnings
})
probs <- table(totwin)/10000
plot(probs)
mean(totwin)

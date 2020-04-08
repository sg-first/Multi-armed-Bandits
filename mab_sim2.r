# multi-armed bandit simulator
#
# 3 machines, each return the same ammount, but with different chances of winning
# you get to play 100 times, try and maximise the number of wins

# assume we use a strategy of 
# a) playing each machine k times,
# b) choosing a machine based on estimates of the winning probabilities,
# c) using that machine for our remaining plays.
# what is the best choice of k?

mab <- function(k) {
  p <- runif(3)
  phat <- sapply(p, function(x) rbinom(1, k, x))
  i <- which.max(phat)               
  return(sum(phat) + rbinom(1, 100-3*k, p[i]))
}

nrep <- 10000
for (k in 1:20) {
  cat("k =", k, "mean return =", mean(replicate(nrep, mab(k))), "\n")
}
choiceA <- function(alpha, beta, t) {
  # just choose bandit 1
  return(1)
}

choiceB <- function(alpha, beta, t) {
  # Thompson Sampling
  n <- length(alpha)
  theta <- rbeta(n, alpha, beta)
  return(which.max(theta))
}

choiceC <- function(alpha, beta, t) {
  # max expected return
  return(which.max(alpha/(alpha + beta)))
}

choiceD <- function(alpha, beta, t) {
  # eps Greedy
  n <- length(alpha)
  if (runif(1) < 1/t) {
    return(sample(n, 1))
  } else {
    return(which.max(alpha/(alpha + beta)))
  }
}

choiceE <- function(alpha, beta, t) {
  # Hoeffding-based Upper Confidence Bound 
  return(which.max(alpha/(alpha + beta) + sqrt(2*log(t)/(alpha + beta))))
}

choiceF <- function(alpha, beta, t) {
  # Bayes Percentage Point
  return(which.max(qbeta(0.95, alpha, beta)))
}

regret <- function(choice, n, Ti) {
  # choice is a function of alpha, beta and t
  #   alpha and beta are length n, give dist of theta_i's; t is time
  #   returns 1 <= i <= n
  #
  # given n bandits generates n success probabilities at random
  # then applies choice Ti times
  #
  # returns regret at times 0, 1, 2, ..., Ti
  theta <- rbeta(n, 1, 1)
  max_theta <- max(theta)
  alpha <- rep(1, n)
  beta <- rep(1, n)
  regret <- rep(0, Ti+1)
  for (ti in 1:Ti) {
    i <- choice(alpha, beta, ti)
    xi <- rbinom(1, 1, theta[i])
    alpha[i] <- alpha[i] + xi
    beta[i] <- beta[i] + 1 - xi
    regret[ti+1] <- regret[ti] + max_theta - xi 
  }
  return(regret)
}

regret_avg <- function(choice, n, Ti, n_rep=1000) {
  # repeats regret(choice, n, Ti) n_rep times and averages
  regret_matrix <- matrix(0, nrow = Ti+1, ncol = n_rep)
  for (i in 1:n_rep) {
    regret_matrix[,i] <- regret(choice, n, Ti)
  }
  return(rowMeans(regret_matrix))
}

plot(regret_avg(choiceA, 10, 400))
points(regret_avg(choiceB, 10, 400), col="red")    # Thompson
points(regret_avg(choiceC, 10, 400), col="orange") # max expected return
points(regret_avg(choiceD, 10, 400), col="green")  # eps-greedy
points(regret_avg(choiceE, 10, 400), col="blue")   # Hoeffding
points(regret_avg(choiceF, 10, 400), col="purple") # Bayes

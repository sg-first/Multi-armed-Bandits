# multi-armed bandit simulator
library(rBayesianOptimization)

machNum=10
triesNum=400

p <- runif(machNum) # do not look at p until you are finished!

winNum <- array(0:0,dim=c(machNum))
totalNum <- array(0:0,dim=c(machNum))
choice <- array(0:0,dim=c(triesNum))

wins <- 0
tries <- 0

play<-function(machine,times)
{
  lastWin<-wins
  k <- machine
  n <- min(times, triesNum - tries)
  res<-runif(n)
  wins <<- wins + sum(res < p[k])
  tries <<- tries + n
  cat(wins, "wins from", tries, "tries\n")
  return(wins-lastWin)
}

Test_Fun <- function(x) 
{
  x<-floor(x)
  if(x==11)
    x<-10
  list(Score = play(x,1),
       Pred = 0)
}
OPT_Res <- BayesianOptimization(Test_Fun,
                                bounds = list(x = c(1, machNum+1)),
                                init_points = 2, n_iter = triesNum,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)
cat('\n',p)

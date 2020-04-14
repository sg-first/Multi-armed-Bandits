# multi-armed bandit simulator

machNum=10 # Number of machine
triesNum=500 # Number of attempts

p <- runif(machNum) # do not look at p until you are finished!

winNum <- array(0:0,dim=c(machNum)) # Number of victories per machine
totalNum <- array(0:0,dim=c(machNum)) # Number of attempts per machine
choice <- array(0:0,dim=c(triesNum)) # which machine is chosen each time

wins <- 0 # Number of victories already
tries <- 0 # Number of attempts already

play<-function(machine,times) # Choose to play a machine a few times
{
  lastWin<-wins
  k <- machine
  n <- min(times, triesNum - tries)
  res<-runif(n)
  wins <<- wins + sum(res < p[k])
  tries <<- tries + n
  cat(wins, "wins from", tries, "tries\n")
  return(wins-lastWin) # Returns the number of victories of these attempts
}

getBayP<-function(mach) # Get the victory frequency of a machine
{
  if(totalNum[mach]!=0)
  {
    winNum[mach]/totalNum[mach]
  }
  else
  {
    0.5
  }
}

UCB<-function(mach) # Get the UCB score for a machine
{
  if(totalNum[mach]==0 || tries==0)
    return(0.5)
  else
  {
    r1 <- (tries/triesNum) * getBayP(mach) # explore
    r2 <- ((triesNum-tries)/triesNum)/5 * sqrt(2*log(tries)/totalNum[mach]) # using
    return(r1+r2)
  }
}

chooseTS <- function() # Select a machine by Thompson sampling
{
  alpha<-winNum
  beta<-totalNum
  # Thompson Sampling
  n <- length(alpha)
  theta <- rbeta(n, alpha, beta) # Beta distribution is defined in terms of frequency
  maxSub<-which(theta==max(theta),arr.ind=TRUE) # Find the subscript with the maximum sample value (corresponding to the machine number)
  cho<-maxSub[rand(1,length(maxSub))] # If there are more than one with the same value, pick one at random
  choice[tries+1]<<-cho # Record the selection
  return(cho)
}

rand<-function(min,max) {floor(runif(1,min,max+1))} # Pick a random number in the range

randChoose<-function(){rand(1,machNum)} # Select a machine by random
chooseUCB<-function() # Select a machine by UCB
{
  machScore <- array(0:0,dim=c(machNum))
  for(n in 1:machNum)
  {
    machScore[n] <- UCB(n) # Calculate the UCB score for each machine
  }
  # Choose the machine with the highest rating.Same as above
  maxSub<-which(machScore==max(machScore),arr.ind=TRUE)
  cho<-maxSub[rand(1,length(maxSub))]
  choice[tries+1]<<-cho
  return(cho)
}

chooseGreedy<-function() # Select a machine by e-Greedy
{
  machScore <- array(0:0,dim=c(machNum))
  allMach <- array(1:machNum)
  for(n in allMach)
  {
    machScore[n] <- getBayP(n) # Gets the victory frequency of each machine
  }
  maxP=max(machScore) # Find the maximum frequency
  res<-runif(n)
  if(res<maxP[1]) # If the random number is greater than the frequency value, choose randomly
  {
    maxSub<-which(machScore==max(machScore),arr.ind=TRUE)
    cho<-maxSub[rand(1,length(maxSub))]
    choice[tries+1]<<-cho
    return(cho)
  }
  else # Choose the machine with the maximum frequency
  {
    cho<-allMach[rand(1,length(allMach))]
    choice[tries+1]<<-cho
    return(cho)
  }
}

while(tries<triesNum)
{
  cat("enter choice of machine and number of tries")
  machine<-chooseTS() # Use an algorithm to select a machine
  times<-1 # Try it once
  thisWin<-play(machine,times)
  # Updates the number of wins and attempts
  winNum[machine]<-winNum[machine]+thisWin
  totalNum[machine]<-totalNum[machine]+times
}
cat('finished, win:', wins) # Calculate the odds
cat('\n',p)
times<-array(1:triesNum)
plot(times,choice,type="b") # Draw a selection diagram to measure the rate of convergence

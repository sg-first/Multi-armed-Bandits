# multi-armed bandit simulator

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

getBayP<-function(mach) 
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

UCB<-function(mach)
{
  if(totalNum[mach]==0 || tries==0)
    return(0.5)
  else
  {
    r1 <- (tries/triesNum) * getBayP(mach)
    r2 <- ((triesNum-tries)/triesNum) * sqrt(2*log(tries)/totalNum[mach])
    return(r1+r2)
  }
}

rand<-function(min,max) {floor(runif(1,min,max+1))}

randChoose<-function(){rand(1,machNum)}
chooseUCB<-function()
{
  machScore <- array(0:0,dim=c(machNum))
  for(n in 1:machNum)
  {
    machScore[n] <- UCB(n)
  }
  maxSub<-which(machScore==max(machScore),arr.ind=TRUE)
  cho<-maxSub[rand(1,length(maxSub))]
  choice[tries+1]<<-cho
  return(cho)
}

chooseGreedy<-function()
{
  machScore <- array(0:0,dim=c(machNum))
  allMach <- array(1:machNum)
  for(n in allMach)
  {
    machScore[n] <- getBayP(n)
  }
  maxP=max(machScore)
  res<-runif(n)
  if(res<maxP[1])
  {
    maxSub<-which(machScore==max(machScore),arr.ind=TRUE)
    cho<-maxSub[rand(1,length(maxSub))]
    choice[tries+1]<<-cho
    return(cho)
  }
  else
  {
    cho<-allMach[rand(1,length(allMach))]
    choice[tries+1]<<-cho
    return(cho)
  }
}

while(tries<triesNum)
{
  cat("enter choice of machine and number of tries")
  machine<-chooseUCB()
  times<-1
  thisWin<-play(machine,times)
  winNum[machine]<-winNum[machine]+thisWin
  totalNum[machine]<-totalNum[machine]+times
}
cat('finished, win:', wins)
cat('\n',p)
times<-array(1:triesNum)
plot(times,choice,type="b")

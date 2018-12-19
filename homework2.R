library(tidyr)
path <- "/Users/linuslagergren/dropbox/cas/stochastic_processes/homework2/node.txt"
myLibrary <- "/Users/linuslagergren/dropbox/cas/linear_statistical_models/MyFunctionLibrary.R"
source(myLibrary)
##########################################
#1
##########################################
lambda <- 15/9

### Poisson offspring distribution
branch <- function(start, n,lam) {  ## Poisson
  z <- c(start,rep(0,n))
  for (i in 2:(n+1)) {
    z[i] <- sum(rpois(z[i-1],lam))
  }
  return(z)
}

### Poisson-gamma conjugacy distribution
myBranch <- function(start, n,lam) {  ## Poisson
  z <- c(start,rep(0,n))
  for (i in 2:(n+1)) {
    z[i] <- sum(rgamma(n*z[i-1],lam))
  }
  return(z)
}

trials <- 10000
n <- 10
start <- 7
simlist <- replicate(trials, branch(start, n,lambda))

#Computes how many of the simulations that die out
result <- 0
for (i in 1:dim(simlist)[2]){
  if (0 %in% simlist[,i]){
    result <- result + 1
  } 
}
extinctionProb <- result/trials

#This is what it should be but it gets to big in my case
sum(simlist==0)/trials
#Antalet nollor igenom antalet f??rs??k kan v??l inte bli r??tt?

##########################################
#2
##########################################
# a)
myData <- read.csv(path, sep = " ")
xmin <- min(myData[,1])
xmax <- max(myData[,1])
x <- seq(xmin,xmax,length=100)

myFunction <- function(x, teta){
  y <- teta[1]*sin(x/teta[2])
  return(y)
}

myFunction2 <- function(x, teta){
  y <- teta[1]*sin(x/teta[2])+teta[3]*rnorm(length(x), mean = 0, sd = 1)
  return(y)
}

#Seems to be possible parameters
tetaVec <- c(6,5,3.4131)
proposedFunction <- myFunction(x, tetaVec)
functionWithError <- myFunction2(x, tetaVec)
  
plot(myData[,1], myData[,2])
lines(x, proposedFunction, col='blue')

logLikelihood <- function(X){
  n <- length(X)
  return(-log(2*pi)*(n/2)-(1/2)*sum(X^2))
}

errors <-(tetaVec[3]^-1)*(myFunction(myData[,1], tetaVec) - myData[,2])

plot(myData[,1],errors)
abline(a=0, b=0)

plot(sort(errors))

computeLogLikelihood <- logLikelihood(errors)

badTetaVec <- c(1,1,1)
badProposedFunction <- myFunction(x, badTetaVec)
badErrors <-(badTetaVec[3]^-1)*(myFunction(myData[,1], badTetaVec) - myData[,2])

computeBadLogLikelihood <- logLikelihood(badErrors)

# b)
proposalFunction <- function(teta){
  newTeta <- abs(teta + rnorm(length(teta), mean=0, sd=0.1))
  return(newTeta)
}

newTetaVec <- proposalFunction(tetaVec)

i <- sample(seq(1, dim(myData)[1]), 1)

previousValue <- myFunction(myData[i,1], tetaVec)
proposalValue <- myFunction(myData[i,1], newTetaVec)

previousError <- (badTetaVec[3]^-1)*abs(previousValue-myData[i,2])
proposalError <- (badTetaVec[3]^-1)*abs(proposalValue-myData[i,2])

dnorm(proposalError)/dnorm(previousError)


# standardnormal.R
# Example 5.5

trials <- 1000000
simlist <- numeric(trials)
state <- 0
for (i in 2:trials) {
  prop <- runif(1, state-1,state+1)
  acc <- exp(-(prop^2-state^2)/2)
  if (runif(1) < acc) state <- prop
  simlist[i] <- state
}
hist(simlist,xlab="",main="",prob=T)
curve(dnorm(x),-4,4,add=T)




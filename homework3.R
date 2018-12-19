#######################################
# a)
#######################################
size = c(0,1)
mySquare = c(0.2, 0.6)
lambda = 36

simulateTrees <- function(size, lambda, square){
  N <- rpois(1, lambda)
  
  x <- runif(N,size[1], size[2])
  y <- runif(N,size[1], size[2])
  
  #Plotting to test which is a later excercise
  #plot(x,y,xlim = c(0,1),ylim = c(0,1))
  #rect(0.2, 0.2, 0.4, 0.4)
  
  #return(sum((x > 0.2 & x < 0.4) & (y > 0.2 & y < 0.4)))
  xInSquare <- (x > square[1] & x < square[2])
  yInSquare <- (y > square[1] & y < square[2])
  pointsInSquare <- sum(xInSquare & yInSquare)
  
  return(pointsInSquare >= 6)
}

simlist <- replicate(10000, simulateTrees(size, lambda, mySquare))
mean(simlist)

1-sum(dpois(seq(1,5),36*0.16))

#######################################
# b)
#######################################
size = c(0,1)
square1 = c(0.2, 0.6)
square2 = c(0.4, 0.8)
lambda = 36

simulateTrees2 <- function(size, lambda, square1, square2){
  N <- rpois(1, lambda)
  
  x <- runif(N,size[1], size[2])
  y <- runif(N,size[1], size[2])
  
  xInSquare1 <- (x > square1[1] & x < square1[2])
  yInSquare1 <- (y > square1[1] & y < square1[2])
  pointsInSquare1 <- sum(xInSquare1 & yInSquare1)
  
  xInSquare2 <- (x > square2[1] & x < square2[2])
  yInSquare2 <- (y > square2[1] & y < square2[2])
  pointsInSquare2 <- sum(xInSquare2 & yInSquare2)
  
  return(pointsInSquare1 == 4 & pointsInSquare2 == 4)
}

simlist <- replicate(100000, simulateTrees2(size, lambda, square1, square2))
mean(simlist) # 0.0226

dpois(4, 36*0.16) + dpois(4, 36*0.16) # 0.2890497
dpois(4, 2*36*0.16) # 0.007286622

#######################################
# c)
#######################################
size = c(0,1)
mySquare = c(0.2, 0.6)
lambda = 36

plotTrees <- function(size, lambda, square){
  N <- rpois(1, lambda)
  
  x <- runif(N,size[1], size[2])
  y <- runif(N,size[1], size[2])
  
  plot(x,y,xlim = size,ylim = size, col="blue", main="Plot of trees")
}

plotTrees(size, lambda, mySquare)

#######################################
# c)
#######################################

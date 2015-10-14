rm(list = ls())
source('~/cousera-ml/gradient-descent/gradient_vectorized.R')

### example 1 - one variable regression
myMatrix <- data.frame(x = c(0,1,2), y=c(1,3,5))
xMatrix <- cbind( x0 = rep(1,nrow(myMatrix)) , x1 = myMatrix$x )
yVector <- myMatrix$y
costs <- gradientDescent(c(30, 1), xMatrix, yVector, 0.5, 5000)
plot(costs)

### example 2 - one variable regression
data("ChickWeight")
cao <- subset(ChickWeight, Chick == 1 & Diet == 1)
myMatrix <- data.frame(x = cao$Time, y = cao$weight)
xMatrix <- cbind( x0 = rep(1,nrow(myMatrix)) , x1 = myMatrix$x )
yVector <- myMatrix$y
plot(myMatrix)
costs <- gradientDescent(c(30, 1), xMatrix, yVector, 0.01, 5000)
plot(costs)

### example 3
data("mtcars")
head(mtcars)
myMatrix <- data.frame(x = mtcars$hp, y = mtcars$mpg)
xMatrix <- cbind( x0 = rep(1,nrow(myMatrix)) , x1 = myMatrix$x )
yVector <- myMatrix$y
plot(myMatrix)
costs <- gradientDescent(c(30, 1), xMatrix, yVector, 0.00001, 5000)
plot(costs)


### example 4
myMatrix <- read.csv2('ex1data2.txt', sep=',', header = FALSE)

# normalizing the matrix before the computation
myMatrix <- t(t(myMatrix)/colSums(myMatrix)) 
xMatrix <- cbind( x0 = rep(1,nrow(myMatrix)) , x1 = myMatrix[,1] , x2=myMatrix[,2])
yVector <- myMatrix[,3]
costs <- gradientDescent(c(4, 22, 3), xMatrix, yVector, 1, 100000)
plot(costs)

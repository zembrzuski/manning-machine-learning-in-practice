costFunction <- function(teta0, teta1, myMatrix) {
  m <- length(myMatrix$x)
  1/(2*m) * sum((teta0 + teta1*myMatrix$x - myMatrix$y)^2)  
}

tetaZeroDerivativeSum <- function(teta0, teta1, myMatrix) {
  sum(teta0 + teta1*myMatrix$x - myMatrix$y)
}

tetaUmDerivativeSum <- function(teta0, teta1, myMatrix) {
  sum((teta0 + teta1*myMatrix$x - myMatrix$y) * (myMatrix$x))
}

gradientDescent <- function(teta0, teta1, myMatrix, alfa) {
  costs <- rep(NA, 5000)

  for (i in 1:5000) {  
    temp0 <- teta0 - alfa*(1/length(myMatrix$x)) * tetaZeroDerivativeSum(teta0, teta1, myMatrix)
    temp1 <- teta1 - alfa*(1/length(myMatrix$x)) * tetaUmDerivativeSum(teta0, teta1, myMatrix)

    costs[i] <- costFunction(temp0, temp1, myMatrix)
    teta0 <- temp0 ; teta1 <- temp1;
  }

  print(paste(paste(paste('teta0, teta1: ', teta0)), ', ', teta1))
  costs
}

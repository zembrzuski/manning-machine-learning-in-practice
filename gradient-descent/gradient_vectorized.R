costFunction <- function(thetas, xMatrix, yVector, m) {
  1/(2*m) * sum((thetas %*% t(xMatrix) - yVector)^2)
}

tetaDerivativeSum <- function(thetas, xMatrix, yVector) {
  (thetas %*% t(xMatrix) - yVector) %*% xMatrix
}

gradientDescent <- function(thetas, xMatrix, yVector, alfa, numberIterations) {
  m <- nrow(xMatrix)
  costs <- rep(NA, numberIterations)
  
  for (i in 1:numberIterations) {  
    thetas <- thetas - alfa*(1/nrow(xMatrix)) * tetaDerivativeSum(thetas, xMatrix, yVector)
    costs[i] <- costFunction(thetas, xMatrix, yVector, m)
  }
  
  print(thetas)
  costs
}
# xPoint - x value for a given point
# yPoint - y value for a given poitn
# datasetElement - another point to calculate the distance
#
# this function calculates the euclidian distance for 2 points
calculateDistanceSinglePoint <- function(xPoint, yPoint, referenceElement) {
  x <- as.numeric(referenceElement[1])
  y <- as.numeric(referenceElement[2])
  
  sqrt((x-xPoint)^2 + (y-yPoint)^2)
}

# xPoint - x value for a given point
# yPoint - y value for a given poitn
# dataset - dataset of many points
#
# this function calculates the euclidian distance for a
# given point and every point of the dataset
calculateDistanceDataset <- function(xPoint, yPoint, dataset) {
  dataframe <- data.frame(x = character(), y = numeric())
  for(i in 1:nrow(dataset)) {
    distance <- calculateDistanceSinglePoint(xPoint, yPoint, dataset[i,])
    dataframe <- rbind(dataframe, data.frame(dataset[i,3], distance))
  }
  dataframe
}

# xPoint - x value for a given point
# yPoint - y value for a given poitn
# dataset - dataset of many points
# nn - number of items with lowest distances to compare
#
# this function calculates the euclidian distance for a
# given point and every point of the dataset
knn <- function(xPoint, yPoint, dataset, nn) {
  df <- calculateDistanceDataset(xPoint, yPoint, dataset)
  df <- df[order(df[,2]),]
  x <- table(head(df, n=nn)[,1])
  names(x[which(x %in% max(as.data.frame(x)$Freq))])
} 

# given a vector of numbers, normalize it.
normalizeVector <- function(myVector) {
  (myVector-min(myVector))/(max(myVector)-min(myVector))
}

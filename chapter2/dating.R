rm(list = ls())

dating <- read.table('/home/nozes/Desktop/dating.txt')
names(dating) <- c('miles', 'video-games', 'ice-cream', 'like')

normalizedDating <- as.data.frame(cbind(
  miles = normalizeVector(dating$miles), 
  `video-games` = normalizeVector(dating$`video-games`), 
  `ice-cream` = normalizeVector(dating$`ice-cream`), 
  like = dating$like
))


baseSet <- head(cbind(normalizedDating$miles, normalizedDating$`video-games`, colour=normalizedDating$like), 800)
testingSet <- tail(cbind(normalizedDating$miles, normalizedDating$`video-games`, colour=normalizedDating$like), 200)

numberOfHits <- 0
for(i in 1:nrow(testingSet)) {
  answer <- knn(testingSet[i,1], testingSet[i,2], baseSet, 3)
  
  if (answer == normalizedDating[i+800,4]) {
    numberOfHits <- numberOfHits + 1
  }
  
  print(paste(i, answer == normalizedDating[i+800,4]))
}

print(paste("Hits rate: ", numberOfHits/200))

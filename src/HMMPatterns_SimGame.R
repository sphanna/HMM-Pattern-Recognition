library(ggplot2)

source("src/HMMPatterns_ModelFunctions.R")
source("src/HMMPatterns_Tables.R")
source("src/HMMPatterns_Util.R")

run <- function(numToPredict = 1){
  playHist <- vector()
  uniqueStates <- vector()
  predictions <- vector()
  benchmark <- vector()
  benchmark[1] <- 0
  predictions[1] <- 0
  modelData <- NA
  
  while(TRUE) {
    #Get Input
    input <- readline("Enter character (~ to end) > ")
    if(input == "~"){break}
    inputCharSeq <- strsplit(input,"")[[1]]
    
    #Convert input into state vectors
    playHist <- c(playHist,inputCharSeq)
    uniqueStates <- unique(playHist)
    numStates <- length(uniqueStates)
    playIDs <- getIdVector(playHist,uniqueStates)

    #model the observations and make predictions
    lastState = tail(playIDs,1)
    modelData <- unsupervisedModel(playIDs,numObsStates<-numStates,maxPatterns<-18,seQLength<-6)
    predictedStates <- getPredictedStates(numToPredict,modelData,lastState,numStates)
    predictions <- c(predictions, predictedStates$predictedObs)
    
    naiveP <- naivePredictor(playIDs)
    benchmark <- c(benchmark,naiveP)
    #predictions <- c(predictions,predictedStates@predictedObs)
    
    #output
    outputCharVector <- getCharVector(predictedStates$predictedObs,uniqueStates)
    print(paste(outputCharVector,collapse=""))
    #print(modelData)
    #print(playIDs)
    #print(predictions)
  }
  
  print("[end]")
  
  if(numToPredict == 1){
    playIDs[length(playIDs)+1] <- 0
    return(data.frame(playIDs,predictions,benchmark))
  }
}

compareModel <- function(plays, modelPredictions, spread){
  modelCorrect <- unlist(Map((function (x,y) x==y), plays,modelPredictions))
  modelPercents <- vector()
  
  for(k in 1:length(modelCorrect)){
    min <- max(1,k-spread)
    dif <- k - min
    matchesM <- lapply(TRUE, function(x) which(modelCorrect[min:k] %in% x))[[1]]
    modelPercents[k] <- length(matchesM)/dif
  }
  
  return(data.frame(modelPredictions,modelCorrect,modelPercents))
}

#spread is the number of states over which to average the percentages
analyzeRunData <- function(runData,spread=nrow(runData)){
  N <- nrow(runData)
  data <- runData[3:N-1,]
  plays <- data$playIDs
  predictions <- data$predictions
  benchPredictions <- data$benchmark
  
  modelDat <- compareModel(plays,data$predictions, spread)
  benchDat <- compareModel(plays,data$benchmark, spread)

  num <- as.numeric(row.names(data))
  allDat <- data.frame(num,plays,modelDat,benchDat)
  allDat <- allDat[2:nrow(allDat),]
  names(allDat)[6:8] <- c("benchPredictions", "benchCorrect", "benchPercents")
  return(allDat)
}


output <- run(numToPredict = 1)
data <- analyzeRunData(output)


ggplot(data, aes(x= num)) + geom_line(aes(y = modelPercents), color = 'blue') +
  geom_line(aes(y = benchPercents), color = 'dark green')


library(ggplot2)

source("src/HMMPatterns_ModelFunctions.R")
source("src/HMMPatterns_Tables.R")
source("src/HMMPatterns_Util.R")

run <- function(numToPredict = 1){
  playHist <- vector()
  uniqueStates <- vector()
  predictions <- vector()
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
    modelData <- unsupervisedModel(playIDs,numObsStates<-numStates,maxPatterns<-18,seQLength<-8)
    predictedStates <- getPredictedStates(numToPredict,modelData,lastState,numStates)
    predictions <- c(predictions, predictedStates$predictedObs)
    #predictions <- c(predictions,predictedStates@predictedObs)
    
    #output
    #outputCharVector <- getCharVector(predictedStates$predictedObs,uniqueStates)
    #print(paste(outputCharVector,collapse=""))
    #print(modelData)
    #print(playIDs)
    #print(predictions)
  }
  
  print("[end]")
  
  playIDs[length(playIDs)+1] <- 0
  return(data.frame(playIDs,predictions))
}

#spread is the number of states over which to average the percentages
analyzeRunData <- function(runData,spread=10){
  len <- nrow(runData)
  data <- runData[3:len-1,]
  plays <- data$playIDs
  numStates <- length(unique(plays))
  predictions <- data$predictions
  correct <- unlist(Map((function (x,y) x==y), plays,predictions))
  percents <- vector()
  
  for(k in 1:length(correct)){
    min <- max(1,k-spread)
    dif <- k - min
    matches = lapply(TRUE, function(x) which(correct[min:k] %in% x))[[1]]
    percents[k] <- length(matches)/dif
  }
  
  avg <- 1/numStates
  avgs <- rep(avg,length(correct))
  num <- as.numeric(row.names(data))
  allDat <- data.frame(num,plays,predictions,correct,percents,avgs)
  return(allDat)
}


output <- run(numToPredict = 1)
data <- analyzeRunData(output,nrow(output))
data
ggplot(data, aes(x= num)) + geom_line(aes(y = percents), color = 'blue') +
  geom_line(aes(y = avgs), color = 'dark green')


source("src/HMMPatterns_Functions.R")
source("src/HMMPatterns_Tables.R")

getPredictedStates <- function(N,data,playHistory,numObsStates){
  estPatternSeq <- data[[1]]
  estPatterns <- data[[2]]
  estPatternTransition <- data[[3]]
  
  currentPatternVec <- stateVec(tail(estPatternSeq,1),length(estPatterns))
  predictedPatternSeqVecs <- generateSequence(currentPatternVec, estPatternTransition,N)
  predictedPatterns <- toStateIDs(predictedPatternSeqVecs)
  
  lastState <- stateVec(tail(playHistory,1),numObsStates)
  predictedObsVecs <- generateObservables(lastState, predictedPatternSeqVecs, estPatterns)
  predictedObs <- toStateIDs(predictedObsVecs)
  
  return(data.frame(predictedObs,predictedPatterns))
}

getIdVector <- function(charVector, uniqueChars){
  N <- length(charVector)
  idVector <- vector()
  for(k in 1:N){
    idVector[k] <- match(charVector[k],uniqueChars)
  }
  return(idVector)
}

getCharVector <- function(idVector, uniqueChars){
  N <- length(idVector)
  charVector <- vector()
  for(k in 1:N){
    charVector[k] <- uniqueChars[idVector[k]]
  }
  return(charVector)
}

run <- function(){
  playHist <- vector()
  uniqueStates <- vector()
  
  while(TRUE) {
    input <- readline("Enter character (~ to end) > ")
    if(input == "~"){break}
    inputCharSeq <- strsplit(input,"")[[1]]
    playHist <- c(playHist,inputCharSeq)
    uniqueStates <- unique(playHist)
    numStates <- length(uniqueStates)
    playIDs <- getIdVector(playHist,uniqueStates)
    modelData <- unsupervisedModel(playIDs,numObsStates<-numStates,maxPatterns<-30,radius<-10)
    predictedStates <- getPredictedStates(100,modelData,playIDs,numStates)
    
    outputCharVector <- getCharVector(predictedStates$predictedObs,uniqueStates)
    print(paste(outputCharVector,collapse=""))
    #print(predictedStates[[1]])
  }

  print("[end]")
}

run()




#TESTING AREA
#plays <- c('a','b')
#uniqueStates <- unique(plays)
#numStates <- length(uniqueStates)
#playIDs <- getIdVector(plays,uniqueStates)
#modelData <- unsupervisedModel(playIDs,numObsStates<-numStates,maxPatterns<-6,radius<-2)
#getPredictedStates(5,modelData,playIDs,numStates)



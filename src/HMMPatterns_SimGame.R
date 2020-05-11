
source("src/HMMPatterns_ModelFunctions.R")
source("src/HMMPatterns_Tables.R")
source("src/HMMPatterns_Util.R")

run <- function(){
  playHist <- vector()
  uniqueStates <- vector()
  
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
    modelData <- unsupervisedModel(playIDs,numObsStates<-numStates,maxPatterns<-20,seQLength<-numStates, bias = 0.2)
    predictedStates <- getPredictedStates(300,modelData,playIDs,numStates)
    
    #output
    outputCharVector <- getCharVector(predictedStates$predictedObs,uniqueStates)
    #print(predictedStates$predictedPatterns)
    print(paste(outputCharVector,collapse=""))
    #print(modelData)
  }

  print("[end]")
}

run()



# libraries 
library(ggplot2)
library(gridExtra)
library(reshape2)

source("src/HMMPatterns_Functions.R")
source("src/HMMPatterns_Tables.R")

simulatePlays <- function(N,numObsStates,patternTable,patternTransition){
  #generate sequence of patterns
  patternStart <- rep(0,numPatterns)
  patternStart[sample(1:numPatterns, 1)] = 1 #random start pattern
  
  patternSeqVec <- getSequence(patternStart, patternTransition, N)
  patterns = toStateIDs(patternSeqVec)
  
  #generate observables using pattern sequence
  gameStart <- rep(0,numObsStates)
  gameStart[sample(1:numObsStates, 1)] = 1 #random play to start
  
  playsVec <- generateObservables(gameStart,patternSeqVec,patternTable)
  plays <- toStateIDs(playsVec)
  output <- data.frame(patterns,plays)
  return(output)
}

unsupervisedModel <- function(plays,numPatterns=4,numObsStates=4,radius=2){
  estPatternSeq <- getPatternSequence(plays, radius, numObsStates)
  patternSet <- getPatternSet(estPatternSeq,numPatterns)
  est <- unlist(Map({function (m) mostLikelyPattern(m, patternSet)}, estPatternSeq))
  estTransitionMatrix <- constructTransitionMatrix(est,numPatterns)
  output <- list(est,patternSet,estTransitionMatrix)
  return(output)
}

supervisedModel <- function(plays,patternTable,radius=2){
  numObsStates <- length(patternTable[[1]][,1])
  print(numObsStates)
  estPatternSeq <- getPatternSequence(plays, radius, numObsStates)
  estPatterns <- unlist(Map(function (x) mostLikelyPattern(x,patternTable), estPatternSeq))
  estTransitionMatrix <- constructTransitionMatrix(est,numPatterns)
  output <- list(estPatterns,patternTable,estTransitionMatrix)
  return(output)
}

#Initial States
#Data pulled from src/HMMPatterns_Tables.R
patternTransition <- fourPatternTransitionM
patternTable <- threeStatePatternTable()

#simulate plays
playOutput = simulatePlays(100,numObsStates=uniqueObs,patternTable,patternTransition)
patterns <- playOutput$patterns
plays <- playOutput$plays

#plays <- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,2,1,3,2,1,3,2,1,3,2,1,3,2,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,3,3,3,3,2,2,2,1,1,1,1)

#model plays with supervised pattern table
out <- supervisedModel(plays,patternTable,radius=2)
estSup <- out[[1]]
estPatternsSup <- out[[2]]
estPatternTransitionsSup <- out[[3]]

#model plays unsupervised
out <- unsupervisedModel(plays,numPatterns=4,numObsStates=length(unique(plays)),radius=2)
estUn <- out[[1]]
estPatternsUn <- out[[2]]
estPatternTransitionsUn <- out[[3]]

data <- data.frame(patterns,plays,estSup,estUn)
state <- as.numeric(row.names(data))
g0 <- ggplot(data, aes(x= state, y = patterns, fill = patterns, col = patterns)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

g1 <- ggplot(data, aes(x= state, y = plays, fill = plays, col = plays)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

g2 <- ggplot(data, aes(x= state, y = estSup, fill = estSup, col = estSup)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

g3 <- ggplot(data, aes(x= state, y = estUn, fill = estUn, col = estUn)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

grid.arrange(g0, g1, g2, g3, widths = 1, nrow = 4)

estPatternTransitionsSup
estPatternTransitionsUn

estPatternsUn

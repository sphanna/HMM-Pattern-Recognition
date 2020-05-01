# libraries 
library(ggplot2)
library(gridExtra)
library(reshape2)

source("src/HMMPatterns_Functions.R")
source("src/HMMPatterns_Tables.R")

#Initial States
#Data pulled from src/HMMPatterns_Tables.R
patternTransition <- fourPatternTransitionM
patternTable <- threeStatePatternTable()

N = 100
numObsStates = 3
numPatterns = length(patternTable)

#generate sequence of patterns
patternStart <- rep(0,numPatterns)
patternStart[sample(1:numPatterns, 1)] = 1 #random start pattern

patternSeqVec <- getSequence(patternStart, patternTransition, N)
patterns = toStateIDs(patternSeqVec)

#play game using pattern sequence
gameStart <- rep(0,numObsStates)
gameStart[sample(1:numObsStates, 1)] = 1 #random play to start

playsVec <- generateGame(gameStart,patternSeqVec,patternTable)
plays <- toStateIDs(playsVec)

#Estimate the patterns
radius = 1 #length on either side of state to determine pattern
est <- estimatePatternSequence(plays,patternTable,radius)

data <- data.frame(patterns,plays,est)

#Compare estimate
estTransitionMatrix <- constructTransitionMatrix(est,numPatterns)
patternTransition
estTransitionMatrix
likelyhood(estTransitionMatrix,patternTransition)


#output plot
g0 <- ggplot(data, aes(x= as.numeric(row.names(data)), y = patterns, fill = patterns, col = patterns)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

g1 <- ggplot(data, aes(x= as.numeric(row.names(data)), y = plays, fill = plays, col = plays)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

g2 <- ggplot(data, aes(x= as.numeric(row.names(data)), y = est, fill = est, col = est)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

grid.arrange(g0, g1, g2, widths = 1, nrow = 3)




# libraries 
library(ggplot2)
library(gridExtra)
library(reshape2)

source("src/HMMPatterns_Functions.R")
source("src/HMMPatterns_Tables.R")

#generate patterns and pattern transition matrix
patternTransition<-matrix(
  c(
    0.8,0.05,0.1,0.05,
    0.05,0.8,0.05,0.05,
    0.05,0.05,0.8,0.1,
    0.1,0.1,0.1,0.7
  ),4,4,byrow=TRUE
)

patternTable <- threeStatePatternTable()

#generate sequence of patterns
N = 100
numObsStates = 3
patternStart <- c(0,1,0,0)
patternSeqVec <- getSequence(patternStart, patternTransition, N)
patterns = toStateIDs(patternSeqVec)

#play game using pattern sequence
gameStart <- c(1,0,0)
playsVec <- generateGame(gameStart,patternSeqVec,patternTable)
plays <- toStateIDs(playsVec)

#Estimate the patterns
radius = 1
est <- estimatePatternSequence(plays,patternTable,radius)

data <- data.frame(patterns,plays,est)

#Compare estimate
estTransitionMatrix <- constructTransitionMatrix(est,length(patternTable))
patternTransition
estTransitionMatrix
likelyhood(estTransitionMatrix,patternTransition)

#Simulate next states and compare
#lastState = stateVec(plays[length(plays)],numObsStates)
#nextState(lastState,estTransitionMatrix)
#nextState(lastState,patternTransition)


#output plot
g0 <- ggplot(data, aes(x= as.numeric(row.names(data)), y = patterns, fill = patterns, col = patterns)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

g1 <- ggplot(data, aes(x= as.numeric(row.names(data)), y = plays, fill = plays, col = plays)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

g2 <- ggplot(data, aes(x= as.numeric(row.names(data)), y = est, fill = est, col = est)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

grid.arrange(g0, g1, g2, widths = 1, nrow = 3)




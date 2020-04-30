# libraries 
library(ggplot2)
library(gridExtra)
library(reshape2)

nextState <- function(state, transitionM){
  prob = state %*% transitionM
  n = length(state)
  ind = 1:n
  newstateIndex <- sample(ind, size = 1, replace = TRUE, prob)
  newstate = rep(0,n)
  newstate[newstateIndex] = 1
  return(newstate)
}

getState <- function(stateVec){
  return(match(1,stateVec))
}

stateVec <- function(stateID,numStates){
  vec = rep(0,numStates)
  vec[stateID] = 1
  return(vec)
}

likelyhood = function(m1,m2){
  n = length(m1)
  dif <- m1-m2
  d2 <- sum(dif^2)/n
  result <- sqrt(d2)
  return(1-result)
}

mostLikelyPattern <- function(seq, patternTable){
  numStates = length(patternTable[[1]][,1])
  pattern = constructTransitionMatrix(seq,numStates)
  n = length(patternTable)
  closenessVals = rep(NA,n)
  for(k in 1:n){
    closenessVals[k] = likelyhood(pattern,patternTable[[k]])
  }
  maxIndex = which.max(closenessVals)
  return(maxIndex)
}

estimatePatternSequence <- function(obsSeq, patternTable, radius){
  numStates <- length(patternTable[[1]][,1])
  N <- length(obsSeq)
  patternSeq <- rep(NA,N)
  for(k in 1:N){
    min <- k-radius
    max <- k+radius
    if(min < 1){min = 1}
    if(max > N){max = N}
    seq <- obsSeq[min:max]
    patternSeq[k] <- mostLikelyPattern(seq,patternTable)
  }
  return(patternSeq)
}

constructTransitionMatrix <- function(sequence,numStates){
  numTransitions = length(sequence)-1
  transitionM = matrix(0, nrow = numStates, ncol = numStates)
  for(k in 1:numTransitions){
    i <- sequence[k]
    j <- sequence[k+1]
    transitionM[i,j] = transitionM[i,j] + 1
  }
  
  #normalize rows
  for(l in 1:numStates){
    rowSum = sum(transitionM[l,1:numStates])
    if(rowSum != 0){
      transitionM[l,1:numStates] = transitionM[l,1:numStates] / sum(transitionM[l,1:numStates])
    }
  }
  return(transitionM)
}



toStateIDs <- function(stateList){
  n <- length(stateList)
  v <- rep(NA,n)
  for(k in 1:n){
    v[k] = getState(stateList[[k]])
  }
  return(v)
}

getSequence <- function(start, transitionM, N){
  seq <- list()
  seq[[1]] <- start
  for(k in 2:N){
    seq[[k]] <- nextState(seq[[k-1]], transitionM)
  }
  return(seq)
}

generateGame <- function(start, patternSeq, patternTable){
  N = length(patternSeq)
  plays = list()
  plays[[1]] = start
  for(k in 1:(N-1)){
    patternID = getState(patternSeq[[k]])
    pattern = patternTable[[patternID]]
    plays[[k+1]] = nextState(plays[[k]], pattern)
  }
  return(plays)
}

twoStatePatternTable <- function(){
  cycle <- matrix(
    c(
      0.05,0.95,
      0.95,0.05
    ),2,2,byrow=TRUE
  )
  
  repitition <- matrix(
    c(
      0.95,0.05,
      0.05,0.95
    ),2,2,byrow=TRUE
  )
  
  patternTable <- list()
  patternTable[[1]] <- cycle
  patternTable[[2]] <- repitition
  
  return(patternTable)
}

threeStatePatternTable <- function(){
  repitition <- matrix(
    c(
      1,0,0,
      0,1,0,
      0,0,1
    ),3,3,byrow=TRUE
  )
  
  rpsCycle <- matrix(
    c(
      0,1,0,
      0,0,1,
      1,0,0
    ),3,3,byrow=TRUE
  )
  
  prsCycle <- matrix(
    c(
      0,0,1,
      1,0,0,
      0,1,0
    ),3,3,byrow=TRUE
  )
  
  patternTable <- list()
  patternTable[[1]] <- repitition
  patternTable[[2]] <- rpsCycle
  patternTable[[3]] <- prsCycle
  
  return(patternTable)
}



#generate patterns and pattern transition matrix
patternTransition<-matrix(
  c(
    0.4,0.3,0.3,
    0.1,0.8,0.1,
    0.1,0.1,0.8
  ),3,3,byrow=TRUE
)

patternTable = threeStatePatternTable()

#generate sequence of patterns
N = 100
numObsStates = 3
patternStart <- c(0,1,0)
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
estTransitionMatrix <- constructTransitionMatrix(est,numObsStates)
likelyhood(estTransitionMatrix,patternTransition)

#Simulate next states and compare
lastState = stateVec(plays[length(plays)],numObsStates)
nextState(lastState,estTransitionMatrix)
nextState(lastState,patternTransition)


#output plot
g0 <- ggplot(data, aes(x= as.numeric(row.names(data)), y = patterns, fill = patterns, col = patterns)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

g1 <- ggplot(data, aes(x= as.numeric(row.names(data)), y = plays, fill = plays, col = plays)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

g2 <- ggplot(data, aes(x= as.numeric(row.names(data)), y = est, fill = est, col = est)) + 
  geom_bar(stat = "identity", alpha = I(0.7))

grid.arrange(g0, g1, g2, widths = 1, nrow = 3)


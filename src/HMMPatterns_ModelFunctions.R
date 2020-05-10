
#This file Contains the functions developed for the HMM Project.
source("src/HMMPatterns_Util.R")

##MODEL FUNCTIONS##

simulatePlays <- function(N,numObsStates,patternTable,patternTransition){
  #generate sequence of patterns
  numPatterns <- length(patternTable)
  patternStart <- rep(0,numPatterns)
  patternStart[sample(1:numPatterns, 1)] = 1 #random start pattern
  
  patternSeqVec <- generateSequence(patternStart, patternTransition, N)
  patterns = toStateIDs(patternSeqVec)
  
  #generate observables using pattern sequence
  gameStart <- rep(0,numObsStates)
  gameStart[sample(1:numObsStates, 1)] = 1 #random play to start
  
  playsVec <- generateObservables(gameStart,patternSeqVec,patternTable)
  plays <- toStateIDs(playsVec)
  output <- data.frame(patterns,plays)
  return(output)
}

#Input:
#plays: numeric array of observed states.  ex: c(1,2,1,2,1,3,3,3,4,4,3,2,3,1)
#numObsStates: number of observed states, usually input as the length(unique(plays))
#maxPatterns: defines how many patterns you would expect to find.  Useful for comparison with a supervised model
#seqLength: length of the subsequence to search for patterns
#bias: modifies the pattern transition matrix to make it less likely to get stuck on repeated patterns.
#
#Output is a list of three pieces of data:
#estPatterns: array of the estimated patterns for each state
#patternSet: a list of estimated patterns no larger than maxPatterns
#estTransitionMatrix: the estimated pattern transition matrix that defines transitions between patterns in the patternSet
unsupervisedModel <- function(plays,numObsStates=4,maxPatterns=4,seqLength=4, bias = 0.01){
  radius = floor(seqLength/2)
  estPatternSeq <- estPatternSequence(plays, radius, numObsStates)
  patternSet <- getPatternSet(estPatternSeq,maxPatterns)
  estPatterns <- unlist(Map({function (m) mostLikelyPattern(m, patternSet)}, estPatternSeq))
  estTransitionMatrix <- constructTransitionMatrix(estPatterns,length(patternSet), bias)
  output <- list(estPatterns,patternSet,estTransitionMatrix)
  return(output)
}

#Input:
#plays: numeric array of observed states.  ex: c(1,2,1,2,1,3,3,3,4,4,3,2,3,1)
#patternTable: list of matrices which define predetermined expected patterns in the sequence
#seqLength: length of the subsequence to search for patterns
#bias: modifies the pattern transition matrix to make it less likely to get stuck on repeated patterns
#
#Output is a list of three pieces of data:
#estPatterns: array of the estimated patterns for each state
#patternTable: the input pattern Table
#estTransitionMatrix: the estimated pattern transition matrix that defines transitions between patterns in the table
supervisedModel <- function(plays,patternTable,seqLength=4, bias = 0.01){
  radius = floor(seqLength/2)
  numPatterns <- length(patternTable)
  numObsStates <- length(patternTable[[1]][,1])
  estPatternSeq <- estPatternSequence(plays, radius, numObsStates)
  estPatterns <- unlist(Map(function (x) mostLikelyPattern(x,patternTable), estPatternSeq))
  estTransitionMatrix <- constructTransitionMatrix(estPatterns,numPatterns,bias)
  output <- list(estPatterns,patternTable,estTransitionMatrix)
  return(output)
}

#compare two markov transition matricies of equal size
likelyhood = function(m1,m2){
  n <- length(m1)
  dif <- m1-m2
  d2 <- sum(dif^2)/n
  result <- sqrt(d2)
  return(1-result)
}

#return the index of the most likely pattern found in the
#pattern table based on the input transition matrix
mostLikelyPattern <- function(transitionM, patternTable){
  n <- length(patternTable)
  closenessVals <- rep(NA,n)
  for(k in 1:n){
    closenessVals[k] <- likelyhood(transitionM,patternTable[[k]])
  }
  maxIndex <- which.max(closenessVals)
  return(maxIndex)
}

#Based on a sequence of observed states give an estimate for the most
#likely pattern.  We look at the current index +- the radius to build 
#a subsequence of observables from which we compute the likely pattern.
estPatternSequence <- function(obsSeq, radius, numObsStates){
  N <- length(obsSeq)
  patternSeq <- list()
  
  #if there is no sequence yet then return a uniform transition matrix
  if(N <= 1){
    patternSeq[[1]] <- uniformMarkov(numObsStates)
    return(patternSeq)
  }
  
  for(k in 1:N){
    min <- k-radius
    max <- k+radius
    if(min < 1){min = 1}
    if(max > N){max = N}
    seq <- obsSeq[min:max]
    patternMat <- constructTransitionMatrix(seq,numObsStates)
    patternSeq[[k]] <- patternMat
  }
  return(patternSeq)
}

#builds a transition matrix from a sequence of observables
constructTransitionMatrix <- function(sequence,numStates,bias=0){
  if(length(sequence) <= 1){
    return(uniformMarkov(numStates))
  }
  
  numTransitions <- length(sequence)-1
  #the bias is to help ensure we don't get stuck in one state forever
  transitionM <- matrix(bias, nrow = numStates, ncol = numStates)
  
  #simply count occruances of each transition
  for(k in 1:numTransitions){
    i <- sequence[k]
    j <- sequence[k+1]
    transitionM[i,j] <- transitionM[i,j] + 1
  }
  
  #TODO:Be smarter about this
  #if we see no occurance of some state in the pattern
  #how do we account for a situation where we get that state?
  #We should transition to a state in the pattern we observed.
  for(i in 1:numStates){
    rowSum = sum(transitionM[i,1:numStates])
    if(rowSum == 0){
      transitionM[i,1:numStates] = 1 + bias
      transitionM[i,i] = bias
    }
  }
  
  return(markovNormalize(transitionM))
}

getPatternSet <- function(pSeq, maxPatterns = 5, maxIterations = 10){
  numPatterns = maxPatterns + 1
  seq <- pSeq
  comp <- 1
  delta = 1 / maxIterations
  
  while(numPatterns > maxPatterns){
    seq <- reducePatternSequence(seq, comp)
    numPatterns <- length(seq)
    comp = comp - delta
  }
  
  return(seq)
}


reducePatternSequence <- function(pSeq, comp=1){
  result = list()
  first <- pSeq[[1]]
  
  compare <- Map(function (x) likelyhood(x,first) >= comp, pSeq) 
  matchedSet <- pSeq[compare == TRUE]
  sumMat <- Reduce(function (x,y) x + y, matchedSet)
  mat <- markovNormalize(sumMat)
  #mat <- comparisonSet[[sample(1:length(matchedSet),1)]]
  
  if(length(result)==0){
    result <- list(mat)
  } else{
    result <- c(result,list(mat))
  }
  
  notMatched <- pSeq[compare == FALSE]
  
  if(length(notMatched) > 1){
    result <- c(result, reducePatternSequence(notMatched, comp))
  } else {
    result <- c(result,notMatched)
  }
  
  return(result)
}

getPredictedStates <- function(N,data,playHistory,numObsStates){
  estPatternSeq <- data[[1]]
  estPatterns <- data[[2]]
  estPatternTransition <- data[[3]]
  
  currentPatternVec <- stateVec(tail(estPatternSeq,1),length(estPatterns))
  predictedPatternSeqVecs <- generateSequence(currentPatternVec, estPatternTransition,N+1)
  predictedPatterns <- toStateIDs(predictedPatternSeqVecs)
  
  lastState <- stateVec(tail(playHistory,1),numObsStates)
  predictedObsVecs <- generateObservables(lastState, predictedPatternSeqVecs, estPatterns)
  predictedObs <- toStateIDs(predictedObsVecs)[2:(length(predictedObsVecs))]
  
  return(data.frame(predictedObs,predictedPatterns[1:(length(predictedPatterns)-1)]))
}

##STATE GENERATION##

#sample the next state based on current state and the transition matrix
nextState <- function(state, transitionM){
  prob <- state %*% transitionM
  n <- length(state)
  ind <- 1:n
  newstateIndex <- sample(ind, size = 1, replace = TRUE, prob)
  newstate <- rep(0,n)
  newstate[newstateIndex] <- 1
  return(newstate)
}


#uses a patternSequence build a sequence of observable states
generateObservables <- function(start, patternSeq, patternTable){
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

generateSequence <- function(start, transitionM, N){
  seq <- list()
  seq[[1]] <- start
  for(k in 2:N){
    seq[[k]] <- nextState(seq[[k-1]],transitionM)
  }
  return(seq)
}





#This file Contains the fucntions developed for the HMM Project.


##MODEL FUNCTIONS##

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
getPatternSequence <- function(obsSeq, radius, numObsStates){
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
constructTransitionMatrix <- function(sequence,numStates){
  numTransitions <- length(sequence)-1
  transitionM <- matrix(0, nrow = numStates, ncol = numStates)
  
  #simply count occruances of each transition
  for(k in 1:numTransitions){
    i <- sequence[k]
    j <- sequence[k+1]
    transitionM[i,j] <- transitionM[i,j] + 1
  }
  
  for(i in 1:numStates){
    rowSum = sum(transitionM[i,1:numStates])
    if(rowSum == 0){
      transitionM[i,i] = 1
    }
  }

  return(markovNormalize(transitionM))
}

getPatternSet <- function(pSeq, maxPatterns = 5, maxIterations = 100){
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

##STATE UTILS##

#convert from state vector to state ID number
getState <- function(stateVec){
  return(match(1,stateVec))
}

#convert from state ID to state vector
stateVec <- function(stateID,numStates){
  vec <- rep(0,numStates)
  vec[stateID] <- 1
  return(vec)
}

#Convert a list of state vectors to a list of IDs
toStateIDs <- function(stateList){
  n <- length(stateList)
  v <- rep(NA,n)
  for(k in 1:n){
    v[k] = getState(stateList[[k]])
  }
  return(v)
}

randStateVec <- function(numStates){
  v <- rep(0,numStates)
  ind <- sample(1:numStates, 1)
  v[ind] <- 1
  return(v)
}

##TRANSITION MATRIX UTILS##

#normalizes by row
markovNormalize <- function(transitionM){
  size <- length(transitionM[,1])
  for(k in 1:size){
    rowSum = sum(transitionM[k,1:size])
    if(rowSum != 0){
      transitionM[k,1:size] = transitionM[k,1:size] / rowSum
    }
  }
  return(transitionM)
}

uniformMarkov <- function(size){
  transitionM <- matrix(0,size,size,byrow=TRUE)
  for(k in 1:size){
    transitionM[k,1:size] = 1 / size
  }
  return(transitionM)
}

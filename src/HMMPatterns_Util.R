#Utility File


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
  v <- rep(0,n)
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



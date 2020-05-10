#This file contains the functions that build the pattern tables
source("src/HMMPatterns_Util.R")

#generate patterns and pattern transition matrix


patternTransitionM <- function(){
  m<-matrix(
    c(
      0.9,0.01,0.02,0.07,0.01,0.01,
      0.01,0.9,0.07,0.02,0.01,0.01,
      0.01,0.02,0.9,0.07,0.01,0.01,
      0.1,0.1,0.1,0.7,0.01,0.01,
      0.01,0.02,0.04,0.07,0.8,0.01,
      0.01,0.02,0.1,0.07,0.01,0.8
    ),6,6,byrow=TRUE
  )
  
  return(markovNormalize((m)))
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
  
  rpCycle <- matrix(
    c(
      0,1,0,
      1,0,0,
      0.5,0.5,0
    ),3,3,byrow=TRUE
  )
  
  psCycle <- matrix(
    c(
      0,0.5,0.5,
      0,0,1,
      0,1,0
    ),3,3,byrow=TRUE
  )
  
  srCycle <- matrix(
    c(
      0,0,1,
      0.5,0,0.5,
      1,0,0
    ),3,3,byrow=TRUE
  )
  
  
  patternTable <- list()
  patternTable[[1]] <- repitition
  patternTable[[2]] <- rpsCycle
  patternTable[[3]] <- prsCycle
  patternTable[[4]] <- rpCycle
  patternTable[[5]] <- psCycle
  patternTable[[6]] <- srCycle
  
  return(patternTable)
}


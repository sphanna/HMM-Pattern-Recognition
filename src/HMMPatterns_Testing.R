#TESTING AREA
library(rTensor)

source("src/HMMPatterns_ModelFunctions.R")
source("src/HMMPatterns_Util.R")
source("src/HMMPatterns_Tables.R")

plays <- c(1,2,3,1,2,3,1,2,3,3,3,3,3,3,1,2,3,1,2,3,1,2,3,3,3,3,3,3,3)
uniqueStates <- unique(plays)
numStates <- length(uniqueStates)
playIDs <- getIdVector(plays,uniqueStates)
modelData <- unsupervisedModel(playIDs,numObsStates<-numStates,maxPatterns<-6,radius<-2)
getPredictedStates(20,modelData,playIDs,numStates)
print(modelData)

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

out<-svd(m)
out$u %*% t(out$d) %*% out$v

dv <- t(out$d) %*% out$v
out$u %*% t(dv)


tnsr <- rand_tensor(c(6,7,8))

hosvdD <-hosvd(tnsr)
hosvdD
hosvdD$fnorm_resid
hosvdD2 <-hosvd(tnsr,ranks=c(3,3,4))
hosvdD2$fnorm_resid



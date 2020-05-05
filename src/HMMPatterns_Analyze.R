# libraries 
library(ggplot2)
library(gridExtra)
library(reshape2)

source("src/HMMPatterns_Functions.R")
source("src/HMMPatterns_Tables.R")

#Initial States
#Data pulled from src/HMMPatterns_Tables.R
patternTransition <- patternTransitionM()
patternTable <- threeStatePatternTable()

#simulate plays
playOutput = simulatePlays(100,numObsStates=3,patternTable,patternTransition)
patterns <- playOutput$patterns
plays <- playOutput$plays

#plays <- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,2,1,3,2,1,3,2,1,3,2,1,3,2,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,3,3,3,3,2,2,2,1,1,1,1)

#model plays with supervised pattern table
out <- supervisedModel(plays,patternTable,radius=2)
estSup <- out[[1]]
estPatternsSup <- out[[2]]
estPatternTransitionsSup <- out[[3]]

#model plays unsupervised
out <- unsupervisedModel(plays,numObsStates=length(unique(plays)),maxPatterns=6,radius=2)
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


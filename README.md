# HMM-Pattern-Recognition

The models and related code can be found in HMMPatterns_ModelFunctions.R.  Below are some examples of how we can use both the unsupervised and supervised models.  We will first take a look at the HMMPatterns_Analyze.R file:

```{r}
#Initial States
#Data pulled from src/HMMPatterns_Tables.R
patternTransition <- patternTransitionM()
patternTable <- threeStatePatternTable()

#simulate plays
playOutput = simulatePlays(100,numObsStates=3,patternTable,patternTransition)
patterns <- playOutput$patterns
plays <- playOutput$plays
```

We first simulate a list of plays.  This is done with a pre-defined pattern table and associated transition matrix that encodes the transition probabilities between each of the patterns.  Above we simulate 100 plays with 3 observation states using this information.  This simulation step is unecessary to run the models.  You could simply create any vector of integers as an alternative such as:

```{r}
plays <- c(2,1,2,3,3,2,3,3,3,2,1,2,1,2,2,2,2,3,2,1,2,3,3,2,1,2,3,2,2,1)
```

Below we run two models on the simulated (or provided) data.

```{r}
#model plays with supervised pattern table
outSup <- supervisedModel(plays,patternTable,seqLength<-5)
estSup <- outSup[[1]]
estPatternsSup <- outSup[[2]]
estPatternTransitionsSup <- outSup[[3]]

#model plays unsupervised
outUn <- unsupervisedModel(plays,numObsStates<-length(unique(plays)),maxPatterns<-6,seqLength<-4)
estUn <- outUn[[1]]
estPatternsUn <- outUn[[2]]
estPatternTransitionsUn <- outUn[[3]]
```

The supervised model takes in a pattern table of expected patterns.  We also need to know the sequence length by which to find patterns.  The unsupervised model is given the number of states and the maximum number patterns to reduce to.  It may reduce the patterns that are found down to 6 or fewer in the example above.  We also need to give it a sequence length.

Both models output three pieces of data:
1) A vector representing the expected patterns at each observation.  These values represent the index of the pattern table where we can find the pattern.
2) The set of patterns.  If the model is supervised this is just the input pattern table.
3) The estimated transition matrix between patterns in the pattern table.

The information from these models we could look at the pattern sequence or we could use the pattern table and transition matrix to simulate predicted plays.

# HMM-Pattern-Recognition
The purpose of this project is to make accurate predictions of future states based on observed patterns in prior states.  In the models provided, a pattern is defined by a transition matrix between observed states.  The model makes use of a markov chain to estimate transition probabilities between these patterns.

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

In the code above we simulate a list of plays.  This is done with a pre-defined pattern table and associated transition matrix that encodes the transition probabilities between each of the patterns.  Above we simulate 100 plays with 3 observation states.  This simulation step is unecessary to run the models.  You could simply create any vector of integers as an alternative such as:

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

The supervised model takes in a pattern table of expected patterns.  We also need to know the sequence length by which the model will identify patterns.  The unsupervised model is given the number of states and the maximum number patterns to reduce to.  It may reduce the patterns that are found down to 6 or fewer in the example above.  We also need to give it a sequence length.

Both models output three pieces of data:
1) A vector representing the expected patterns at each observation.  These values represent the index of the pattern table where we can find the pattern.
2) The set of patterns.  If the model is supervised this is just the input pattern table.
3) The estimated transition matrix between patterns in the pattern table.

The information from these models we could look at the pattern sequence or we could use the pattern table and transition matrix to simulate predicted plays.  

I proved a getPredictedStates function that takes in the number of desired predicted states, the model data, the last observed state, and the number of states:

```{r}
predictedStates <- getPredictedStates(300,modelData,lastState,numStates)
```

This function outputs two vectors equal in length to the desired number of predicted states.  The first vector is of the predicted patterns and the second vector is the predicted states.

The HMMPatterns_SimGame.R file has two functions.  The run() function takes in input from the console.  This input can be any sequence of characters.  It then runs the unsupervised model on the input and makes predictions about next states.  You can output data from the run() function and run it with the AnalyzeRun() function to get some information about how well the predictions did.

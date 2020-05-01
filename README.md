# HMM-Pattern-Recognition

# Introduction
The purpose of this project is to use hidden markov models to make predictions based on patterns within a sequence of events.  A pattern is a subsequence of transitions between observable states.  While engaging in a game it is common for humans to change transition patterns routinely during play.  The hidden states become the transition patterns themselves.  If we can make inferences about the set of patterns used by an oppponent in a game, based on a sequence of their plays, then we may be able to predict their current pattern of play, and the chance that they may change to a new pattern.

As a beginning point for this project I have started with the three-observable-state game of rock, paper, scissors (r,p,s).  A sequence of plays may look something like this:

(r,r,p,s,r,s,p,p,p,s,r,r,s,r,s,p,p,p,r,p,p,r)

We want to predict our opponents next move then play the corresponding counter-move.  We assume that there may be patterns within this set of observables.  For example if our opponent played the following sequence of moves:

(r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r)

we can clearly see they are just repeating the same state.  We should hopefully predict the next play rather easily.  The pattern of repetition can be represented as a transition matrix between the three states as follows:

```{r}
repitition <- matrix(
c(
  1,0,0,
  0,1,0,
  0,0,1
  ),3,3,byrow=TRUE
)
```

To find the next state we multiply the state vector by the transition matrix.  Whatever the current state may be the next state will be the same.  Another example output might be:

(r,p,s,r,p,s,r,p,s,r,p,s,r,p,s)

In this case the player is simply cycling through rock, paper, then scissors.  We can represent this with the following transition matrix:
```{r}
rpsCycle <- matrix(
  c(
    0,1,0,
    0,0,1,
    1,0,0
  ),3,3,byrow=TRUE
)
```

Most player will not stick to a single pattern during play.  Perhaps a slightly more expert player will play:

(r,r,r,r,r,p,s,r,p,s,r,p,p,p,p,p,p,p,p,p)

We can quickly see that they were in the repitition pattern for some time then went into an rps cycle then continued repeating again.  We want to be able to estimate what pattern the player was in at each state and come up with a sequence of patterns.  If we call the repitition pattern, pattern 1 and the cycle pattern, pattern 2 then we would hope to get the following sequence based on the observed states above:

(1,1,1,1,1,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1)

We can then analyze this sequence as a standard markov chain with its own set of transition probabilities.  Using this we can make an estimate about the next pattern in the sequence, then the next played state based on that pattern.

# Estimating Patterns
Let's say we want to estimate the sequence of patterns based on the exammple observable sequence:

(r,r,r,r,r,p,s,r,p,s,r,p,p,p,p,p,p,p,p,p)

We can break the sequence up into a set of subsequences of fixed length.  The length is variable but we can assume it is the average length a person would stick to one sequence, say 5 states.  Based on that subsequence and the transitions between each state in the sequence we can create a transition matrix.  We can then compare this transition matrix to a list of possible transition patterns and choose the most likely one.  We do this for every observable state.  I use a likelyhood function to compare two transition matrices that is currently quite simple and needs to be improved.

# Some example output
Below is an plot of a simulated set of patterns (top graph) and the corresponding sequence of observables (middle graph).  The bottom graph is the estimated pattern based only on the observables.

![RPS Output](https://github.com/sphanna/HMM-Pattern-Recognition/blob/master/RPS_StateCapture.JPG)

Below is the output showing the original pattern Transition matrix, the estimated pattern transition matrix, and the similarity likelyhood.
```{r}
> patternTransition
     [,1] [,2] [,3]
[1,]  0.4  0.3  0.3
[2,]  0.1  0.8  0.1
[3,]  0.1  0.1  0.8
> estTransitionMatrix
           [,1]      [,2]       [,3]
[1,] 0.61111111 0.1666667 0.22222222
[2,] 0.10416667 0.8541667 0.04166667
[3,] 0.06060606 0.1212121 0.81818182
> likelyhood(estTransitionMatrix,patternTransition)
[1] 0.9074547
```



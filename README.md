# HMM-Pattern-Recognition

# Introduction
The purpose of this project is to use hidden markov models to make predictions based on patterns within a sequence of events.  A pattern is a subsequence of transitions between observable states.  While engaging in a game it is common for humans to change transition patterns routinely during play.  The hidden states become the transition patterns themselves.  If we can make inferences about the set of patterns used by an oppponent in a game, based on a sequence of their plays, then we may be able to predict their current pattern of play, and the chance that they may change to a new pattern.

As a beginning point for this project I have started with the three-observable-state game of rock, paper, scissors (r,p,s).  A sequence of plays may look something like this:

(r,r,p,s,r,s,p,p,p,s,r,r,s,r,s,p,p,p,r,p,p,r)

We want to predict our opponents next move then play the corresponding counter-move.  We assume that there may be patterns within this set of observables.  One example of a pattern is repetition.  So for example if our opponent played this:

(r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r)

we (and our model) should hopefully predict the next play rather easily.  The pattern of repetition can be represented as a transition matrix between the three states as follows:
repitition <- matrix(
  c(
    1,0,0,
    0,1,0,
    0,0,1
  ),3,3,byrow=TRUE
)

To find the next state we multiply the state vector by the transition matrix.  Whatever the current state may be the next state will be the same.  Another example output might be:

(r,p,s,r,p,s,r,p,s,r,p,s,r,p,s)

In this case the player is simply cycling through rock, paper, then scissors.  We can represent this with the following transition matrix:
rpsCycle <- matrix(
  c(
    0,1,0,
    0,0,1,
    1,0,0
  ),3,3,byrow=TRUE
)

Most player will not stick to a single pattern during play.  Perhaps a slightly more expert player will play:

(r,r,r,r,r,p,s,r,p,s,r,p,p,p,p,p,p,p,p,p)

We can quickly see that they were in the repitition pattern for some time then went into an rps cycle then continued repeating again.  We want to be able to estimate what pattern the player was in at each state and come up with a sequence of patterns.  If we call the repitition pattern, pattern 1 and the cycle pattern, pattern 2 then we would hope to get the following sequence based on the observed states above:

(1,1,1,1,1,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1)

We can then analyze this sequence as a standard markov chain with its own set of transition probabilities.  Using this we can make an estimate about the next pattern in the sequence, then the next played state based on that pattern.

# Estimating Patterns




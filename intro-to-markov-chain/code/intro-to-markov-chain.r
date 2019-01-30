# for working with a discrete time markov chain
library(markovchain)

#define the transition matrix
tmA <- matrix(c(0,0.5,0.5,0.5,0,0.5,0.5,0.5,0),nrow = 3, byrow = TRUE) 

dtmcA <- new("markovchain",transitionMatrix=tmA, states=c("a","b","c"), name="MarkovChain A") #create the DTMC
dtmcA

plot(dtmcA)

initialState<-c(0,1,0)

steps<-4

#using power operator
finalState<-initialState*dtmcA^steps 

finalState


data(rain)
mysequence<-rain$rain

#createSequenceMatrix returns a function showing previous vs actual states from the pairs in a given sequence.

createSequenceMatrix(mysequence)

myFit<-markovchainFit(data=mysequence,confidencelevel = .9)

# look at fit markov model
myFit


library(markovchain)
# simulate discrete Markov chains according to transition matrix P
run.mc.sim <- function( P, num.iters = 50, initial.state.is.random=F) {
  
  # number of possible states
  num.states <- nrow(P)
  
  # stores the states X_t through time
  states     <- numeric(num.iters)

  # initialize variable for first state 
  states[1]    <- 1 

  if (initial.state.is.random)
  {
	  states[1] <- sample(seq(1:nrow(P)), 1)
  }
  
  for(t in 2:num.iters) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1], ]
    
    ## draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}

data(rain)
mysequence<-rain$rain
myFit<-markovchainFit(data=mysequence,confidencelevel = .9)

# setup transition matrix
# note: we use markov chain library to figure
# out what this matrix is from an existing data set
P <- t(myFit$estimate@transitionMatrix)

num.trials <- 5
num.steps.to.simulate <- 365

# each column stores the sequence of states for a single chains
chain.states  <- matrix(NA, ncol=num.trials, nrow=num.steps.to.simulate)

# simulate chains
for(c in seq_len(num.trials)){
	chain.states[,c] <- run.mc.sim(P, num.iters=num.steps.to.simulate,initial.state.is.random=FALSE)
}

# clean up data a little bit to plot it

# make it a data frame
chain.states = as.data.frame(chain.states)

# make col names
prefix = "MC.Model.Run.Trajectory"
suffix = seq(1:num.trials)

column.titles = paste0(prefix,"-",suffix)

colnames(chain.states) = column.titles

# put labels in data.frame
chain.states <- chain.states %>% map_dfr(function(col) as.character(col))
chain.states <- chain.states %>% map_dfr(str_replace, pattern = "1", replacement = "0")
chain.states <- chain.states %>% map_dfr(str_replace, pattern = "2", replacement = "1-5")
chain.states <- chain.states %>% map_dfr(str_replace, pattern = "3", replacement = "6+")

# make it all factor data
chain.states <- chain.states %>% map_dfr(function(col) as.factor(col))

# add time steps to the data set
chain.states <-  chain.states %>% mutate(TimeStep = seq.int(nrow(chain.states)))

# now get it in a format ggplot likes
chain.states <- chain.states %>% gather("Model.Run", "State", column.titles)

# now ggplot it
ggplot(chain.states, aes(x=TimeStep, y=State, color=State, group=Model.Run))+
geom_point(size=2)+geom_line(size=1)+facet_wrap(~Model.Run, ncol=1)

# now figure out using all these examples how much rain we expect the next year

# first make it a string so we can do our substitutions
chain.states <- chain.states %>% mutate(State=as.character(State))

chain.states <- chain.states %>% mutate(State=str_replace(State, pattern="0", replacement="1"))

# since 3 is between 1 and 5 we will use that as our estimate
chain.states <- chain.states %>% mutate(State=str_replace(State, pattern="1-5", replacement="3"))

# six plus is harder call so we just say 10
chain.states <- chain.states %>% mutate(State=str_replace(State, pattern="6\\+", replacement="10"))

# now make the column numeric so we can do our math
chain.states <- chain.states %>% mutate(State=as.numeric(State))

# total rainfall amount is the sum of state
total.rainfall = chain.states %>% select(State) %>% sum()

# print result
cat(paste("After", num.trials, "Markov Chain model runs, we estimate there to be", 
          total.rainfall, "millileters of rain on Alofi island next year."))


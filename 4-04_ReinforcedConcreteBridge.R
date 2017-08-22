# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 4.4
# | PROBLEM NAME: Reinforced Concrete Bridge
# | UPDATE: AE     
# | DESCRIPTION: The exercise shows the calculation for first 
# |              passage time using the markovchain-package.
# |              The expected time of intervention is calculated
# |              for different intervention strategies.
# | 
# | KEYWORDS: passage, transition, Markov, intervention, expected, 
# |           time, bridge
# *-----------------------------------------------------------------

# Note: install package "markovchain" in order to run this script

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls()) #clear the memory and items in R console
library(markovchain)

### ------------------VARIABLE DEFINITION---------------------

# N is the number of time steps that we calculate first passage
# probabilities for

# The package Markov Chain allows the user to build a class around 
# the item, and the corresponding intervention strategy. We define 
# the transition matrix to fit the intervention strategy in question.
# Using this method we build classOldIS and classNewIS

### ---------------VARIABLE INPUT-------------------

N=200 

classOldIS <- new("markovchain"
                  , states=c("1","2","3","4","5")
                  , transitionMatrix=matrix(c(0.91,0.09,0,0,0
                                              ,0,0.95,0.05,0,0
                                              ,0,0,0.94,0.06,0
                                              ,0,0,0,0.85,0.15
                                              ,1,0,0,0,0)
                                            , nrow=5
                                            , byrow=TRUE)
                  , name="Old Intervention strategy, 1")

classNewIS <- new("markovchain"
                  , states=c("1","2","3","4","5")
                  ,transitionMatrix=matrix(c(0.91,0.09,0,0,0
                                             ,0,0.95,0.05,0,0
                                             ,0.8,0.2,0,0,0
                                             ,0,0,0,0.85,0.15
                                             ,0,0,0,0,1)
                                           , nrow=5
                                           , byrow=TRUE)
                  , name="New Intervention strategy, 2")



### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program. 

# A and B are matrices that store calculations of the passage
# probabilities.

A <- matrix(nrow=N, ncol=5)
B <- matrix(nrow=N, ncol=5)

# General and miscellaneous definitions
S <- 10 # Steps between values of results

# The firstPassage function calculates the probability of an item in 
# condition state 1 to reach a certain condition state in a certain 
# amount of time steps, in our case between 1 and N.

### ------------------CALCULATIONS-------------------

# The following for-loop now multiplies the time step with the 
# probability of passage into the intervention-triggering state. The 
# accumulated b then gives the expected time of intervention for the 
# item, according to the IS represented by the transition matrix.

A <- firstPassage(classOldIS,"1",N) 
b <- 0
for (i in 1:N)
{
  #  For this IS, the passage to CS5 is the time of intervention.
  b <- b+A[i,5]*i
}
print(b)
t1 <- b # The expected time of intervention for item, given IS1

# We do the same calculations for the New Intervention Strategy

B <- firstPassage(classNewIS,"1",N)
b <- 0
for (i in 1:N)
{
  # For this IS, passage to CS3 is the time of intervention.
  b <- b+B[i,3]*i 
}
print(b)
t2 <- b # The expected time of intervention for item, given IS2

# We set the results of the passage probabilities of IS2 up in the 
# following data frame

results <- data.frame(B[seq(0,N,by=S),1]
                      , B[seq(0,N,by=S),2]
                      , B[seq(0,N,by=S),3]
                      , B[seq(0,N,by=S),4]
                      , B[seq(0,N,by=S),5])
names(results) <- c("1-1","1-2","1-3","1-4","1-5")
print(results)


print('The expected time of intervention for item, given IS1 is ')
print(t1)
print('The expected time of intervention for item, given IS2 is ')
print(t2)


### --------------------PLOT--------------------

### ------------------PLOT END-------------------

# END

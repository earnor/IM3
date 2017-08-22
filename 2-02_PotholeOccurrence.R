# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 2.2
# | PROBLEM NAME: Pothole Occurrence
# | UPDATE: AE     
# | DESCRIPTION: This program simulates Pothole Occurrence in
# |              town sections as a Poisson-distributed variable.
# |              The program finds the solution of the problem when
# |              P(N(t)>n.lim)
# | 
# | KEYWORDS: Poisson, pothole, simulation, occurrence
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------

# IMPORTANT: Please make sure, before running the script, that the 
#            following file is located in the current working 
#            directory. To see which folder is the working directory,
#            type ´getwd()´ in the console.
setwd("Y:/common/IBI/Martani/IM Script - Workspace/Exercises/R-files/Updated")
data<-read.csv("2-02_PotholeOccurrence_csvData.csv"
               ,header=TRUE,sep=",") 
attach(data)

### ------------------VARIABLE DEFINITION---------------------

## Let us define the variables used for the program.

# lambda is the expected value of the Poisson function, the mean rate
# of occurrence N(t) is the number of occurrences in a time t. 
# Symbolised in for-loops as n.

# .a, .b, and .c are used to differentiate between sections of the 
# town. The town, composed of all three sections is symbolised with 
# .t . .res stands for Results

### ---------------VARIABLE INPUT-------------------


### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program. We must calculate 
## Probabilities according to Poisson model, the probability within
## the time frame (t,t+1] for N(t)=n. 
## Question B asks for the Probability of N(t)>5. In order to 
## calculate this, we use a for loop, to calculate probabilities of
## all instances when N(t) is less than or equal to 5, and subtract
## it from 1 

# General and miscellaneous
N.max <- 12 # n.max >= n.lim . This value defines x-axis on plot.
N.lim <- 5  # n.lim is the limit set by question B.
T <- 1 # Question B defines the time of inspection as 1 month.

# Define empty matrices
# Matrix holds the probabilities of different values of n
P.a <- matrix(nrow=N.max) 
# A separate matrix is calculated for each section.
P.b <- matrix(nrow=N.max) 
P.c <- matrix(nrow=N.max)
P.t <- matrix(nrow=N.max)

# Now we define vectors that hold the sum of calculated probabilities
# for occurrences between 1 and N.lim. 
# P(N<=5)
sumP <- c("Section A"=0,"Section B"=0,"Section C"=0,"Town"=0)
# P(N>5)
P.res <- c("Section A"=0,"Section B"=0,"Section C"=0,"Town"=0) 

### ------------------CALCULATIONS-------------------

# Question A: Estimate the pothole occurrence rate of each road  
#             section and town as a whole. Units are 1/month
cat("QUESTION A - pothole occurrence rate at each road section \n")

lambda.a <- mean(data$A)
lambda.b <- mean(data$B)
lambda.c <- mean(data$C)
lambda.t <- (mean(data$A)+mean(data$B)+mean(data$C))

lambda <- data.frame(lambda.a , lambda.b , lambda.c , lambda.t)
cat("Answer A: Mean occurrence rate for each section")
print(lambda)

cat("QUESTION B - probability that N(t)>N.lim=5 \n")

# First we define the probability function of the Poission (.poi) 
# model
Pr.poi = function(l,n,t){exp(-l*t)*((l*t)^n)/factorial(n)}

for (n in 1:N.max)
{
  P.a[n] <- Pr.poi(lambda.a,n,T)
  P.b[n] <- Pr.poi(lambda.b,n,T)
  P.c[n] <- Pr.poi(lambda.c,n,T)
  P.t[n] <- Pr.poi(lambda.t,n,T)
}
for (n in 1:N.lim)
{
  # At end of loop, we get P[5]+P[4]+P[3]+P[2]+P[1]
  sumP[1] <- sumP[1] + P.a[n] 
  sumP[2] <- sumP[2] + P.b[n] # Done for each section
  sumP[3] <- sumP[3] + P.c[n] # And placed in vector sumP
  sumP[4] <- sumP[4] + P.t[n]
}
# results of P=1-P[5]-P[4]-P[3]-P[2]-P[1] for all sections
P.res <- 1-sumP 
cat("Answer B: Probability of more than 5 Potholes in each section 
    in 1 month \n")
print(P.res)
### --------------------PLOT--------------------

### ------------------PLOT END-------------------

# END

# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 4.3
# | PROBLEM NAME: Reinforced Concrete Bridge
# | UPDATE: AE     
# | DESCRIPTION: This program calculates the steady state 
# |              probability of a system dependent on two different
# |              intervention strategies. We use the steady state
# |              probabilities and assumed costs of intervention to
# |              compare the costs of the two IS. We then find an
# |              optimal IS.
# | 
# | KEYWORDS: Markov, transition, intervention, strategy
# |           
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------

rm(list=ls()) #clear the memory and items in R console

setwd("Y:/common/IBI/03-Employees Folders/Martani/IM3 Script - Workspace/Exercises/R-files/Updated")
data1 <- read.csv("4-03_ReinforceConcreteBridge-P_csvData.csv"
                  ,header=TRUE,sep=";") 
attach(data1)
data2 <- read.csv("4-03_ReinforceConcreteBridge-R_csvData.csv"
                  ,header=TRUE,sep=";") 
attach(data2)

library(ggplot2)
library(tidyr)

### ------------------VARIABLE DEFINITION---------------------

## First we want to define the variables 

# The matrix P contains the transition matrix of the system
# The matrix R contains the transition effectiveness vectors of
# different IS
# The vector C contains the different costs. 


### ---------------VARIABLE INPUT-------------------

N.c <- length(data1[,1]) # maximum number of condition states
N.r <- length(data2[,1]) # number of strategies
T.max <- 100 # number of year

# define the dimension for transition matrix P
P <- array(dim=c(N.c,N.c)) 
# intervention effectiveness vectors
R <- array(dim=c(N.r,N.c))
# cost of each IS
C <- matrix(nrow=N.r)


# Read value of transition matrix and effectiveness- and 
# cost-vectors from csv file
for (j in 1:N.c)
{

  for (i in 1:N.r)
  {
    R[i,j] <- data2[i,j]
    C[i]   <- data2$cost[i]
  }
  for (k in 1:N.c)
  {
    P[j,k] <- data1[j,k]
  }
}
cat('value of transition matrix P \n')
print(P)
cat('value of intervention vector R \n')
print(R)
cat('cost incurred by each IS C \n')
print(C)

Cvec <- matrix(0,nrow=N.c,ncol=N.r)
Cvec[5,1] <- C[1]
Cvec[5,2] <- C[1]
Cvec[3,2] <- C[2]
Cvec



### ---------------PROGRAM OUTPUT-------------------



### ------------------CALCULATIONS-------------------

# transition matrix corresponding to each IS
Q <- array(dim=c(N.c,N.c,N.r)) 
for (i in 1:N.c)
{
  for (j in 1:N.c)
  {
    for (k in 1:N.r)
    {
      if (k==1)
      {
        Q[i,j,k] <- P[i,j]
        Q[5,j,k] <- R[1,j]
        Q[i,j,k] <- Q[i,j,k]
      } 
      else if (k==2)
      {
        Q[i,j,k] <- P[i,j]
        Q[3,j,k] <- R[2,j]
        Q[5,j,k] <- R[1,j]
        Q[i,j,k] <- Q[i,j,k]
      }
    }
  }
}

#
cat('Intervention repair matrix Q \n')
print(Q)

pi <- array(dim=c(N.c,T.max,N.r)) # this is the steady state prob.

for (k in 1:N.r)
{
  for (t in 1:T.max)
  {
    if (t==1)
    {
      pi[,t,k] <- c(10/23,4/23,6/23,2/23,1/23)
    } 
    else 
    {
      pi[,t,k] <- pi[,t-1,k]%*%Q[,,k]   # %*% is matrix multiplic.
    }
  }
}
options("scipen"=100, "digits"=4)
pi[,T.max,] <- as.numeric(pi[,T.max,])

cat('the steady state corresponding to each IS \n')
print(pi[,T.max,])

cat('estimating the cost associated with each strategy \n')
C.is <- pi[,T.max,]*Cvec
print(C.is)

cat('The savings made by switching to IS 2 are\n')
print(sum(C.is[,1])-sum(C.is[,2]))
cat('Where IS 2 costs annually')
print(sum(C.is[,2]))
### --------------------PLOT--------------------
# The following code is for plotting purposes for part A

data <- as.data.frame(t(pi[,,1]))
data <- cbind(1:T.max,data)
names(data) <- c("t","CS1","CS2","CS3","CS4","CS5")
datatidy <- gather(data
                   ,CS1,CS2,CS3,CS4,CS5
                   ,key="key",value="value")

plotoldIS <- ggplot(datatidy, aes(x=t,y=value,group=key)) +
  geom_area(aes(fill = key)) +
  geom_line(aes(group = key), position = "stack") +
  scale_colour_hue(name="Condition state",      # Set legend title
                 breaks=c("CS1","CS2","CS3","CS4","CS5"),
                 labels=c("\nCS1\n"
                          , "\nCS2\n"
                          , "\nCS3\n"
                          , "\nCS4\n"
                          , "\nCS5\n")
  )  +
  xlim(0,30)+
  scale_fill_discrete(name="Set-up time") +
  xlab("Time [years]") +
  ylab("Share in each CS [-]") + # Set axis labels
  theme_bw(base_size=36)

plotoldIS
dev.copy(png,'E4-03_oldIS.png',width=2000,height=1330)
dev.off()

# New IS
data <- as.data.frame(t(pi[,,2]))
data <- cbind(1:T.max,data)
names(data) <- c("t","CS1","CS2","CS3","CS4","CS5")
datatidy <- gather(data
                   ,CS1,CS2,CS3,CS4,CS5
                   ,key="key",value="value")

plotnewIS <- ggplot(datatidy, aes(x=t,y=value,group=key)) +
  geom_area(aes(fill = key)) +
  geom_line(aes(group = key), position = "stack") +
  scale_colour_hue(name="Condition state",      # Set legend title
                   breaks=c("CS1","CS2","CS3","CS4","CS5"),
                   labels=c("\nCS1\n"
                            , "\nCS2\n"
                            , "\nCS3\n"
                            , "\nCS4\n"
                            , "\nCS5\n")
  )  +
  xlim(0,30)+
  scale_fill_discrete(name="Set-up time") +
  xlab("Time [years]") +
  ylab("Share in each CS [-]") + # Set axis labels
  theme_bw(base_size=36)

plotnewIS
dev.copy(png,'E4-03_newIS.png',width=2000,height=1330)
dev.off()

### ------------------PLOT END-------------------

# Question B

results <- as.data.frame(t(pi.plot[,1:15,1]))
results <- cbind(results,t(pi.plot[,1:15,2]))

names(results) <- c("YEAR / IS1 -     CS1","CS2","CS3","CS4","CS5"
                    ,"IS2 -      CS1","CS2","CS3","CS4","CS5")
results


# END

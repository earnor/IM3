# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 3.11
# | PROBLEM NAME: Road Interventions
# | UPDATE: AE     
# | DESCRIPTION: Given benefits of road sections of adequate
# |              LOS, and penalties during their downtime,
# |              we can calculate the cost per unit time of the
# |              operation. As the cost is a term covering both 
# |              negative and positive costs, we maximise
# |              the costs in this example to find the 
# |              optimal time between preventive interventions.
# | 
# | KEYWORDS: failure, optimisation, time-dependent, Weibull,
# |           road, LOS, cost,
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls())     # clear the memory and items in R console
setwd("Y:/common/IBI/03-Employees Folders/Martani/IM3 Script - Workspace/Exercises/R-files/Updated")
data <- read.csv("3-11_RoadInterventions_csvData.csv"
                 ,header=TRUE,sep=";",dec=",") 
attach(data)

library(ggplot2)
library(tidyr)
### ------------------VARIABLE DEFINITION---------------------

## First we want to define the variables used for the program. 

# Indices are used for variables, bI stands for between Interventions
# as in text. Likewise, dI stands for 'during Interventions'. o 
# stands for operating, f for failed, i for every intervention and n
# for number of items.


### ---------------VARIABLE INPUT-------------------

N       <- nrow(data)   # numbers of items
# reward revenue per m2 per tu between Interventions
C.bI.o  <- as.vector(data$reward)  
# penalty for item not functioning per tu
C.bI.f  <- as.vector(data$penalty)*1000 
# fixed costs during intervention
C.dI.i  <- as.vector(data$fixed)*1000  
# variable costs per failed silo during intervention.
C.dI.n  <- as.vector(data$variable)*1000  
w       <- as.vector(data$width)
l       <- as.vector(data$length)
LAMBDA  <- as.vector(data$lambda)
BETA    <- as.vector(data$beta)
T.max   <- 30

# we will search for optimal renewal time in range T
T       <- seq(1,T.max, by=1) 

### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program. 

# The cost per silo if failure occurs on (0,T]
C1    <- matrix(nrow=length(T)) 
# The cost per silo if no failure occurs on (0,T]
C2    <- matrix(nrow=length(T)) 
C.bI  <- matrix(nrow=length(T),ncol=N) # C1+C2
eta   <- matrix(nrow=length(T),ncol=N) # (Positive) Cost per tu
eta.max  <- matrix(nrow=N)
eta.i    <- matrix(nrow=N)
eta.B1    <- matrix(nrow=length(T))
eta.B2    <- matrix(nrow=length(T))

# We define the probability and cumulative density function for 
# expon. distribution
f = function(x,lambda,beta){((lambda^beta)*beta*(x^(beta-1)))*
    exp(-((lambda*x)^beta))} 
F = function(x,lambda,beta){1-exp(-((lambda*x)^beta))}


# If we split C.bI into two parts, one consisting of the integral, 
# C1, and the other the rest of the sum, C2, we set up the function
# that is inside the integral as 
c1Int = function(x,a,b,t,lambda,beta)
{
  ((a*x)+(b*(t-x)))*f(x,lambda,beta)
} 


### ------------------CALCULATIONS-------------------
# We calculate the terms individually to be able to store them and 
# publish them as results.

for (i in (1:N)){
  
  for (t in (1:length(T)))
  {
    C1[t]  <- integrate(c1Int,0,T[t]
                        ,a=C.bI.o[i]*w[i]*l[i]
                        ,b=C.bI.f[i]
                        ,lambda=LAMBDA[i]
                        ,beta=BETA[i]
                        ,t=T[t])$val
    C2[t]     <- (C.bI.o[i]*w[i]*l[i]*T[t]*
                    (1-F(T[t],LAMBDA[i],BETA[i])))
    C.bI[t,i] <- C1[t]+C2[t]
    eta[t,i]  <- (C.bI[t,i]-C.dI.i[i]-C.dI.n[i])/T[t]/1000
    
  }
  print(C1)
  eta.max[i] <- max(eta[,i])
  eta.i[i] <- which.max(eta[,i])
}

cat("value of reward \n")
print(eta)

cat("Maximum value of reward and T* \n")
print(cbind(eta.max,eta.i))


# Part B
for (t in 1:T.max)
{
  eta.B1[t] <- ( sum(C.bI[t,])-sum(C.dI.i)-sum(C.dI.n)) /t /1000
  eta.B2[t] <- ( sum(C.bI[t,]) - 300000   -sum(C.dI.n)) /t /1000
}

cat("Maximum value of reward for IS B with unchanged set up costs\n")
print(max(eta.B1))
cat("Maximum value of reward for IS B with 300'000 mu \n")
print(max(eta.B2))

# Part C
cat("Maximum value of reward for IS A totalled over all items \n")
print(sum(eta.max))

cat("More reward for unchanged set up costs in comparison to OIS \n")
print(max(eta.B1)/ sum(eta.max))
cat("More reward for saving set up costs in comparison to OIS \n")
print(max(eta.B2)/ sum(eta.max))



# We see that if costs are unchanged, it is beneficial to use IS A 
# where all items are treated indiviudally, but if savings are made, 
# so the fixedcosts only total 300'000 instead of 587'000 mu, more 
# profit can be madeas seen by the maximum value of eta.B2


### --------------------PLOT--------------------
## The following code is only for plotting the figure given as 
## a solution to the Exercise

rdf <- as.data.frame(cbind(1:T.max,eta))
names(rdf) <- c("T","item1","item2","item3","item4","item5"
                ,"item6","item7","item8","item9","item10","item11")

rdftidy <- gather(rdf
                  ,item1,item2,item3,item4,item5
                  ,item6,item7,item8,item9,item10,item11
                      ,key="key",value="value")

plotetaT <- ggplot(data=rdftidy,aes(x=T,y=value
                                    ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Item",      # Set legend title
                   breaks=c("item1","item2","item3","item4","item5"
                            ,"item6","item7","item8","item9","item10"
                            ,"item11"),
                   labels=c("\n1\n"
                            , "\n2\n"
                            , "\n3\n"
                            , "\n4\n"
                            , "\n5\n"
                            , "\n6\n"
                            , "\n7\n"
                            , "\n8\n"
                            , "\n9\n"
                            , "\n10\n"
                            , "\n11\n")
  )  +
  scale_linetype_discrete(name="Item") +
  xlab("T [years]") + 
  ylab("Average cost per year [mu/yr]") + # Set axis labels
  ylim(0,80) +
  theme_bw(base_size=36) +
  theme(legend.position=c(.977, .5))           # Position legend

# Let us collect the optimal points in the data frame optimal
optimal <- as.data.frame(matrix(nrow=11,ncol=3))
names(optimal) <- c("key","x","y")

key <- c("item1","item2","item3","item4","item5"
         ,"item6","item7","item8","item9","item10","item11")
data <- rdf
for (i in 1:11)
{
  optimal[i,1] <- key[i]
  optimal[i,2] <- data$T[which.max(data[,i+1])]
  optimal[i,3] <- max(data[,i+1])
}
# Now we add these points to the graph before exporting
plotetaT + geom_point(data=optimal
                      , aes(x=x
                            , y=y)
                      , colour="navyblue"
                      , size=6) 

dev.copy(png,'E3-11_eta.png',width=2000,height=1330)
dev.off()

## Second plot for part B
etaB <- data.frame(cbind(1:T.max,eta.B1,eta.B2))
names(etaB) <- c("T","unchanged","changed")

rdftidyB <- gather(etaB
                  ,unchanged,changed
                  ,key="key",value="value")

plotetaT <- ggplot(data=rdftidyB,aes(x=T,y=value
                                     ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Costs",      # Set legend title
                   breaks=c("unchanged","changed"),
                   labels=c("\nUnchanged\n"
                            , "\nChanged to 300'000\n")
  )  +
  scale_linetype_discrete(name="Costs") +
  xlab("T [years]") + 
  ylab("Average cost per year [mu/yr]") + # Set axis labels
  ylim(0,200) +
  theme_bw(base_size=36) +
  theme(legend.position=c(.8, .3))           # Position legend

# Let us collect the optimal points in the data frame optimal
optimal <- as.data.frame(matrix(nrow=2,ncol=3))
names(optimal) <- c("key","x","y")

key <- c("unchanged","changed")
data <- etaB
for (i in 1:2)
{
  optimal[i,1] <- key[i]
  optimal[i,2] <- data$T[which.max(data[,i+1])]
  optimal[i,3] <- max(data[,i+1])
}
# Now we add these points to the graph before exporting
plotetaT + geom_point(data=optimal
                      , aes(x=x
                            , y=y)
                      , colour="navyblue"
                      , size=6) 

dev.copy(png,'E3-11_etaB.png',width=2000,height=1330)
dev.off()

### ------------------PLOT END-------------------

# END

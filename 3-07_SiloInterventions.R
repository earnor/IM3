# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 3.7
# | PROBLEM NAME: Silo Interventions
# | UPDATE: AE     
# | DESCRIPTION: Given benefits of operating silos, and penalties
# |              during their downtime, we can calculate the cost 
# |              per unit time of the operation. As the cost is a 
# |              term covering both negative and positive costs, we 
# |              maximise the costs in this example to find the 
# |              optimal time between preventive interventions.
# | 
# | KEYWORDS: exponential, failure, optimisation, time-dependent, 
# |           Silo, cost,
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls())     # clear the memory and items in R console
library(ggplot2)
### ------------------VARIABLE DEFINITION---------------------

## First we want to define the variables used for the program. They 
## are extracted from question

# Indices are used for variables, bI stands for between Interventions
# as in text. Likewise, dI stands for 'during Interventions'. o 
# stands for operating, f for failed, i for every intervention and n
# for number of items.


### ---------------VARIABLE INPUT-------------------

N       <- 10   # numbers of items
C.bI.o  <- 100  # reward revenue per tu between Interventions
C.bI.f  <- -20  # penalty for item not functioning per tu
C.dI.i  <- 600  # fixed costs during intervention
C.dI.n  <- 15   # variable costs per failed silo during intervention.
LAMBDA  <- .02
T.max   <- 20

# we will search for optimal renewal time in range T
T       <- seq(1,T.max, by=1) 

### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program.
## Mean cost per time unit, eta, is found for the Time dependent 
## planned replacement with no unplanned interventions IS. The time 
## period that maximises the positive costs, shown with eta is the 
## optimal time T we seek, i.e. how much time should pass between PIs
## so costs are optimised.

# Define empty matrices for models

# The cost per silo if failure occurs on (0,T]
C1    <- matrix(nrow=length(T)) 
# The cost per silo if no failure on (0,T]
C2    <- matrix(nrow=length(T)) 
C.bI  <- matrix(nrow=length(T)) # C1+C2
eta   <- matrix(nrow=length(T)) # (Positive) Cost per unit time

# We define the probability and cumulative density function for 
# expon. distribution
f = function(x,lambda){lambda*exp(-(lambda*x))} 
F = function(x,lambda){1-exp(-(lambda*x))}

# If we split C.bI into two parts, one consisting of the integral,
# C1, and the other the rest of the sum, C2, we set up the function 
# that is inside the integral as
c1Int = function(x,a,b,t,lambda){(a*x+b*(t-x))*f(x,lambda)} 

### ------------------CALCULATIONS-------------------
# We calculate the terms individually to be able to store them and 
# publish them as results.

for (i in (1:length(T))){
  C1[i]  <- integrate(c1Int,0,T[i]
                      , a=C.bI.o
                      , b=C.bI.f 
                      , t=T[i]
                      , lambda=LAMBDA)$val
  C2[i]  <- C.bI.o*T[i]*(1-F(T[i],LAMBDA))
  C.bI[i]  <- C1[i]+C2[i]
  eta[i] <- (N*C.bI[i]-C.dI.i-N*C.dI.n)/T[i]
}

cat("value of reward \n")
print(eta)
cat("value of reward in 10 units of time \n")
results=data.frame(T[seq(1,T.max,by=1)]
                   , C1[seq(1,T.max,by=1)]
                   , C2[seq(1,T.max,by=1)]
                   , C.bI[seq(1,T.max,by=1)]
                   , eta[seq(1,T.max,by=1)])
names(results) <- c("T","C1","C2","C.bI","eta")

cat("Maximum value of reward \n")
print(max(eta))
cat("optimal units of time to execute intervention \n")
print(which.max(eta))

### --------------------PLOT--------------------
## The following code is only for plotting the figure given as 
## a solution to the Exercise

plotres <- data.frame(1:T.max,eta)
names(plotres) <- c("T","eta")

plottau <- ggplot(data=plotres,aes(x=T,y=eta)) + 
  geom_line(size=2,color="red") + 
  xlab("Time between Int. [months]") + 
  ylab("Average cost per year [mu/month]") + # Set axis labels
  scale_x_continuous(breaks = seq(0, max(plotres$T), by = 1)) +
  ylim(0,1000) +
  theme_bw(base_size=36) #+
# theme(legend.position=c(.85, .8))           # Position legend 

# Let us collect the optimal points in the data frame optimal
optimal <- data.frame(nrow=1,ncol=2)
names(optimal) <- c("x","y")

data <- plotres
optimal[1,1] <- data$T[which.max(data$eta)]
optimal[1,2] <- max(data$eta)
optlines <- data.frame(key = c("z","z")
                       , x0 = c(min(plotres$T)
                                ,optimal$x)
                       , x1 = c(optimal$x
                                ,optimal$x)
                       , y0 = c(optimal$y,optimal$y)
                       , y1 = c(optimal$y,0)) # Same as y.min

# Now we add these points to the graph before exporting
plottau + geom_point(data=optimal
                     , aes(x=x
                           , y=y)
                     , colour="navyblue"
                     , size=6) +
  annotate("text", label="Optimal - 815.2\nT=8"
           , x=optimal[1,1]+2, y=optimal[1,2]-70
           , size=12) +
  geom_segment(aes(x = x0, y = y0
                   , xend = x1, yend = y1)
               , data = optlines
               , size = 1.5
               , color= "grey") # Also draw lines to optimal point

dev.copy(png,'E3-07_eta.png',width=2000,height=1330)
dev.off()
### ------------------PLOT END-------------------

# END

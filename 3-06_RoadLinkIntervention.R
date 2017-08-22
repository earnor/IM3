# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 3.6
# | PROBLEM NAME: Road Link Intervention
# | UPDATE: AE     
# | DESCRIPTION: The benefits of running the road links in an 
# |              adequate state are found using this program. 
# |              Given benefits of operating the road and penalties
# |              during inadequate state, we can calculate the cost 
# |              per unit time of the operation. As the cost is a 
# |              term covering both negative and positive costs, we 
# |              maximise the costs in this example to find the 
# |              optimal time between preventive interventions.
# | 
# | KEYWORDS: exponential, failure, optimisation, cost, road, link,
# |           time-dependent
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls())     # clear the memory and items in R console
library(ggplot2)
### ------------------VARIABLE DEFINITION---------------------

## First we want to define the variables used for the program.

# Indices are used for variables, bI stands for between Interventions
# as in text. Likewise, dI stands for 'during Interventions'. o 
# stands for operating, f for failed, i for every intervention and n
# for number of items.

### ---------------VARIABLE INPUT-------------------

N       <- 10   # numbers of items
C.bI.o  <- 100  # reward revenue per tu between Interventions
C.bI.f  <- -10  # penalty for failed item per tu
C.dI.i  <- 500  # fixed costs during intervention
C.dI.n  <- 10   # variable costs per failed silo during intervention.
LAMBDA  <- .01  # extracted from model, 1/100 = 0.01
T.max   <- 20

unit    <- 0.5
# we will search for optimal renewal time in range T
T       <- seq(unit*1,T.max, by=unit) 


### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program.
## Mean cost per time unit, eta, is found for the Time dependent 
## planned replacement with no unplanned interventions IS. The time 
## period that maximises the positive costs, shown with eta is the 
## optimal time T we seek, i.e. how much time should pass between PIs
## so costs are optimised.

# General and miscellaneous definitions

# Define empty matrices for models

# The cost per link if failure occurs on (0,T]
C1    <- matrix(nrow=length(T)) 
# The cost per link if no failure on (0,T]
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

for (k in (1:length(T))){
  C1[k]  <- integrate(c1Int,0,T[k] 
                      # integrating {(a*x+b*(t-x))*f(x,lambda)} 
                      , a=C.bI.o
                      , b=C.bI.f 
                      , t=T[k]
                      , lambda=LAMBDA)$val
  C2[k]  <- C.bI.o*T[k]*(1-F(T[k],LAMBDA))
  C.bI[k]  <- C1[k]+C2[k]
  eta[k] <- (N*C.bI[k]-C.dI.i-N*C.dI.n)/T[k]
}

cat("value of reward \n")
print(eta)
cat("value of reward in 10 units of time \n")
results=data.frame(T[seq(1,T.max/unit,by=1)]
                   , C1[seq(1,T.max/unit,by=1)]
                   , C2[seq(1,T.max/unit,by=1)]
                   , C.bI[seq(1,T.max/unit,by=1)]
                   , eta[seq(1,T.max/unit,by=1)])
names(results) <- c("T","C1","C2","C.bI","eta")

cat("Maximum value of reward \n")
print(max(eta))
cat("optimal units of time to execute intervention \n")
print(which.max(eta))


### --------------------PLOT--------------------
## The following code is only for plotting the figure given as 
## a solution to the Exercise

plotres <- data.frame(seq(from=1*unit,to=T.max,by=unit),eta)
names(plotres) <- c("T","eta")

plottau <- ggplot(data=plotres,aes(x=T,y=eta)) + 
  geom_line(size=2,color="red") + 
  xlab("Time between Int. [tu]") + 
  ylab("Average cost per year [mu/tu]") + # Set axis labels
  ylim(500,1000) +
  theme_bw(base_size=36) #+
# theme(legend.position=c(.85, .8))           # Position legend

# Let us collect the optimal points in the data frame optimal

optimal <- data.frame(nrow=1,ncol=2)
names(optimal) <- c("x","y")


data <- plotres
optimal[1,1] <- data$T[which.max(data$eta)]
optimal[1,2] <- max(data$eta)

# Now we add these points to the graph before exporting
plottau + geom_point(data=optimal
                     , aes(x=x
                           , y=y)
                     , colour="navyblue"
                     , size=6) +
  annotate("text", label="Optimal - 887\nT=11.0"
           , x=optimal[1,1]+0.02, y=optimal[1,2]-30
           , size=12)

dev.copy(png,'E3-06_eta.png',width=2000,height=1330)
dev.off()
### ------------------PLOT END-------------------

# END

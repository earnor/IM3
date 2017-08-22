# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 3.3a
# | PROBLEM NAME: Pipe Intervention
# | UPDATE: AE     
# | DESCRIPTION: Calculates and plots cost as a function of time 
# |              between interventions. Allows a determination of 
# |              optimal time to perform intervention. Uses Weibull
# |              distribution to calculate probability of failure.
# |              A comparison between an age-dependent model and 
# |              a time-dependent model is made.
# | 
# | KEYWORDS: Weibull, failure, optimization, cost, age-dependent, 
# |           time-dependent, pipes
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls())     # clear the memory and items in R console
library(ggplot2)
library(tidyr)
### ------------------VARIABLE DEFINITION---------------------

# lambda is the scale-parameter of the Weibull distribution
# beta is the shape-parameter of the Weibull distribution
# C.CI is the Cost of Corrective Intervention (CI) of a pipe
# C.PI is the Cost of Preventive Intervention (PI) of a pipe

# Furthermore, the index .d is given to variables relating to ductile 
# pipe and .c to cast iron, for example C.CI.d

### ---------------VARIABLE INPUT-------------------

#Cast iron pipe
lambda.c = 0.0147 
beta.c = 4.8107 
C.CI.c = 100 
C.PI.c = 30

#Ductile iron pipe
lambda.d = 0.0178
beta.d = 2.7715
C.CI.d = 90
C.PI.d = 25

### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program. Mean cost per time unit
## is marked with eta. The time period that minimises eta is the 
## optimal time T we seek, i.e. how much time should pass between 
## PIs so costs are optimised.

# Let us begin with defining matrices of size T.max that help us 
# find the solution. T.max is the maximum time period T we will 
# observe, and plot for, in this program.

# Furthermore, the index .t is given to time-dependent model, .a for 
# age-dependent model, for example eta.c.t


# General and miscellaneous definitions
# maximum investigated time between PI, defines matrix size and x-axis
T.max = 80  
# upper limit of y-axis on plots
unit = 5
# number of simulations for m(T)
N = 10000   

# Define empty matrices and function for time- and age-dependent models
# cumulative Weibull distrib. of prob. of failure
F <- matrix(nrow=T.max)
# S is survival probability, 1-F
S <- matrix(nrow=T.max) 
# m is matrix collecting mean number of failure m(T)
m <- matrix(nrow=T.max) 
# E is the matrix of expected mean time to failure
E <- matrix(nrow=T.max) 

#f is failure density function
f     = function(t,a,b){(a^b)*b*(t^(b-1))*exp(-((a*t)^b))} 
#f.int is f(x)*x used for integral
f.int = function(t,a,b){(a^b)*b*(t^b)*exp(-((a*t)^b))} 


# We lastly define Cost per unit time (CPUT) that will then be 
# graphed against T
# eta.c.t is CPUT for cast iron in time-dep. model
eta.c.t <-matrix(nrow=T.max) 
# eta.c.a is CPUT for cast iron in age-dep. model
eta.c.a <-matrix(nrow=T.max) 
# eta.d.t is CPUT for ductile iron in time-dep. model
eta.d.t <-matrix(nrow=T.max) 
# eta.d.a is CPUT for ductile iron in age-dep. model
eta.d.a <-matrix(nrow=T.max) 


### ------------------CALCULATIONS-------------------
## MATERIAL 1: Cast Iron Pipe
cat('RESULTS FOR CAST IRON PIPE \n')

# Let us calculate the amount of time an object fails in the observed 
# time period T. The number of failures is described with m(T), where T 
# is time between PIs. First we use iteration process described on 
# Figure 9 to find m.n(T). In the process, the random variable g 
# is used to determine the time length in years from time point t=0 
# to which the object (pipe) fails. This time is represented using Tn. 
# If this time is less than the time period T, the loop reiterates to 
# see if the next failure also occurs before time T. If the next Tn 
# brings the sum of all Tn to a value that exceeds time T, the loop 
# stops. The amount of failure iterations are counted using i. Once 
# the loop is exited, one is subtracted from i to count for the last 
# failure, which occurred after the time T had elapsed.

input <- matrix(data=rep(1:T.max,each=N), ncol=N, byrow=T.max)
m.calc <- function(T){
  i <- 0
  
  
  sumTn <- 0 # Sum of all failure durations, essentially sumTn=t .
  # We start at t=0
  while (sumTn<T){
    i <- i+1 # Here we count every iteration
    g <- runif(1,0,1) 
    #time to failure, T.n,i on Figure
    Tn <- ((-log(g))^(1/beta.c))*(1/lambda.c) 
    # total time elapsed after adding the generated failure time
    sumTn <- sumTn + Tn 
  }
  # Corrects that there is one less failure, than no. of iterations
  i <- i-1 
  return (i)
}

# The values for all N iterations for every T is inserted in a matrix
m.table <- apply(input,c(1,2),m.calc) #refers to m.n(T)

# Now find the average of all N results for all different values of T
# to find their respective m(T).

m.average <- apply(m.table,1,mean) # m(T)

for (T in 1: T.max){
  F[T] <- 1-exp(-((lambda.c)*T)^beta.c) 
  S[T] <- 1-F[T] # Calculated for readability of age-dep. equation.
  m[T] <- m.average[T]
  E[T] <- integrate(f.int,0,T,a=lambda.c,b=beta.c)$val
  eta.c.t[T] <- (C.PI.c+(m[T]*C.CI.c))/T 
  eta.c.a[T] <- (C.PI.c+((F[T]/S[T])*C.CI.c))/(T+(E[T]/S[T])) 
}

cat('Results of time-dependent model \n')
cat('T= \n')
print(which.min(eta.c.t))
cat('eta= \n')
print(min(eta.c.t))

cat('Results of age-dependent model \n')
cat('T= \n')
print(which.min(eta.c.a))
cat('eta= \n')
print(min(eta.c.a))

# Let us verify that the strategy of doing a PI on object every 
# T years is more cost effective than letting the object run down and
# replacing it every time it fails. For this to be true we observe 
# if the Efficiency, Q > 1. 

# We first calculate the converging value of eta, and pick 
# a large number such as 1500

eta.CI.c <- C.CI.c/integrate(f.int,0,1500,a=lambda.c,b=beta.c)$val

Q.c.t <- eta.CI.c/(min(eta.c.t))
cat('value of Q.c.t \n')

print(Q.c.t)
Q.c.a <- eta.CI.c/(min(eta.c.a))
cat('value of Q.c.a \n')
print(Q.c.a)



## MATERIAL 2: Ductile Iron Pipe
cat('RESULTS FOR DUCTILE IRON PIPE \n')

# m(T)
input <- matrix(data=rep(1:T.max,each=N), ncol=N, byrow=T.max)
m.calc <- function(T){
  i <- 0
  sumTn <- 0
  while (sumTn<=T){
    i <- i+1
    g <- runif(1,0,1)
    Tn <- ((-log(g))^(1/beta.d))*(1/lambda.d) 
    sumTn <- sumTn + Tn 
  }
  i <- i-1
  return (i)
}

m.table <- apply(input,c(1,2),m.calc) #refers to m.n(T)
m.average <- apply(m.table,1,mean) # m(T)


for (T in 1: T.max){
  F[T] <- 1-exp(-((lambda.d)*T)^beta.d) 
  S[T] <- 1-F[T] # Calculated for readability of age-dep. equation.
  m[T] <- m.average[T]
  E[T] <- integrate(f.int,0,T,a=lambda.d,b=beta.d)$val
  eta.d.t[T] <- (C.PI.d+(m[T]*C.CI.d))/T 
  eta.d.a[T] <- (C.PI.d+((F[T]/S[T])*C.CI.d))/(T+(E[T]/S[T])) 
}


# Results

cat('Results of time-dependent model \n')
cat('T= \n')
print(which.min(eta.d.t))
cat('eta= \n')
print(min(eta.d.t))

cat('Results of age-dependent model \n')
cat('T= \n')
print(which.min(eta.d.a))
cat('eta= \n')
print(min(eta.d.a))

# Efficiency
eta.CI.d <- C.CI.d/integrate(f.int,0,1500,a=lambda.d,b=beta.d)$val
Q.d.t <- eta.CI.d/(min(eta.d.t))
cat('value of Q.d.t \n')
print(Q.d.t)
Q.d.a <- eta.CI.d/(min(eta.d.a))
cat('value of Q.d.a \n')
print(Q.d.a)

val <- c(10,15,20,25,28,30,33,35,40,42,43,45,47,50,55)
r1 <- matrix(eta.c.t[val])
r2 <- matrix(eta.c.a[val])
r3 <- matrix(eta.d.t[val])
r4 <- matrix(eta.d.a[val])
results <- as.data.frame(cbind(val,r1,r2,r3,r4))
names(results) <- c("T","Cast.TI","Cast.AI","Duct.TI","Duct.AI")
results

### --------------------PLOT--------------------

plotres <- data.frame(1:T.max,eta.c.t,eta.c.a,eta.d.t,eta.d.a)
names(plotres) <- c("T","Cast.TI","Cast.AI","Duct.TI","Duct.AI")

resultstidy <- gather(plotres
                      ,Cast.TI,Cast.AI,Duct.TI,Duct.AI
                      ,key="key",value="value")

ploteta <- ggplot(data=resultstidy,aes(x=T,y=value
                                       ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Pipe and Model type",      
                   breaks=c("Cast.TI","Cast.AI","Duct.TI","Duct.AI"),
                   labels=c("\nCast iron - Time dep.\n"
                            , "\nCast iron - Age dep.\n"
                            , "\nDuctile iron - Time dep.\n"
                            , "\nDuctile iron - Age dep.\n")
  )  +                  
  scale_linetype_discrete(name="Pipe and Model type") +
  xlab("Time between Int. [years]") + 
  ylab("Average cost per year [10^3 mu/yr]") + # Set axis labels
  ylim(0,5) +
  theme_bw(base_size=36) + 
  theme(legend.position=c(.85, .8))           

# Let us collect the optimal points in the data frame optimal

optimal <- data.frame(NA,nrow=4,ncol=3)
names(optimal) <- c("key","x","y")
type <- c("Cast.TI","Cast.AI","Duct.TI","Duct.AI")

for (n in 1:4)
{
  data <- resultstidy[(1+(T.max*(n-1))):(n*T.max),]
  optimal[n,1] <- type[n]
  optimal[n,2] <- data$T[which.min(data$value)]
  optimal[n,3] <- min(data$value)
}
# Now we add these points to the graph before exporting
ploteta + geom_point(data=optimal
                     , aes(x=x
                           , y=y)
                     , colour="navyblue"
                     , size=6)

dev.copy(png,'E3-03a_eta.png',width=2000,height=1330)
dev.off()
# The time-dependent lines seem unstable because they 
# are based on random number generation

# ------------------PLOT END-------------------

# END

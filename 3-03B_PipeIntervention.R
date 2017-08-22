# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 3.3b
# | PROBLEM NAME: Pipe Intervention
# | UPDATE: AE     
# | DESCRIPTION: This program attempts to build on example 3.3a, and
# |              looks at different values for beta and C.PI. As 
# |              models include large uncertainties, this program 
# |              shows how results change using different values,
# |              and using different decision rules, namely
# |              average and the maxmin principle.
# | 
# | KEYWORDS: Weibull, failure, decision, rule, age-dependent, 
# |           time-dependent, pipes
# *-----------------------------------------------------------------

# PLEASE NOTE: The running of this program takes a few moments, 
#              please be patient.

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls())     # clear the memory and items in R console
library(ggplot2)
library(tidyr)
### ------------------VARIABLE DEFINITION---------------------

## First we want to define the variables used for the program. 

# We use the indices .c for cast-iron pipes, .d for ductile iron 
# pipes. We use .min and .max for the minimum and maximum values of 
# a range. .bas is the base value.

### ---------------VARIABLE INPUT-------------------

# Cast iron

lambda.c <- 0.0147
beta.c.bas <- 4.8107
beta.c.min <- 4.570
beta.c.max <- 5.051

C.CI.c <- 100
C.PI.c.bas <- 30
C.PI.c.min <- 10
C.PI.c.max <- 33.3

# Ductile iron

lambda.d <- 0.0178
beta.d <- 2.7715

C.CI.d <- 90
C.PI.d.bas <- 25
C.PI.d.min <- 9
C.PI.d.max <- 30

### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program. 
## The time period that minimises eta is the optimal 
## time T we seek, i.e. how much time should pass between PIs 
## so costs are optimised.

# .a is used to label age-dependent model results and .t for time
# -dependent. .r1 and .r2 are used to differentiate between decision 
# rules 1 and 2. .avg stands for average

# General and miscellaneous definitions
T.max <- 80 #investigate time frame
xunit <- 0
unit  <- 3 # the plotted upper limit of the y axis.
N     <- 10000 # number of simulations for m(T)

# Define empty matrices and functions for time- and age-dependent
# models

# F is the cumulative Weibull distrib. of prob. of failure
F       <- matrix(nrow=T.max) 
Ff      = function(T,a,b){1-exp(-((a*T)^b))}
# S is survival probability, 1-F
S       <- matrix(nrow=T.max) 
# m collects mean number of failure m(T)
m.c     <- matrix(nrow=T.max,ncol=4) 
m.d     <- matrix(nrow=T.max,ncol=2) 
#f is failure density function
f       = function(t,a,b){(a^b)*b*(t^(b-1))*exp(-((a*t)^b))} 
#f.int is f(x)*x used for integration 
f.int   = function(t,a,b){(a^b)*b*(t^(b))*exp(-((a*t)^b))} 

# E is matrix of expected mean time to failure
E.c     <- matrix(nrow=T.max,ncol=4) 
E.d     <- matrix(nrow=T.max,ncol=2) 
# Used to collect products of m.n(T) generator
m.calc  <- matrix(nrow=T.max,ncol=N) 
# Used to collect mean life time over long time
mu.c      <- matrix(ncol=4)           
# Used to collect eta if only CIs performed
eta.CI.c  <- matrix(ncol=4)            
# Used to collect mean life time over long time
mu.d      <- matrix(ncol=4)            
# Used to collect eta if only CIs performed
eta.CI.d  <- matrix(ncol=4)            

# Now define matrices that will contain the calculated cost per tu,
# and their mean
eta.c.a     <- matrix(nrow=T.max,ncol=4)
eta.c.a.avg <- matrix(nrow=T.max)
eta.c.t     <- matrix(nrow=T.max,ncol=4)
eta.c.t.avg <- matrix(nrow=T.max)

eta.d.a     <- matrix(nrow=T.max,ncol=2)
eta.d.a.avg <- matrix(nrow=T.max)
eta.d.t     <- matrix(nrow=T.max,ncol=2)
eta.d.t.avg <- matrix(nrow=T.max)

# To find minimum values of calculated etas
mineta.c.a <- matrix(ncol=4)
mineta.c.t <- matrix(ncol=4)
mineta.d.a <- matrix(ncol=2)
mineta.d.t <- matrix(ncol=2)

### ------------------CALCULATIONS-------------------

## Cast-iron pipes

# We define the variables of the combinations
beta.c <- c(beta.c.min, beta.c.min, beta.c.max, beta.c.max)
C.PI.c <- c(C.PI.c.min, C.PI.c.max, C.PI.c.min, C.PI.c.max)

# We start a loop, which will perform the calculations for every
# combination.
for (k in 1:4){
  # We generate values of m.n(T) and subsequently m(T) using the
  # variables above.
  input <- matrix(data=rep(1:T.max,each=N), ncol=N, byrow=T.max)
  m.calc = function(T){
    i <- 0
    # Sum of all failure durations, essentially sumTn=t .
    sumTn <- 0 # We start at t=0
    while (sumTn<T){
      i <- i+1 # Here we count every iteration
      g <- runif(1,0,1)
      # time to failure, T.n,i on Figure
      Tn <- ((-log(g))^(1/beta.c[k]))*(1/lambda.c) 
      # total time elapsed after adding the generated failure time
      sumTn <- sumTn + Tn 
    }
    # Corrects that there is one less failure, than no. of iterations
    i <- i-1 
    return (i)
  }
  m.table <- apply(input,c(1,2),m.calc) #refers to m.n(T)
  m.average <- apply(m.table,1,mean) # m(T)
  m.c[,k] = m.average
  
  for (T in 1:T.max){
    F[T]=Ff(T,lambda.c,beta.c[k])
    S[T] <- 1-F[T]
    E.c[T,k] <- integrate(f.int,0,T,a=lambda.c,b=beta.c[k])$val
    eta.c.t[T,k] <- (C.PI.c[k]+(m.c[T,k]*C.CI.c))/T
    eta.c.t[T.max,k] <- eta.c.t[T.max-1,k] 
    eta.c.a[T,k] <- (C.PI.c[k]+(F[T]/(1-F[T])*C.CI.c))/
      (T+(E.c[T,k]/(1-F[T])))
    eta.c.a[T.max,k] <- eta.c.a[T.max-1,k] 
  }
  # Let us estimate the expected costs if only CIs were executed
  # We first find average mean life time found over large period, 
  # say 1500
  mu.c[k] <- integrate(f.int,0,1500,a=lambda.c,b=beta.c[k])$val
  eta.CI.c[k] <- C.CI.c/mu.c[k]
  
  # Average of the combinations
  for (T in 1: T.max){
    eta.c.a.avg[T] <- mean(eta.c.a[T,])
    eta.c.t.avg[T] <- mean(eta.c.t[T,])
  }
  
  # Let us verify that the strategy of doing a PI on object every T 
  # years is more cost effective than letting the object run down 
  # and replacing it every time it fails. For this to be true we
  # observe if the Efficiency, Q > 1. 
  
  # Let us create a vector of minimum values of the different 
  # combinations
  mineta.c.a[k] <- min(eta.c.a[,k])
  mineta.c.t[k] <- min(eta.c.t[,k])
  
} # Here the first for-loop ends.


# We now compare costs of strategies, with that of the no 
# intervention strategy to verify that it is worth it to execute PIs.

# decision rule 1, all values are of equal weight
Q.c.a.r1 <- eta.CI.c[2]/min(eta.c.a.avg) 
# decision rule 2, using maxmin principle.
Q.c.a.r2 <- eta.CI.c[2]/max(mineta.c.a) 

# decision rule 1, all values are of equal weight
Q.c.t.r1 <- eta.CI.c[2]/min(eta.c.t.avg)
# decision rule 2, using maxmin principle.
Q.c.t.r2 <- eta.CI.c[2]/max(mineta.c.t) 


cat('&&&&&  AGE-DEPENDENT REPLACEMENT &&&&&&&&&&&&&&&&&&&&&&&&&& \n')
cat('combination 1 \n')
print(which.min(eta.c.a[,1]))
print(mineta.c.a[1])

cat('combination 2 \n')
print(which.min(eta.c.a[,2]))
print(mineta.c.a[2])

cat('combination 3 \n')
print(which.min(eta.c.a[,3]))
print(mineta.c.a[3])

cat('combination 4 \n')
print(which.min(eta.c.a[,4]))
print(mineta.c.a[4])

cat('average \n')
print(which.min(eta.c.a.avg))
print(min(eta.c.a.avg))

cat('value of Q.c.a for DR 1 and 2 \n')
print(Q.c.a.r1)
print(Q.c.a.r2)


cat('&&& TIME-DEPENDENT REPLACEMENT &&&&&&&&&&&&&&&&&&&&&&&&&&& \n')
cat('combination 1 \n')
print(which.min(eta.c.t[,1]))
print(mineta.c.t[1])

cat('combination 2 \n')
print(which.min(eta.c.t[,2]))
print(mineta.c.t[2])

cat('combination 3 \n')
print(which.min(eta.c.t[,3]))
print(mineta.c.t[3])

cat('combination 4 \n')
print(which.min(eta.c.t[,4]))
print(mineta.c.t[4])

cat('average \n')
print(which.min(eta.c.t.avg))
print(min(eta.c.t.avg))

cat('value of Q.c.t for DR 1 and 2 \n')
print(Q.c.t.r1)
print(Q.c.t.r2)


## Ductile iron pipes

# We define the variables of the combinations
C.PI.d <- c(C.PI.d.min,C.PI.d.max)


for (k in 1:2){
  # m(T)
  input <- matrix(data=rep(1:T.max,each=N), ncol=N, byrow=T.max)
  m.calc = function(T){
    i <- 0
    # Sum of all failure durations, essentially sumTn=t .
    sumTn <- 0 #  We start at t=0
    while (sumTn<T){
      i <- i+1 # Here we count every iteration
      g <- runif(1,0,1) 
      #time to failure, T.n,i on Figure 
      Tn <- ((-log(g))^(1/beta.d))*(1/lambda.d) 
      # total time elapsed after adding the generated failure time
      sumTn <- sumTn + Tn 
    }
    # Corrects that there is one less failure, than no. of iterations
    i <- i-1 
    return (i)
  }
  m.table <- apply(input,c(1,2),m.calc) #refers to m.n(T)
  m.average <- apply(m.table,1,mean) # m(T)
  m.d[,k] = m.average
  
  for (T in 1:T.max){
    F[T]=Ff(T,lambda.d,beta.d)
    S[T] <- 1-F[T]
    E.d[T,k] <- integrate(f.int,0,T,a=lambda.d,b=beta.d)$val
    eta.d.t[T,k] <- (C.PI.d[k]+(m.d[T,k]*C.CI.d))/T
    eta.d.t[T.max,k] <- eta.d.t[T.max-1,k] 
    eta.d.a[T,k] <- (C.PI.d[k]+(F[T]/(1-F[T])*C.CI.d))/
      (T+(E.d[T,k]/(1-F[T])))
    eta.d.a[T.max,k] <- eta.d.a[T.max-1,k] 
  }
  
  
  # Average of the Combinations
  for (T in 1: T.max){
    eta.d.a.avg[T] <- mean(eta.d.a[T,])
    eta.d.t.avg[T] <- mean(eta.d.t[T,])
  }
  
  mu.d[k] <- integrate(f.int,0,1500,a=lambda.d,b=beta.d)$val
  eta.CI.d[k] <- C.CI.d/mu.d[k]
  
  mineta.d.a[k] <- min(eta.d.a[,k])
  mineta.d.t[k] <- min(eta.d.t[,k])
}

# We now compare costs of strategies, with that of the no 
# intervention strategy to verify that it is worth it to execute PIs.

Q.d.a.r1 <- eta.CI.d[2]/min(eta.d.a.avg) 
Q.d.a.r2 <- eta.CI.d[2]/max(mineta.d.a)  

Q.d.t.r1 <- eta.CI.d[2]/min(eta.d.t.avg) 
Q.d.t.r2 <- eta.CI.d[2]/max(mineta.d.t)  


### --------------------PLOT--------------------

results.time <- data.frame(1:T.max,eta.c.t)
results.time <- cbind(results.time,eta.c.t.avg)
names(results.time) <- c("T"
                         ,"Combination.1","Combination.2"
                         ,"Combination.3","Combination.4"
                         ,"Average")

timetidy <- gather(results.time
                   ,Combination.1,Combination.2
                   ,Combination.3,Combination.4,Average
                   ,key="key",value="value")

results.age <- data.frame(1:T.max,eta.c.a)
results.age <- cbind(results.age,eta.c.a.avg)
names(results.age) <- c("T"
                        ,"Combination.1","Combination.2"
                        ,"Combination.3","Combination.4"
                        ,"Average")

agetidy <- gather(results.age
                  ,Combination.1,Combination.2
                  ,Combination.3,Combination.4,Average
                  ,key="key",value="value")

# Now we create each plot

plottime <- ggplot(data=timetidy,aes(x=T,y=value
                                     ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Time-dependent",      # Set legend title
                   breaks=c("Combination.1"
                            ,"Combination.2"
                            ,"Combination.3"
                            ,"Combination.4"
                            ,"Average"),
                   labels=c("\nCombination 1\n"
                            , "\nCombination 2\n"
                            , "\nCombination 3\n"
                            , "\nCombination 4\n"
                            , "\nAverage\n")
  )  +                  
  xlab("Time between Int. [years]") + 
  ylab("Average cost per year [10^3 mu/yr]") + # Set axis labels
  ylim(0,5) +
  theme_bw(base_size=36) + 
  theme(legend.position=c(.85, .8))          

# Let us collect the optimal points in the data frame optimal

optimal <- data.frame(NA,nrow=5,ncol=3)
names(optimal) <- c("key","x","y")
type <- c("Combination.1","Combination.2"
          ,"Combination.3","Combination.4","Average")

for (n in 1:5)
{
  data <- timetidy[(1+(T.max*(n-1))):(n*T.max),]
  optimal[n,1] <- type[n]
  optimal[n,2] <- data$T[which.min(data$value)]
  optimal[n,3] <- min(data$value)
}
# Now we add these points to the graph before exporting
plottime + geom_point(data=optimal
                      , aes(x=x
                            , y=y)
                      , colour="navyblue"
                      , size=6)

dev.copy(png,'E3-03b_time.png',width=2000,height=1330)
dev.off()

plotage <- ggplot(data=agetidy,aes(x=T,y=value
                                   ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Age-dependent",      # Set legend title
                   breaks=c("Combination.1"
                            ,"Combination.2"
                            ,"Combination.3"
                            ,"Combination.4"
                            ,"Average"),
                   labels=c("\nCombination 1\n"
                            , "\nCombination 2\n"
                            , "\nCombination 3\n"
                            , "\nCombination 4\n"
                            , "\nAverage\n")
  )  +                  
  # scale_linetype_discrete(name="Pipe and Model type") +
  xlab("Time between Int. [years]") + 
  ylab("Average cost per year [10^3 mu/yr]") + # Set axis labels
  ylim(0,5) +
  theme_bw(base_size=36) + 
  theme(legend.position=c(.85, .8))           # Position legend
# Let us collect the optimal points in the data frame optimal

optimal <- data.frame(NA,nrow=5,ncol=3)
names(optimal) <- c("key","x","y")
type <- c("Combination.1","Combination.2"
          ,"Combination.3","Combination.4","Average")

for (n in 1:5)
{
  data <- agetidy[(1+(T.max*(n-1))):(n*T.max),]
  optimal[n,1] <- type[n]
  optimal[n,2] <- data$T[which.min(data$value)]
  optimal[n,3] <- min(data$value)
}
# Now we add these points to the graph before exporting
plotage + geom_point(data=optimal
                     , aes(x=x
                           , y=y)
                     , colour="navyblue"
                     , size=6)

dev.copy(png,'E3-03b_age.png',width=2000,height=1330)
dev.off()


### ------------------PLOT END-------------------

# END

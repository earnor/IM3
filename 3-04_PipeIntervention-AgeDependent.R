# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 3.4
# | PROBLEM NAME: Pipe Intervention
# | UPDATE: AE     
# | DESCRIPTION: In this problem a Pipe Intervention is considered 
# |              in terms of optimisation of costs. A
# |              Weibull distribution is used to model the
# |              probability of failure. 
# | 
# | KEYWORDS: Weibull, failure, optimization, cost, age-dependent, 
# |           pipes, 
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls())     # clear the memory and items in R console
library(ggplot2)
### ------------------VARIABLE DEFINITION---------------------

# C.CI is the Cost of a Corrective Intervention (CI)
# C.PI is the Cost of a Preventive Intervention (PI)
# t.CI is the Down-Time needed for execution of a Corrective
# Intervention (CI)
# t.PI is the Down-Time needed for execution of a Preventive
# Intervention (PI)
# tau is the total expected down time per time unit. We try to
# maximise tau.

### ---------------VARIABLE INPUT-------------------

C.CI  <- 100    # cost for corrective intervention
C.PI  <- 1      # cost for preventive intervention
mt    <- 1000   # mean lifetime in cycles of operation. 
beta  <- 3      # shape parameter of Weibull distribution function
p.max <- 0.01   # within (0,T] the failure prob. cannot exceed p.max
T.max <- 1000

### ---------------PROGRAM OUTPUT-------------------

## Outputs of our program are similar to those in previous programs. 
## Here we also collate the probabilities of failure at every year, 
## to verify that the probability p[t] does not exceed 0.01

# Define empty matrices and function

F <- matrix(nrow=T.max) # cumulative distribution
S <- matrix(nrow=T.max) # survival probability
E <- matrix(nrow=T.max) # expected lifetime of pipe
p <- matrix(nrow=T.max) # Vector of prob. until they exceed 0.01
eta <- matrix(nrow=T.max) # cost per unit of time 
survi = function(t,a,b){exp(-(a*t)^b)} # survival function, 1-F

### ------------------CALCULATIONS-------------------

# We first calculate lambda for the Weibull probability.
lambda <- gamma(1+1/beta)/mt

# AGE REPLACEMENT MODEL - minimise cost & maximise availability

for (T in 1:T.max){
  F[T] <- 1-exp(-(lambda*T)^beta)
  S[T] <- 1-F[T]
  E[T] <- integrate(survi,0,T,a=lambda,b=beta)$val
  eta[T] <- (F[T]*C.CI+S[T]*C.PI)/E[T] # cost per unit time
  # For problem 3.5 we find maximum availability
  p.temp <- F[T] # want to see if the prob. is larger than 0.01
  if(p.temp<p.max)
  {
    p[T] <- F[T];
  } 
  else 
  {
    p[T] <- 0 
    # if larger than 0.01, we make the prob. 0. This way we find the
    # last chance for Infr.Manager to intervene before p>0.01
  }
}

cat('Estimation results \n')
result=data.frame(F,S,E,eta)
cat('Time at which yielding the minimal cost per unit of time \n')
print(which.min(eta))
cat('Time at which the probability of failure exceeds 0.01 \n')
print(which.max(p))

# If time of probability exceeding 0.01 is larger than optimal time,
# we print OK
if (which.max(p)>which.min(eta)){ 
  print('OK')
  cat('Minimal cost per unit time \n')
  print(min(eta))
} else {
  print('minimal cost displayed is reached when failure probability
        exceeds 0.01')
}


### --------------------PLOT--------------------
## The following code is only for plotting the figure given as 
## a solution to the Exercise


plotres <- data.frame(1:T.max,eta)
names(plotres) <- c("T","eta")

ploteta <- ggplot(data=plotres,aes(x=T,y=eta),color="red") + 
  geom_line(size=2,color="red") + 
  xlab("Time between Int. [tu]") + 
  ylab("Average cost per year [mu/tu]") + # Set axis labels
  ylim(0,0.05) +
  theme_bw(base_size=36)

# Let us collect the optimal points in the data frame optimal

optimal <- data.frame(nrow=1,ncol=2)
names(optimal) <- c("x","y")


data <- plotres
optimal[1,1] <- data$T[which.min(data$eta)]
optimal[1,2] <- min(data$eta)

# Now we add these points to the graph before exporting
ploteta + geom_point(data=optimal
                     , aes(x=x
                           , y=y)
                     , colour="navyblue"
                     , size=6) +
  annotate("text", label="Optimal - 0.0078\nT=192"
           , x=optimal[1,1]+0.02, y=optimal[1,2]-0.003
           , size=12)

dev.copy(png,'E3-04_eta.png',width=2000,height=1330)
dev.off()

### ------------------PLOT END-------------------

# END

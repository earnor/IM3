# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 2.1
# | PROBLEM NAME: Bridge Girder Renewal
# | UPDATE: AE     
# | DESCRIPTION: With the choice of two different strategies, and 
# |              two models, Weibull and an exponential model, 
# |              suggested by either consultant, we analyse both 
# |              strategies for both models.
# | 
# | KEYWORDS: Weibull, exponential, cost, renewal, 
# |           
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls())     # clear the memory and items in R console
library(ggplot2)  # load ggplot graphics package.
library(tidyr)

# The working directory must be set to wherever you keep your files
setwd("Y:/common/IBI/Martani/IM Script - Workspace/Exercises/R-files/Updated")
### ------------------VARIABLE DEFINITION---------------------

## First we want to define the variables used for the program. They 
## are extracted from the table in the problem.

# obj is a vector entailing the names of the girders.
# lambda is a vector of the scale-parameter of the Weibull
# distribution, l
# beta is a vector of the shape-parameter of the Weibull 
# distribution, b
# alpha is a vector of the rate-parameter of the exponential 
# distribution, r
# C.CI is a vector of the Cost of Corrective Intervention (CI) 
# C.PI is a vector of the Cost of Preventive Intervention (PI) 
# data is a table of all the above-mentioned values

### ---------------VARIABLE INPUT-------------------

obj     <- c("A","B")            # We set up girders A and B
C.CI    <- c(100000,100000)      # and their intervention costs
C.PI    <- c(3000,3000)
alpha   <- c(1.129,2.257)        # their deterioration parameters
lambda  <- c(1,2)
beta    <- c(2,2)

# the length of the input string, i.e. granularity of input
l     <- 200     

data  <- data.frame(obj,C.CI,C.PI,alpha,lambda,beta)
print(data)

### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program. 

## In this program, the goal is to decide between two strategies for 
## each of the type of distribution. Strategy 1 suggests to only 
## perform CI on the girderswhenever they show signs of failure. 
## Strategy 2 however suggests to perform aCI on the failed girder, 
## and a PI on the other, although it does not show signs of failure

# The indices .e and .w are used to differentiate between the 
# exponential and Weibull distributions suggested by the different 
# consultants, for example F.e.
# .a and .b will also be used to differentiate between the girders 

# General and miscellaneous definitions

N     <- length(obj) # number of girders to analyse 
T.max <- 100 # here the time-frame of the investigation is determined

# Consultant 1: Weibull distribution

# For Strategy 1
# Weibull CDF. Prob of failure.
F.w   =   function(x,l,b){(1-exp((-(x*l)^(b))))}	
# Survival, 1-F, Weibull, integrated
S.w   =   function(x,l,b){(1-F.w(x,l,b))}      
# Build this matrix to hold the mu values
mu.w  <-  matrix(nrow=T.max,ncol=N)               

# For Strategy 2
# The integral for Strategy 2 is composed of the multiples of the two
# Survival functions for each girder
f.w   = function(x,l.a,b.a,l.b,b.b)
  {
  exp(-(x*l.a)^b.a)*exp(-(x*l.b)^b.b)
  } # Integral
mu0.w <- matrix(nrow=T.max)

# To compare strategies, we set the results of cost per unit time in
# same matrix.
eta.w <- matrix(nrow=2)   # a row for every strategy

# Consultant 2: Exponential distribution

# Strategy 1
F.e   <- function(x,r){1-exp(-(r*x))}	# exponential CDF
S.e   <- function(x,r){(1-F.e(x,r))}  # Survival for exp model
mu.e  <- matrix(nrow=T.max,ncol=N) 

# Strategy 2
f.e   = function(x,r.a,r.b){exp(-(x*r.a))*exp(-(x*r.b))} 
mu0.e <- matrix(nrow=T.max)

# Cost per unit Time
eta.e <- matrix(nrow=2) 


# Lastly we create vectors including the results 

f.a.e <- matrix(nrow=l)
f.a.w <- matrix(nrow=l)
f.b.e <- matrix(nrow=l)
f.b.w <- matrix(nrow=l)
F.a.e <- matrix(nrow=l)
F.a.w <- matrix(nrow=l)
F.b.e <- matrix(nrow=l)
F.b.w <- matrix(nrow=l)

### ------------------CALCULATIONS-------------------

# We start off by calculating mu, the average time between failures.
# We further assume that mu has converged at T.max.

for (t in 1:T.max){
  for (i in 1:N){
    # Strategy 1, first column is for Girder A, second for Girder B
    mu.w[t,i] <- integrate(S.w,0,t,data$lambda[i],data$beta[i])$val 
    mu.e[t,i] <- integrate(S.e,0,t,data$alpha[i])$val	
  }
  # Strategy 2
  mu0.w[t] <- integrate(f.w,0,t
                        ,lambda[1],beta[1],lambda[2],beta[2])$val
  mu0.e[t] <- integrate(f.e,0,t,alpha[1],alpha[2])$val
}

#Strategy 1
cat("Convergent values of girders´ expected mean time to failure,
    Weibull\n") 
print(mu.w[T.max,])
cat("Convergent values of girders´ expected mean time to failure,
    Exponential\n") 
print(mu.e[T.max,])

# Strategy 2
cat("Convergent value of expected mean time to failure 
    - Weibull \n") 
print(mu0.w[T.max])
cat("Convergent value of expected mean time to failure 
    - Exponential \n") 
print(mu0.e[T.max])

## Now to calculate the Cost per Unit Time, with the assumption mu 
## has converged at T.max

cat("Estimation results for each strategy 
    - Weibull distribution\n")
eta.w[1,1] <- data$C.CI[1]*(1/mu.w[T.max,1]+1/mu.w[T.max,2]) 
eta.w[2,1] <- (data$C.CI[1]+data$C.PI[1])*(1/(mu0.w[T.max])) 
print(eta.w)

cat("Estimation results for each strategy 
    - Exponential distribution \n")
eta.e[1,1] <- data$C.CI[1]*(1/mu.e[T.max,1]+1/mu.e[T.max,2])
eta.e[2,1] <- (data$C.CI[1]+data$C.PI[1])*(1/(mu0.e[T.max]))
print(eta.e)

cat("Time unit Costs of Strategies for each model \n")
result    <- data.frame("Cons1:Weibull"=eta.w
                        ,"Cons2:Exponential"=eta.e)
print(result)

### --------------------PLOT--------------------
## The following code is only for plotting the figure given as 
## a solution to the Exercise

tmin <- 0
tmax <- 4
t=seq(tmin,tmax,length=l)
for(n in 1:l)
{
  x <- t[n]
  f.a.e[n] <- data$alpha[1]*exp(-(data$alpha[1]*x))*tmax/l
  f.a.w[n] <- ((data$lambda[1])^(data$beta[1]))*data$beta[1]*
    (x^(data$beta[1]-1))*
    exp(-((x*data$lambda[1])^(data$beta[1])))*tmax/l
  
  f.b.e[n] <- data$alpha[2]*exp(-(data$alpha[2]*x))*tmax/l
  f.b.w[n] <- ((data$lambda[2])^(data$beta[2]))*data$beta[2]*
    (x^(data$beta[2]-1))*
    exp(-((x*data$lambda[2])^(data$beta[2])))*tmax/l
  F.a.e[n] <- 1-exp(-(data$alpha[1]*x))
  F.a.w[n] <- 1-exp(-((x*data$lambda[1])^(data$beta[1])))
  F.b.e[n] <- 1-exp(-(data$alpha[2]*x))
  F.b.w[n] <- 1-exp(-((x*data$lambda[2])^(data$beta[2])))
}
results <- data.frame(t
                      ,f.a.e,f.a.w,f.b.e,f.b.w
                      ,F.a.e,F.a.w,F.b.e,F.b.w)
resultstidy <- gather(results
                      ,f.a.e,f.a.w,f.b.e,f.b.w
                      ,F.a.e,F.a.w,F.b.e,F.b.w
                      ,key="key",value="value")
resultsf <- resultstidy[1:(nrow(resultstidy)/2),]
resultsF <- resultstidy[(nrow(resultstidy)/2)+1:nrow(resultstidy),]

plotpdf <- ggplot(data=resultsf
                  , aes(x=t,y=value,group=key,color=key)) +
  theme_bw(base_size=36) +
  geom_line(size=2) + 
  scale_colour_discrete(name="Girder and distribution", 
                        breaks=c("f.a.e", "f.a.w", "f.b.e", "f.b.w"),
                        labels=c("\nA - Exponential\n"
                                 , "\nA - Weibull\n"
                                 , "\nB - Exponential\n"
                                 , "\nB - Weibull\n")
  )  +                  
  scale_linetype_discrete(name="Girder and distribution") +
  xlab("Time [tu]") + ylab("Failure probability") + # Set axis labels
  theme(legend.position=c(.7, .65))           # Position legend
plotpdf

plotcdf <- ggplot(data=resultsF
                  , aes(x=t,y=value,group=key,color=key)) +
  geom_line(size=2) + 
  theme_bw(base_size=36) +
  scale_colour_hue(name="Girder and distribution",      
                   breaks=c("F.a.e", "F.a.w", "F.b.e", "F.b.w"),
                   labels=c("\nA - Exponential\n"
                            , "\nA - Weibull\n"
                            , "\nB - Exponential\n"
                            , "\nB - Weibull\n")
  )  +                 
  scale_linetype_discrete(name="Girder and distribution") +
  xlab("Time [tu]") + 
  ylab("Cumulated failure probability") + # Set axis labels
  theme(legend.position=c(.7, .3))           # Position legend
plotcdf

# Let us export the figures
plotpdf
dev.copy(png,'E2-01_pdf.png',width=2000,height=1330)
dev.off()
plotcdf
dev.copy(png,'E2-01_cdf.png',width=2000,height=1330)
dev.off()

### ------------------PLOT END-------------------

# END

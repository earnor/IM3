# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 3.8     
# | PROBLEM NAME: Pothole Repair 
# | UPDATE: AE             
# | DESCRIPTION: We simulate the pothole occurrence on a road 
# |              segment using Weibull distribution.
# |              
# | 
# | KEYWORDS: Weibull, failure, pothole, cost, age-dependent, 
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls())     # clear the memory and items in R console
library(ggplot2)
### ------------------VARIABLE DEFINITION---------------------

### ---------------VARIABLE INPUT-------------------

C.CI  <- 90      # cost of corrective intervention
C.PI  <- 1000    # cost of preventive intervention

T.max   <- 20
beta    <- 3.25
lambda  <- 0.025

N <- 10000 # number of simulations for calculating m

dim <- 7*100 # we know the dimensions of a 100 m road section, 7 m wide

### ---------------PROGRAM OUTPUT-------------------



# Define empty matrices

eta     <- matrix(nrow=T.max) 
eta.PI  <- matrix(nrow=T.max) 
eta.CI  <- matrix(nrow=T.max) 
# Used to collect products of m.n(T) generator
m.calc  <- matrix(nrow=T.max,ncol=N) 
m       <- matrix(nrow=T.max) # m collects mean number of failure m(T)

### ------------------CALCULATIONS-------------------

# Let us calculate the amount of time an object fails in the observed 
# time period T. The number of failures is described with m(T), where 
# T is time between PIs. First we use iteration process described in 
# the Theory to find m.n(T). In the process, the random variable g is
# used to determine the time length in years from time point t=0 to 
# which the object fails. This time is represented using Tn. If
# this time is less than the time period T, the loop reiterates to see
# if the next failure also occurs before time T. If the next Tn brings
# the sum of all Tn to a value that exceeds time T, the loop stops. 
# The amount of failure iterations are counted using i. Once the loop 
# is exited, one is subtracted from ito count for the last failure, 
# which occurred after the time T had elapsed.

input <- matrix(data=rep(1:T.max,each=N), ncol=N, byrow=T.max)
m.calc <- function(T){
  i <- 0
  # Sum of all failure durations, essentially sumTn=t .
  sumTn <- 0 # We start at t=0
  while (sumTn<T){
    i <- i+1 # Here we count every iteration
    g <- runif(1,0,1) 
    # time to failure, T.n,i on Figure
    Tn <- ((-log(g))^(1/beta))*(1/lambda) 
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
m = m.average

for (T in (1:T.max))
{
  m[T]        <- m[T]*dim
  eta.CI[T]   <- m[T]*C.CI/T
  eta.PI[T]   <- C.PI/T
  eta[T]      <- eta.CI[T]+eta.PI[T]
}
mineta <- min(eta)


cat("Results \n")
cat("Minimum value of cost \n")
print(min(eta))
cat("optimal units of time to execute intervention \n")
print(which.min(eta))

### --------------------PLOT--------------------
## The following code is only for plotting the figure given as 
## a solution to the Exercise

plotres <- data.frame(1:T.max,eta)
names(plotres) <- c("T","eta")

plottau <- ggplot(data=plotres,aes(x=T,y=eta)) + 
  geom_line(size=2,color="red") + 
  xlab("Time between PI [years]") + 
  ylab("Average cost per year [mu/yr]") + 
  scale_x_continuous(breaks = seq(0, max(plotres$T), by = 1)) +
  ylim(0,500) +
  theme_bw(base_size=36) 

# Let us collect the optimal points in the data frame optimal

optimal <- data.frame(nrow=1,ncol=2)
names(optimal) <- c("x","y")

data <- plotres
optimal[1,1] <- data$T[which.min(data$eta)]
optimal[1,2] <- min(data$eta)
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
  geom_segment(aes(x = x0, y = y0
                   , xend = x1, yend = y1)
               , data = optlines
               , size = 1.5
               , color= "grey") # Also draw lines to optimal point

dev.copy(png,'E3-08_eta.png',width=2000,height=1330)
dev.off()
### ------------------PLOT END-------------------

# END


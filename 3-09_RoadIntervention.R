# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 3.9     
# | PROBLEM NAME: Road Intervention
# | UPDATE: AE             
# | DESCRIPTION: We simulate the pothole occurrence on a road 
# |              segment using Weibull distribution. We apply
# |              a repair and replacement with unplanned 
# |              Corrective repair replacement strategy.
# |
# | KEYWORDS: Weibull, failure, pothole, cost, age-dependent, 
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls())     # clear the memory and items in R console
library(ggplot2)
library(tidyr)
### ------------------VARIABLE DEFINITION---------------------

# C is cost, CI stands for corrective intervention PI for 
# Preventive intervention and PI1 for repair and PI2 for replacement

# K is the number of PIs performed before executing replacement PI
# T.k is the time between PIs regardless whether they are repair or 
# replacement.
# T is the time between replacement PIs

### ---------------VARIABLE INPUT-------------------

C.CI  <- 15     # cost of corrective intervention
C.PI1 <- 500    # cost of preventive intervention
C.PI2 <- 1000   # cost of preventive intervention

T.k.max <- 3      # The maximum value for the scope of the problem
K.max   <- 20
beta    <- 3.25
lambda  <- 0.025

alpha <- 0.03

N <- 10000 # number of simulations for calculating m

# we know the dimensions of a 100 m road section, 7 m wide
dim <- 7*100 

### ---------------PROGRAM OUTPUT-------------------


# We store eta for T= 1, 2, and 3
eta     <- matrix(nrow=K.max,ncol=T.k.max) 

eta.PI1 <- matrix(nrow=K.max) 
eta.PI2 <- matrix(nrow=K.max) 
eta.CI  <- matrix(nrow=K.max) 

# Used to collect products of m.n(T)
m.calc  <- matrix(nrow=T.k.max*K.max,ncol=N) 
# m collects mean number of failure m(T)
m       <- matrix(nrow=T.k.max*K.max) 

### ------------------CALCULATIONS-------------------

# Let us calculate the amount of time an object fails in the observed 
# time period T. The number of failures is described with m(T), where 
# T is time between replacement PIs. First we use iteration process 
# described in the Theory to find m.n(T).
# In the process, the random variable g is
# used to determine the time length in years from time point t=0 to 
# which the object fails. This time is represented using Tn. If
# this time is less than the time period T, the loop reiterates to see
# if the next failure also occurs before time T. If the next Tn brings
# the sum of all Tn to a value that exceeds time T, the loop stops. 
# The amount of failure iterations are counted using i. Once the loop 
# is exited, one is subtracted from ito count for the last failure, 
# which occurred after the time T had elapsed.

input <- matrix(data=rep(1:(T.k.max*K.max),each=N)
                , ncol=N, byrow=T.k.max*K.max)
m.calc <- function(T){
  i <- 0
  # Sum of failure durations, essentially sumTn=t .
  sumTn <- 0 # Start at t=0
  while (sumTn<T){
    i <- i+1 # Here we count every iteration
    g <- runif(1,0,1) 
    #time to failure, T.n,i on Figure 
    Tn <- ((-log(g))^(1/beta))*(1/lambda) 
    # total time after adding generated failure time
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
T.k <- 2  # This is changed in part B of the problem to 1 or 3.
for (t in (1:(T.k.max*K.max)))
{
  m[t]        <- m[t]*dim
}

e <- 0
for (K in (1:K.max))
{
  T <- K*T.k
  d <- (1-alpha)^(K-1)
  eta.CI[K]   <- m[T]*d*C.CI/T
  eta.PI1[K]  <- C.PI1*(K-1)/T
  eta.PI2[K]  <- C.PI2/T
  
  eta[K,T.k]    <- eta.CI[K]+eta.PI1[K]+eta.PI2[K]
}
mineta <- min(eta[,T.k])

mT <- m[seq(from=T.k,to=(K.max*T.k),by=T.k)] # setting up m(T)
cat("Results \n")
results=data.frame(1:K.max,(1:K.max)*T.k,mT
                   ,eta.CI,eta.PI1,eta.PI2,eta[,T.k])
names(results) <- c("K"
                    ,"Years"
                    ,"Pothole occ"
                    ,"Corr.Int. part"
                    ,"Repair PI part"
                    ,"Replacem. PI part"
                    ,"eta")
print(results)

cat("Minimum value of cost \n")
print(min(eta[,T.k]))
cat("optimal units of time to execute intervention \n")
print(which.min(eta[,T.k])*T.k)


### Part B

for (T.k in (1:T.k.max))
{
  e <- 0
  for (K in (1:K.max))
  {
    T <- K*T.k
    d <- (1-alpha)^(K-1)
    eta.CI[K]   <- m[T]*d*C.CI/T
    eta.PI1[K]  <- C.PI1*(K-1)/(K*T.k)
    eta.PI2[K]  <- C.PI2/(K*T.k)
    eta[K,T.k]  <- eta.CI[K]+eta.PI1[K]+eta.PI2[K]
  }
}

eta.CI   <- NULL
eta.PI1  <- NULL
eta.PI2  <- NULL

resultsB=data.frame(1:K.max,eta[,1],eta[,2],eta[,3])
names(resultsB) <- c("K"
                     ,"etaT.1"
                     ,"etaT.2"
                     ,"etaT.3")
print(resultsB)

cat("For T.k=2 \n")
cat("Minimum value of cost \n")
print(min(eta[,2]))
cat("optimal units of time to execute intervention \n")
print(which.min(eta[,2])*2)

cat("For T.k=1 \n")
cat("Minimum value of cost \n")
print(min(eta[,1]))
cat("optimal units of time to execute intervention \n")
print(which.min(eta[,1])*1)

cat("For T.k=3 \n")
cat("Minimum value of cost \n")
print(min(eta[,3]))
cat("optimal units of time to execute intervention \n")
print(which.min(eta[,3])*3)


### --------------------PLOT--------------------
## The following code is only for plotting the figure given as 
## a solution to the Exercise


plotres <- data.frame(1:K.max,eta[,2])
names(plotres) <- c("K","eta")

ploteta <- ggplot(data=plotres,aes(x=K,y=eta),color="red") + 
  geom_line(size=2,color="red") + 
  xlab("Number of PIs [-]") + 
  scale_x_continuous(breaks = seq(0, max(plotres$K), by = 1)) +
  ylab("Average cost per year [mu/tu]") + # Set axis labels
  ylim(0,600) +
  theme_bw(base_size=36) 

# Let us collect the optimal points in the data frame optimal

optimal <- data.frame(nrow=1,ncol=2)
names(optimal) <- c("x","y")

data <- plotres
optimal[1,1] <- data$K[which.min(data$eta)]
optimal[1,2] <- min(data$eta)

optlines <- data.frame(x0 = c(min(data$K),optimal$x)
                       , x1=c(optimal$x,optimal$x)
                       , y0=c(optimal$y,optimal$y)
                       , y1=c(optimal$y,0))

# Now we add these points to the graph before exporting
ploteta + geom_point(data=optimal
                     , aes(x=x
                           , y=y)
                     , colour="navyblue"
                     , size=6) +
  geom_segment(aes(x = x0, y = y0
                   , xend = x1, yend = y1)
               , data = optlines
               , size = 1.5
               , color= "grey") # Also draw lines to optimal point



dev.copy(png,'E3-09_eta.png',width=2000,height=1330)
dev.off()

# Plot B
resultstidy <- gather(resultsB
                      ,etaT.1,etaT.2,etaT.3
                      ,key="key",value="value")

plotetaT <- ggplot(data=resultstidy,aes(x=K,y=value
                                        ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Time between PIs",      # Set legend title
                   breaks=c("etaT.1","etaT.2","etaT.3"),
                   labels=c("\nT=1\n"
                            , "\nT=2\n"
                            , "\nT=3\n")
  )  +
  scale_linetype_discrete(name="Time between PIs") +
  scale_x_continuous(breaks = seq(0, max(resultstidy$K), by = 1)) +
  xlab("Number of PIs [-]") + 
  ylab("Average cost per year [mu/tu]") + # Set axis labels
  ylim(0,800) +
  theme_bw(base_size=36) +
  theme(legend.position=c(.8, .2))           # Position legend inside

# Let us collect the optimal points in the data frame optimal

optimal <- as.data.frame(matrix(nrow=3,ncol=3))
names(optimal) <- c("key","x","y")

key <- c("etaT.1","etaT.2","etaT.3")
data <- resultsB
for (i in 1:3)
{
  optimal[i,1] <- key[i]
  optimal[i,2] <- data$K[which.min(data[,i+1])]
  optimal[i,3] <- min(data[,i+1])
}



# Now we add these points to the graph before exporting
plotetaT + geom_point(data=optimal
                      , aes(x=x
                            , y=y)
                      , colour="navyblue"
                      , size=6)  

dev.copy(png,'E3-09_T.png',width=2000,height=1330)
dev.off()


### ------------------PLOT END-------------------

# END


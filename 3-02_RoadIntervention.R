# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 3.2
# | PROBLEM NAME: Road Intervention
# | UPDATE: AE     
# | DESCRIPTION: This problem is the same as Problem 3.1 except we 
# |              set our optimisation goal to maximising 
# |              infrastructure availability, i.e. minimizing 
# |              down-time.
# | 
# | KEYWORDS: Availability, failure, time, time-dependent, down-time
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls())     # clear the memory and items in R console
library(ggplot2)
library(tidyr)
### ------------------VARIABLE DEFINITION---------------------

## As in Problem 3.1,

# mu0 is the initial value of mu, occurring in year 1. 
# dmu is the annual rate of change of mu

## In addition, 

# t.CI is the Down-Time needed for execution of a Corrective 
# Intervention (CI)
# t.PI is the Down-Time needed for execution of a Preventive 
# Intervention (PI)
# tau is the total expected down time per time unit. We try to 
# maximise tau.

### ---------------VARIABLE INPUT-------------------

mu0   <- 0.2
dmu   <- 0.02
t.CI  <- 1 
t.PI  <- 0.2 

### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program in order to solve.  
## T is defined as the total operational time in
## between renewals. The renewal period counts the total down-time 
## (tn.t) as well.
## Total down-time consists of the parts of CI (tn.CI) and PI (tn.PI)

# General and miscellaneous definitions
T.max <- 10 # defines a maximum number of years for observed period
unit  <- 1 # Scales plot

# Define empty matrices and function for time- and age-dependent 
# models
# linear function of mean amount of failures.
mu = function(a,b,x){(a+b*(x-1))} 
# the corrective intervention time per unit of time
tn.CI  <- matrix(nrow=T.max) 
# the preventive intervention time per unit of time
tn.PI  <- matrix(nrow=T.max) 
# the mean number of failures in time period T
m     <- matrix(nrow=T.max) 
# expected availability per unit of time
tau   <- matrix(nrow=T.max) 
# Total down-time
tn.t   <- matrix(nrow=T.max) 

### ------------------CALCULATIONS-------------------

for (T in 1:T.max)
{
  m[T]      <- T*mu(mu0,dmu,T) 
  tn.CI[T]   <- t.CI*m[T]/T
  tn.PI[T]   <- t.PI/T
  tn.t[T]    <- tn.CI[T]+tn.PI[T] 
  tau[T]    <- T/(T+t.CI*m[T]+t.PI)
}
result <- data.frame(m,tn.PI,tn.CI,tn.t,tau)
result <- cbind(1:T.max,result)
names(result) <- c("t","m","tn.PI","tn.CI","tn.t","tau")

print(result)

### --------------------PLOT--------------------

results <- result[,c(1,3:5)]
resultstau <- result[,c(1,6)]
resultstidy <- gather(results
                      ,tn.CI,tn.PI,tn.t
                      ,key="key",value="value")

plottn <- ggplot(data=resultstidy,aes(x=t,y=value
                                      ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Non-operational time\ndue to:",      
                   breaks=c("tn.CI", "tn.PI", "tn.t"),
                   labels=c("\nCI\n"
                            , "\nPI\n"
                            , "\nTotal\n")
  )  +                  
  scale_linetype_discrete(name="Type of cost") +
  xlab("Time between Int. [years]") + 
  ylab("Average time per year [mu/yr]") + # Set axis labels
  theme_bw(base_size=36) + 
  annotate("text", x = 3, y = 0.36
           , label = "Optimal\nt.n=0.31 at T=3"
           , size=12,color="black")
plottn
dev.copy(png,'E3-02_tn.png',width=2000,height=1330)
dev.off()

# Plot availability

plottau <- ggplot(data=resultstau,aes(x=t,y=tau)) + 
  geom_line(size=2) + 
  
  xlab("Time between Int. [years]") + 
  ylab("Availability") + # Set axis labels
  ylim(0,1) +
  theme_bw(base_size=36) + 
  annotate("text", x = 3, y = 0.7
           , label = "Optimal\ntau=0.77 at T=3"
           , size=12,color="black")
plottau
dev.copy(png,'E3-02_tau.png',width=2000,height=1330)
dev.off()

### ------------------PLOT END-------------------

# END

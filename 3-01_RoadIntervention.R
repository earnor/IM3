# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 3.1
# | PROBLEM NAME: Road Intervention
# | UPDATE: AE     
# | DESCRIPTION: The program uses basic formulae of Two-State
# |              Intervention Strategy theory to minimise costs and
# |              finds optimum time to intervene.
# |              
# | 
# | KEYWORDS: Intervention, cost minimisation, optimization, road, 
# |           surface, increasing rate of failure
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls())     # clear the memory and items in R console
library(ggplot2)
library(tidyr)

### ------------------VARIABLE DEFINITION---------------------

## First we want to define the variables used for the program. 

# mu0 is the initial value of mu, occurring in year 1.
# dmu is the annual rate of change of mu
# C.CI is the Cost of a Corrective Intervention (CI)
# C.PI is the Cost of a Preventive Intervention (PI)

### ---------------VARIABLE INPUT-------------------

mu0  <- 0.2 
dmu  <- 0.02 
C.CI <- 20
C.PI <- 10

### ---------------PROGRAM OUTPUT-------------------

## The program uses simple formulae to calculate each part of the 
## Total Costs. The part deriving from costs of preventive 
## interventions, and the one from corrective interventions.

# The index .c is used to differentiate variables relating to the CI 
# from those indexed using .p for PI.

# General and miscellaneous definitions
unit  <- 1  # defined to scale plots
T.max <- 15 # defines a maximum number of years for observed period

# Define the variable of the model
# linear function of mean amount of failures.
mu = function(a,b,x){(a+b*(x-1))}   
# mean number of cumulated failures at time t=T
m       <- matrix(nrow=T.max)      
# the cost per unit of time for only CI
eta.CI  <- matrix(nrow=T.max)     
# the cost per unit of time for only PI
eta.PI  <- matrix(nrow=T.max)    
# cost per unit of time, sum of eta.p and eta.c
eta     <- matrix(nrow=T.max)      

### ------------------CALCULATIONS-------------------

for (T in 1:T.max){
  m[T]        <- T*mu(mu0,dmu,T)
  eta.CI[T]   <- C.CI*m[T]/T
  eta.PI[T]   <- C.PI/T
  eta[T]      <- eta.CI[T]+eta.PI[T] 
}

result <- data.frame(m,eta.CI,eta.PI,eta)
result <- cbind(1:T.max,result)
names(result) <- c("t","m","eta.CI","eta.PI","eta")

print(result)

cat("Optimal value of cost per unit time is\n")
print(min(eta))

### --------------------PLOT--------------------

plotm <- ggplot(data=result,aes(x=t,y=m)) + 
  geom_line(size=2) + 
  xlab("Time between Int. [years]") + ylab("m[T]") + 
  theme_bw(base_size=36) 
plotm

dev.copy(png,'E3-01_m.png',width=2000,height=1330)
dev.off()

results <- result[,c(1,3:5)]
names(results) <- c("t","eta.CI","eta.PI","eta")
resultstidy <- gather(results
                      ,eta.CI,eta.PI,eta
                      ,key="key",value="value")

ploteta <- ggplot(data=resultstidy,aes(x=t,y=value
                                       ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Type of cost",      # Set legend title
                   breaks=c("eta.CI", "eta.PI", "eta"),
                   labels=c("\nCI costs\n"
                            , "\nPI costs\n"
                            , "\nTotal costs\n")
  )  +                  
  scale_linetype_discrete(name="Type of cost") +
  xlab("Time between Int. [years]") + 
  ylab("Average cost per year [mu/yr]") + # Set axis labels
  theme_bw(base_size=36) + 
  annotate("text", x = 5, y = 8.25
           , label = "Optimal\neta=7.6 at T=5"
           ,size=12,color="black")
ploteta

dev.copy(png,'E3-01_Cost.png',width=2000,height=1330)
dev.off()

### ------------------PLOT END-------------------

# END

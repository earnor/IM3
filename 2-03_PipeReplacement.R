# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 2.3
# | PROBLEM NAME: Pipe Replacement
# | UPDATE: AE     
# | DESCRIPTION: This program uses available packages to model the  
# |              given data to fit a Weibull model. The parameters
# |              are extracted and used to find the average cost 
# |              of replacement using two different type of 
# |              material, cast-iron and ductile iron pipes.
# | 
# | KEYWORDS: Weibull, model, parameters, cast-iron, ductile, 
# |           pipes
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls()) # clear the memory and items in R console

library(MASS)
library(survival)
library(ggplot2)
library(tidyr)

setwd("Y:/common/IBI/Martani/IM Script - Workspace/Exercises/R-files/Updated")
data <- read.csv("2-03_PipeReplacement_csvData.csv"
                 ,header=TRUE,sep=";") 
attach(data)

### ------------------VARIABLE DEFINITION---------------------

## First we want to define the variables used for the program. 

# C is the cost of an intervention . Only CIs will be performed.

# t stands for time, or age
# d stands for delta, and represents the state of the pipe, whether 
# it has failed or not.

# The indices .c for cast iron pipes and .d for ductile iron pipes 
# are used to differentiate between the two.

### ---------------VARIABLE INPUT-------------------

# The input data includes both cast and ductile iron pipe data. We 
# separate them:

t.c <- data$Age[1:40]
d.c <- data$Status[1:40]
t.d <- data$Age[41:70]
d.d <- data$Status[41:70]

C.c <- 100000
C.d <- 90000

# From the imported data, we use the imported package to model the 
# data and extract its parameters. 

cat('Cast iron pipe - model parameters \n')
Sfit.c = survreg(Surv(t.c,d.c)~1,dist='weibull')
summary(Sfit.c)

cat('Ductile iron pipe - model parameters \n')
Sfit.d = survreg(Surv(t.d,d.d)~1,dist='weibull')
summary(Sfit.d)

# we extrapolate the following scale and shape parameters:
beta.c   <- 1/Sfit.c$scale[1]      # shape for cast iron
lambda.c <- 1/exp(Sfit.c$icoef[1]) # scale
beta.d   <- 1/Sfit.d$scale[1]      # for ductile iron
lambda.d <- 1/exp(Sfit.d$icoef[1])

### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program. 

T.max <- 1000 # this is the time frame of investigation

# Define empty matrices and function for time- and age-dependent models

int.c <- matrix(nrow=T.max) 
int.d <- matrix(nrow=T.max)
S.c   <- matrix(nrow=T.max)
S.d   <- matrix(nrow=T.max)

# Weibull CDF
F.w <- function(x,beta,lambda){(1-exp(-(lambda*x)^beta))} 
# Survival function
S.w <- function(x,shape,scale){(1-F.w(x,shape,scale))}    

### ------------------CALCULATIONS-------------------

# Question C : Costs of Pipes
# By integrating the survival function from 0 to t, we find the 
# average life-time of an object for cast-iron and ductile-iron pipes
for (t in 1:T.max)
{
  S.c[t] <- S.w(t,beta.c,lambda.c)
  S.d[t] <- S.w(t,beta.d,lambda.d)
  int.c[t] <- integrate(S.w,0,t,shape=beta.c,scale=lambda.c)$val
  int.d[t] <- integrate(S.w,0,t,shape=beta.d,scale=lambda.d)$val
}

rdf <- data.frame(1:T.max,S.c,S.d) # results data frame
rdf <- rdf[1:100,]
names(rdf) <- c("t","S.c","S.d")

# By iterating over a long period of time, we find the final 
# iteration of the average life time to be:

print(int.c[T.max])
print(int.d[T.max])

# We then find eta, the cost per unit time
eta.c <- C.c/int.c[T.max]
eta.d <- C.d/int.d[T.max]

cat("Cost per unit time for cast iron\n")
print(eta.c)
cat("Cost per unit time for ductile iron\n")
print(eta.d)

### --------------------PLOT--------------------
## The following code is only for plotting the figure given as
## a solution to the Exercise

# Question B - plotting the reliability curves

rdftidy <- gather(rdf,
                  S.c,S.d,
                  key="key",value="value")

plot <- ggplot(data=rdftidy,aes(x=t,y=value,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Type of pipe",      # Set legend title
                   breaks=c("S.c", "S.d"),
                   labels=c("\nCast iron\n", "\nDuctile iron\n")
  )  +                  
  scale_linetype_discrete(name="Type of pipe") +
  xlab("Time [years]") + ylab("") + # Set axis labels
  theme_bw(base_size=36) 
plot

dev.copy(png,'E2-03.png',width=2000,height=1330)
dev.off()

### ------------------PLOT END-------------------

# END

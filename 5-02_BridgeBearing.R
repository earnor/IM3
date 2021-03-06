# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 5.2
# | PROBLEM NAME: Bridge Bearing
# | UPDATE: AE     
# | DESCRIPTION: To find the optimal strategy, we find an optimal
# |              value of p, which minimises the expected costs.
# |              This value is then used to find the time of 
# |              monitoring and the interval between. This is a 
# | 
# | KEYWORDS: Weibull, failure, optimization, cost, bridge, 
# |
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls()) # clear the memory and objects in R console
library(ggplot2) # load ggplot
# set workdrive
setwd("Y:/common/IBI/Martani/IM Script - Workspace/Exercises/R-files/Updated")
### ------------------VARIABLE DEFINITION---------------------

# C stands for the cost.

# p is the probability and Q is the vector containing the whole
# range of considered probabilities.

### ---------------VARIABLE INPUT-------------------

lambda <- 0.8    # lambda is scale parameter
beta   <- 1.15      # Beta is the shape parameter
alpha  <- 1/lambda# we need alpha for calculation of k

C.1   <- 1
C.2   <- 10

k     <- (C.1/C.2)/alpha

T.max <- 15

### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program. 
# Define functions required

fi      <- function(i,x){(i^(1/beta))*((1-x)^(i-1))*x}
# hazard rate
h       <- function(t){(lambda*beta)*(lambda*t)^(beta-1)} 
F       <- function(x,l,b){1-exp(-(l*x)^b)}  

# We say that we will not perform more than 10 monitoring events
# before intervention
i.max <- 10
T     <- c(seq(0,i.max,by=1))
length(T)

# We set up a matrix holding the time of monitoring event
x   <- matrix(nrow=length(T))
dif <- matrix(nrow=length(T))

Q <- c(seq(0.001,1,by=0.001))
Z <- matrix(nrow=(length(Q)-1))
E <- matrix(nrow=(length(Q)-1))

### ------------------CALCULATIONS-------------------

# Start off by finding value of p giving minimum value of Z  

for (j in 1:(length(Q)-1))
{
  # sumA[j]  <- Z(Q[j])
  p <- Q[j]
  sum <- 0
  for (i in 1:1000){
    sum <- sum +(i^(1/beta))*((1-p)^(i-1))*p
  }
  Z[j] <- k/p+((log(1/(1-p)))^(beta)*sum -gamma(1+1/beta))
}

# We give p the value where Z is the smallest
p.min <- which.min(Z)/1000
print(p.min)



# Now to find the exact time of the monitoring events
for (i in 1:length(T))
{
  if (i == 1){
    x[i]=0
  } else {
    x[i]<- 1/lambda*((log(1/(1-p.min)))^(1/beta)) *(T[i]^(1/beta))
  }
  print(x[i])
}
# and the time between monitoring events is the following
dif[1] <- 0
for (t in 2:(length(T)))
{
  dif[t] <- x[t]-x[t-1]
}

print(cbind(x,dif))
# This is the calculation for expected costs E over time.
for (j in 1:(length(Q)-1))
{
  # sumB[j]  <- E(Q[j])
  p <- Q[j]
  sum <- 0
  for (i in 1:1000){
    sum <- sum + (i^(1/beta))*((1-p)^(i-1))*p
  }
  E[j] <- C.1/p + 
    C.2/lambda*((log(1/(1-p)))^(beta)*sum -
                  gamma(1+1/beta))
}

R <- as.data.frame(cbind(Q,Z,E))
names(R) <- c("p","Z","E")

### --------------------PLOT--------------------

# The following code is for plotting purposes

# Create plot
plot1 <- ggplot(data=R, aes(x=p)) + theme_bw(base_size=36) 
# Plot the two lines
plot1 <- plot1 + geom_line(aes(y=Z, colour="Z(p)"),size=2)
# We enter new lines within the quotation marks to create spacing 
# in legend
plot1 <- plot1 + geom_line(aes(y=E/100
                               , colour = "\nE[C(p)]\n")
                           ,size=2)
# Now the secondary axis is created.
plot1 <- plot1 + 
          scale_y_continuous(limits=c(0,3)
                             ,breaks=seq(from=0,to=3,by=0.5)
                             ,sec.axis = sec_axis(~.*100
                                                  ,breaks=seq(from=0
                                                              ,to=300
                                                              ,by=50)
                                                  ,name="Expected cost [mu]"
                                                  )) +
  theme(axis.title.y.right=element_text(angle = 90, hjust = 0.5))
# We colour the lines
plot1 <- plot1 + scale_colour_manual(values = c("blue", "red")) 
# Label the other two axes and name the variables
plot1 <- plot1 + labs(y = "Z(p)",
              x = "p - probability",
              colour = "Variable") +
  scale_x_continuous(breaks = seq(0, max(R$p), by = 0.1))

# Let us create the lines to the optimal point
optlines <- data.frame(key = c("z","z")
                       , x0 = c(min(R$p)
                                ,which.min(R$Z)/1000)
                       , x1 = c(which.min(R$Z)/1000
                                ,which.min(R$Z)/1000)
                       , y0 = c(min(R$Z),min(R$Z))
                       , y1 = c(min(R$Z),0))

# We draw the optimal point
plot1 <- plot1 + annotate("text", label="Optimal - 0.233"
                          , x=R$p[which.min(R$Z)]+0.02
                          , y=min(R$Z)+0.15
                          , size=12) +
                geom_point(data=R, aes(x=p[which.min(R$Z)]
                                       , y=min(Z))
                           , colour="navyblue"
                           , size=10) +
  geom_segment(aes(x = x0, y = y0
                   , xend = x1, yend = y1)
               , data = optlines
               , size = 1.5
               , color= "grey") # Also draw lines to optimal point
# We position the legend
plot1 <- plot1 + theme(legend.position = c(0.6, 0.9))
# We now export the graphic
plot1
dev.copy(png,'E5-02_Results.png',width=2000,height=1330)
dev.off()


### ------------------PLOT END-------------------

# END

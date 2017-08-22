# *-------------------------------------------------------------------
# | PROBLEM NUMBER: 9.4
# | PROBLEM NAME: Oil Distribution Network Construction
# | UPDATE: AE     
# | DESCRIPTION: This program optimises a Network using the simplex
# |              algorithm. The optimal configuration of two to set up
# |              an oil distribution network is sought. The task of 
# |              question A is to find this configuration without any
# |              loss. Question B considers losses.
# | 
# | KEYWORDS: Network, optimisation, capacity, simplex, cost, 
# |           oil, shipment, configuration
# *-------------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
library(linprog)
library(lpSolve)

setwd("Y:/common/IBI/03-Employees Folders/Martani/IM3 Script - Workspace/Exercises/R-files/Updated")
data <- read.csv("9-04_OilDistribution_csvData.csv"
                 ,header=TRUE,sep=";") 
attach(data)

### ------------------VARIABLE DEFINITION---------------------

# The data frame "data" includes
# Origin,         the origin plant of the material
# Destination,    the destination construction site
# C.c1,           the capacity of oil distribution using configuration 1
# C.c2,           the capacity of oil distribution using configuration 2
# P.c1,           losses from the pipeline link using configuration 1
# P.c2,           losses from the pipeline link using configuration 2

### ---------------VARIABLE INPUT-------------------
# Based on the following objective function and constraints we start
# off by maximising supply given that there are no pipeline losses.
# We set up the objective function as the sum of all flow arriving at 
# refinery given the capacity constraints given in the two figures.
    
# We add a column called linkID
linkID <- 1:nrow(data)
data <- cbind(linkID,data)

# The objective function is then 
# Z = x.7 + x.8

# where the indices represent the numbering of linkID

# The objective function is maximised. 

# The constraints 1-8 are set fitting for configuration 1, the 
# upper capacity:

#      x.1 <= 6
#      x.2 <= 4
#      x.3 <= 3
#      x.4 <= 2
#      x.5 <= 2
#      x.6 <= 5
#      x.7 <= 6
#      x.8 <= 4

# At the same time, it must be ensured that the variables take a 
# positive value
# Constraints 9-16 are non-negativity constraints :     
# x.1,...,x.8 >= 0

# We make all inflow of nodes equal the outflow. We do this for every 
# node, but since we are solving for the supply (which is equal to the 
# demand), we cannot make these constraints for the oil field and
# refinery, only the stations.

# Constraints 17-20 are thus:

#   x.1                - x.3 - x.4 = 0
#   x.2                - x.5 - x.6 = 0    
#   x.3 + x.4                - x.7 = 0
#   x.5 + x.6                - x.8 = 0   

# Positive values are inflow, and negative the outflow.

# Constraints 21-28 replace constraints 1-8 when calculating for the 
# second configuration.

# The revenue made is 150'000 mus per barrel. Costs of buiding and 
# maintainingthe pipeline are 1'000'000 for config 1 and 1'400'000 
# for config. 2.
r <- c(150,150)
c <- c(1000,1400)

# For question B, we define the percentage loss vector as defined in
# table in exercise

pl1 <- c(0.150,0.100,0.075,0.050,0.050,0.125,0.150,0.100)
pl2 <- c(0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125)

### ---------------PROGRAM OUTPUT-------------------

# Objective function
Z    <- c(0,0,0,0,0,0,1,1)
Z

n  <- length(Z)   # amount of x-variables.

# Capacity Constraints
f.1  <- c( 1, 0, 0, 0, 0, 0, 0, 0)
g.1  <- c(6)
f.2  <- c( 0, 1, 0, 0, 0, 0, 0, 0)
g.2  <- c(4)
f.3  <- c( 0, 0, 1, 0, 0, 0, 0, 0)
g.3  <- c(3)
f.4  <- c( 0, 0, 0, 1, 0, 0, 0, 0)
g.4  <- c(2)
f.5  <- c( 0, 0, 0, 0, 1, 0, 0, 0)
g.5  <- c(2)
f.6  <- c( 0, 0, 0, 0, 0, 1, 0, 0)
g.6  <- c(5)                 
f.7  <- c( 0, 0, 0, 0, 0, 0, 1, 0)
g.7  <- c(6)       
f.8  <- c( 0, 0, 0, 0, 0, 0, 0, 1)
g.8  <- c(4)       

# Non-negativity Constraints
f.9   <- c( 1, 0, 0, 0, 0, 0, 0, 0)                        
g.9   <- c(0)
f.10  <- c( 0, 1, 0, 0, 0, 0, 0, 0)  
g.10  <- c(0)
f.11  <- c( 0, 0, 1, 0, 0, 0, 0, 0)  
g.11  <- c(0)
f.12  <- c( 0, 0, 0, 1, 0, 0, 0, 0)        
g.12  <- c(0)
f.13  <- c( 0, 0, 0, 0, 1, 0, 0, 0)  
g.13  <- c(0)
f.14  <- c( 0, 0, 0, 0, 0, 1, 0, 0)  
g.14  <- c(0)
f.15  <- c( 0, 0, 0, 0, 0, 0, 1, 0)  
g.15  <- c(0)
f.16  <- c( 0, 0, 0, 0, 0, 0, 0, 1)                        
g.16  <- c(0)

# Functionality constraints
f.17  <- c( 1, 0,-1,-1, 0, 0, 0, 0)
g.17  <- c(0)
f.18  <- c( 0, 1, 0, 0,-1,-1, 0, 0)       
g.18  <- c(0)
f.19  <- c( 0, 0, 1, 1, 0, 0,-1, 0)
g.19  <- c(0)
f.20  <- c( 0, 0, 0, 0, 1, 1, 0,-1)
g.20  <- c(0)

# Capacity Constraints for configuration 2

g.21  <- c(5)
g.22  <- c(5)
g.23  <- c(5)
g.24  <- c(5)
g.25  <- c(5)
g.26  <- c(5)                 
g.27  <- c(5)       
g.28  <- c(5)   

# As no variables should be integers, we define int

int <- NULL

# For our constraints:

# Manually the number of constraints are typed in
c.l <- 8  # For less-than-or-equal-to constraints
c.m <- 8  # For more-than-or-equal-to constraints
c.e <- 4  # For equal-to constraints

A.l <- matrix(c(f.1,f.2,f.3,f.4,f.5,f.6,f.7,f.8)
              ,nrow=c.l,ncol=n, byrow=TRUE) 
b.l <- matrix(c(g.1,g.2,g.3,g.4,g.5,g.6,g.7,g.8)
              ,nrow=c.l,ncol=1, byrow=TRUE)
A.m <- matrix(c(f.9,f.10,f.11,f.12,f.13,f.14,f.15,f.16)
              ,nrow=c.m,ncol=n, byrow=TRUE)
b.m <- matrix(c(g.9,g.10,g.11,g.12,g.13,g.14,g.15,g.16)
              ,nrow=c.m,ncol=1, byrow=TRUE)
A.e <- matrix(c(f.17,f.18,f.19,f.20)
              ,nrow=c.e,ncol=n, byrow=TRUE)
b.e <- matrix(c(g.17,g.18,g.19,g.20)
              ,nrow=c.e,ncol=1, byrow=TRUE)

# For configuration 2, we change the constraints vectors

b2.l <- matrix(c(g.21,g.22,g.23,g.24,g.25,g.26,g.27,g.28)
               ,nrow=c.l,ncol=1, byrow=TRUE)

A  <- rbind( A.l,A.m,A.e)
b  <- rbind( b.l,b.m,b.e)

# here we set the RHS of constraints 1-8 to those defined by g.21-g.28
b2 <- rbind(b2.l,b.m,b.e)

const.dir <- c(rep("<=",c.l),rep(">=",c.m),rep("=",c.e))

### ------------------CALCULATIONS-------------------


results1 <- lp (direction = "max", objective.in=Z, const.mat = A, 
               const.dir, const.rhs=b,
               int.vec = int, all.int=FALSE, all.bin=FALSE,
               num.bin.solns=1, use.rw=FALSE)

supply1 <- sum(results1$solution*Z)
profit1 <- supply1*r[1]-c[1]

results2<- lp (direction = "max", objective.in=Z, const.mat = A, 
                const.dir, const.rhs=b2,
                int.vec = int, all.int=FALSE, all.bin=FALSE,
                num.bin.solns=1, use.rw=FALSE)

supply2 <- sum(results2$solution*Z)
profit2 <- supply2*r[2]-c[2]

# With configuration 1, we can supply the following amount of barrels
supply1
# making a profit of ('000 mus)
profit1
# Configuration 2 provides the following supply of barrels.
supply2
# making a profit of ('000 mus)
profit2

# Using the second configuration will increase the capacity by one 
# barrel/tu but reduce the profit by 150'000 mus.

### --------------------Question B--------------------

# We reconstruct the constraints 17-20
#   P.c1[1]*x.1                - x.3 - x.4 >= 0
#   P.c1[2]*x.2                - x.5 - x.6 >= 0    
#   P.c1[3]*x.3 + P.c1[4]*x.4        - x.7 >= 0
#   P.c1[5]*x.5 + P.c1[6]*x.6        - x.8 >= 0  

# Functionality constraints for question b, configuration 1.

# x.1 loses 0.15
fb1.17  <- c( 0.85,    0,    -1,   -1,    0,     0,    0,    0) 
# x.2 loses 0.10  
fb1.18  <- c(    0, 0.90,     0,    0,   -1,    -1,    0,    0) 
# x.3 7.5%; x.4 5% 
fb1.19  <- c(    0,    0, 0.925, 0.95,    0,     0,   -1,    0) 
# x.5  5% ; x.6 10%
fb1.20  <- c(    0,    0,     0,    0, 0.95, 0.875,    0,   -1)

# Manually the number of constraints are typed in
c.l <- 8  # For less-than-or-equal-to constraints
c.m <- 12 # For more-than-or-equal-to constraints
c.e <- 0  # For equal-to constraints

# The less-than-or-equal-to constraints are the same.
# The equal-to constraints become more-than-or-equal-to constraints 
# including the losses.
A.m <- matrix(c(f.9,f.10,f.11,f.12,f.13,f.14,f.15,f.16
                ,fb1.17,fb1.18,fb1.19,fb1.20)
              ,nrow=c.m,ncol=n, byrow=TRUE)
b.m <- matrix(c(g.9,g.10,g.11,g.12,g.13,g.14,g.15,g.16
                ,g.17,g.18,g.19,g.20)
              ,nrow=c.m,ncol=1, byrow=TRUE)
A.e <- NULL
b.e <- NULL

Ab1  <- rbind( A.l,A.m,A.e)
b  <- rbind( b.l,b.m,b.e)

# Functionality constraints for question b, configuration 2

# loses 0.125
fb2.17  <- c( 0.875,     0,    -1,    -1,     0,     0, 0, 0) 
fb2.18  <- c(     0, 0.875,     0,     0,    -1,    -1, 0, 0)       
fb2.19  <- c(     0,     0, 0.875, 0.875,     0,     0,-1, 0) 
fb2.20  <- c(     0,     0,     0,     0, 0.875, 0.875, 0,-1) 

A.m <- matrix(c(f.9,f.10,f.11,f.12,f.13,f.14,f.15,f.16
                ,fb2.17,fb2.18,fb2.19,fb2.20)
              ,nrow=c.m,ncol=n, byrow=TRUE)

Ab2  <- rbind( A.l,A.m,A.e)
b2   <- rbind(b2.l,b.m,b.e)

const.dir <- c(rep("<=",c.l),rep(">=",c.m),rep("=",c.e))

# Instead of constraining the last two variables, we move these 
# constraints of the percentage loss into the optimal formula

Zb1    <- c(0,0,0,0,0,0, 0.85,  0.90) # x.7 by 15% ; x.8 10% 
Zb2    <- c(0,0,0,0,0,0,0.875, 0.875) # loses 0.125 each


resultsb1 <- lp (direction = "max", objective.in=Zb1
                 , const.mat = Ab1
                 , const.dir, const.rhs=b ,int.vec = int
                 , all.int=FALSE, all.bin=FALSE
                 , num.bin.solns=1, use.rw=FALSE)

supplyb1 <- sum(resultsb1$solution*Zb1)
profitb1 <- supplyb1*r[1]-c[1]

resultsb2 <- lp (direction = "max", objective.in=Zb2
                 , const.mat = Ab2 
                 , const.dir, const.rhs=b2 ,int.vec = int
                 , all.int=FALSE, all.bin=FALSE
                 , num.bin.solns=1, use.rw=FALSE)

supplyb2 <- sum(resultsb2$solution*Zb2)
profitb2 <- supplyb2*r[2]-c[2]

# With configuration 1, we can supply the following amount of barrels
supplyb1
# making a profit of ('000 mus)
profitb1
# Configuration 2 provides the following supply of barrels.
supplyb2
# making a profit of ('000 mus)
profitb2

# Using the second configuration will reduce the supply capacity 
# and reduce the profit by over 430'000 mus.

### ------------------Question B END-------------------

# END

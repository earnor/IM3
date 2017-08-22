# *-------------------------------------------------------------------
# | PROBLEM NUMBER: 9.1
# | PROBLEM NAME: Cement Production and Shipment
# | UPDATE: AE     
# | DESCRIPTION: This program optimises a Network using the simplex
# |              algorithm. The network, based of nodes and links,
# |              has a certain capacity, which is found in question C.
# |              The optimal way to transport 700 truckloads over the
# |              network is the task of question A, and the cheapest 
# |              route is found in question B.
# | 
# | KEYWORDS: Network, optimisation, capapcity, simplex, cost, 
# |           cement, shipment
# *-------------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
library(linprog)
library(lpSolve)

# Important to set the working directory to the folder where you
# keep the csv file.
setwd("Y:/common/IBI/03-Employees Folders/Martani/IM3 Script - Workspace/Exercises/R-files/Updated")
data <- read.csv("9-01_CementProductionShipment_csvData.csv"
                 ,header=TRUE,sep=";") 
attach(data)

### ------------------VARIABLE DEFINITION---------------------

# The data frame "data" includes
# Origin,         the original node of a particular link
# Destination,    the destination of the node.
# Cost,           the cost of using the particular link
# Link.Capacity,  is the maximum amount of users possible for the link

# We add a column called linkID

linkID <- 1:nrow(data)
data <- cbind(linkID,data)

### ---------------VARIABLE INPUT-------------------
# Based on the following objective function and constraints to 
# minimise costs.
# We set up the objective function for all 18 links:
# Z =   5*x.1 +   2*x.2 +  1*x.3 +  4*x.4 +  2*x.5 +  1*x.6 +  2*x.7  
#   +   3*x.8 +   2*x.9 + 2*x.10 + 6*x.11 +12*x.12 + 1*x.13 + 4*x.14 
#   +  9*x.15 +  7*x.16 + 6*x.17 + 4*x.18
#    
# The indices of the variables are representing the linkID

# With 700 trucks leacing from node 1 to node 10, we know that the
# supply of node 1 is equal to the demand of node 10, namely 700
# truckloads.

# We set the functional constraints accordingly, so that inflow of all
# nodes equal the outflow of the node, when supply and demand has been
# taken into account. This is displayed mathematically in an equation
# in the Exercises.
# The constraints 1-10 are thus for node flux equality.
# These constraints, when all variables have been moved to the LHS 
# and constants to the RHS:

#                  - x.1 - x.2 - x.3 - x.4 = -700     
#   x.1 + x.7                  - x.5 - x.6 = 0
#   x.2 + x.9                  - x.7 - x.8 = 0    
#   x.3                - x.9 - x.10 - x.11 = 0
#   x.4 + x.10                      - x.12 = 0   # Positive values are
#   x.5                             - x.13 = 0   # inflow, and negative
#   x.6 + x.8 + x.11  - x.14 - x.15 - x.16 = 0   # the outflow.
#   x.13 + x.14                     - x.17 = 0
#   x.12 + x.15                     - x.18 = 0
#   x.16 + x.17 + x.18                     = 700

# The variables x.1 - x.18 should be limited to positive integers.
# Constraints 11-28 are non-negativity constraints :     
#   x.1,...,x.18 >= 0
# Constraints 29-46 are then the capacity constraints:
#    x.1 <= 250
#    x.2 <= 300
#    x.3 <=  80
#    x.4 <= 150
#    x.5 <= 250
#    x.6 <= 320
#    x.7 <= 150
#    x.8 <= 130
#    x.9 <=  90
#   x.10 <= 200
#   x.11 <= 180
#   x.12 <= 200
#   x.13 <= 255
#   x.14 <= 350
#   x.15 <= 240
#   x.16 <= 250
#   x.17 <= 300
#   x.18 <= 250


### ---------------PROGRAM OUTPUT-------------------

# Objective function
Z    <- as.vector(data$Cost)
Z

# Inflow = Outflow
f.1  <- c(-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.1  <- c(-700)
f.2  <- c( 1, 0, 0, 0,-1,-1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.2  <- c(0)
f.3  <- c( 0, 1, 0, 0, 0, 0,-1,-1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.3  <- c(0)
f.4  <- c( 0, 0, 1, 0, 0, 0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0)
g.4  <- c(0)
f.5  <- c( 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0,-1, 0, 0, 0, 0, 0, 0)
g.5  <- c(0)
f.6  <- c( 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0)  
g.6  <- c(0)                                        
f.7  <- c( 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0,-1,-1,-1, 0, 0)    
g.7  <- c(0)
f.8  <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0,-1, 0)  
g.8  <- c(0)
f.9  <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0,-1)    
g.9  <- c(0)
f.10 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)
g.10 <- c(700)

# Non-negativity

f.11 <- c( 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.11 <- c(0)
f.12 <- c( 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.12 <- c(0)
f.13 <- c( 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.13 <- c(0)
f.14 <- c( 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.14 <- c(0)
f.15 <- c( 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.15 <- c(0)
f.16 <- c( 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.16 <- c(0)
f.17 <- c( 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.17 <- c(0)
f.18 <- c( 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.18 <- c(0)
f.19 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.19 <- c(0)
f.20 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
g.20 <- c(0)
f.21 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
g.21 <- c(0)
f.22 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
g.22 <- c(0)
f.23 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
g.23 <- c(0)
f.24 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
g.24 <- c(0)
f.25 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
g.25 <- c(0)
f.26 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
g.26 <- c(0)
f.27 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
g.27 <- c(0)
f.28 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
g.28 <- c(0)

# Capacity constraints

f.29 <- c( 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.29 <- c(250)
f.30 <- c( 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.30 <- c(300)
f.31 <- c( 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.31 <- c(80)
f.32 <- c( 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.32 <- c(150)
f.33 <- c( 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.33 <- c(250)
f.34 <- c( 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.34 <- c(320)
f.35 <- c( 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.35 <- c(150)
f.36 <- c( 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.36 <- c(130)
f.37 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.37 <- c(90)
f.38 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
g.38 <- c(200)
f.39 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
g.39 <- c(180)
f.40 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
g.40 <- c(200)
f.41 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
g.41 <- c(255)
f.42 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
g.42 <- c(350)
f.43 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
g.43 <- c(240)
f.44 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
g.44 <- c(250)
f.45 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
g.45 <- c(300)
f.46 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
g.46 <- c(250)

# As all variables should be integers, we define int

int <- 1:n

# For our constraints:

# Manually the number of constraints are typed in
c.l <- 18 # For less-than-or-equal-to constraints
c.m <- 18 # For more-than-or-equal-to constraints
c.e <- 10 # For equal-to constraints

n  <- length(Z)   # amount of x-variables.

A.l <- matrix(c(f.29,f.30,f.31,f.32
                ,f.33,f.34,f.35,f.36
                ,f.37,f.38,f.39,f.40
                ,f.41,f.42,f.43,f.44
                ,f.45,f.46)
              ,nrow=c.l,ncol=n, byrow=TRUE) 
b.l <- matrix(c(g.29,g.30,g.31,g.32
                ,g.33,g.34,g.35,g.36
                ,g.37,g.38,g.39,g.40
                ,g.41,g.42,g.43,g.44
                ,g.45,g.46)
              ,nrow=c.l,ncol=1, byrow=TRUE)
A.m <- matrix(c(f.11,f.12,f.13,f.14
                ,f.15,f.16,f.17,f.18
                ,f.19,f.20,f.21,f.22
                ,f.23,f.24,f.25,f.26
                ,f.27,f.28)
              ,nrow=c.m,ncol=n, byrow=TRUE)
b.m <- matrix(c(g.11,g.12,g.13,g.14
                ,g.15,g.16,g.17,g.18
                ,g.19,g.20,g.21,g.22,
                g.23,g.24,g.25,g.26
                ,g.27,g.28)
              ,nrow=c.m,ncol=1, byrow=TRUE)
A.e <- matrix(c(f.1,f.2,f.3,f.4
                ,f.5,f.6,f.7,f.8
                ,f.9,f.10)
              ,nrow=c.e,ncol=n, byrow=TRUE)
b.e <- matrix(c(g.1,g.2,g.3,g.4
                ,g.5,g.6,g.7,g.8
                ,g.9,g.10)
              ,nrow=c.e,ncol=1, byrow=TRUE)

A <- rbind(A.l,A.m,A.e)
b <- rbind(b.l,b.m,b.e)

const.dir <- c(rep("<=",c.l),rep(">=",c.m),rep("=",c.e))

### ------------------CALCULATIONS-------------------


results <- lp (direction = "min", objective.in=Z, const.mat = A, 
               const.dir, const.rhs=b,
               int.vec = int, all.int=FALSE, all.bin=FALSE,
               num.bin.solns=1, use.rw=FALSE)

cost <- results$solution*Z

result <- cbind(data, results$solution, cost)

result
results

# Question B

# To find the cheapest single path from A to B, we set the supply and 
# demand to 1, and optimise the route.

g.1  <- c(-1)
g.10 <- c(1)

# The capacity constraints become redundant

c.l <- 0
A.l <- NULL
b.l <- NULL
A.m <- matrix(c(f.11,f.12,f.13,f.14
                ,f.15,f.16,f.17,f.18
                ,f.19,f.20,f.21,f.22
                ,f.23,f.24,f.25,f.26
                ,f.27,f.28)
              ,nrow=c.m,ncol=n, byrow=TRUE)
b.m <- matrix(c(g.11,g.12,g.13,g.14
                ,g.15,g.16,g.17,g.18
                ,g.19,g.20,g.21,g.22
                ,g.23,g.24,g.25,g.26
                ,g.27,g.28)
              ,nrow=c.m,ncol=1, byrow=TRUE)
A.e <- matrix(c(f.1,f.2,f.3,f.4
                ,f.5,f.6,f.7,f.8
                ,f.9,f.10)
              ,nrow=c.e,ncol=n, byrow=TRUE)
b.e <- matrix(c(g.1,g.2,g.3,g.4
                ,g.5,g.6,g.7,g.8
                ,g.9,g.10)
              ,nrow=c.e,ncol=1, byrow=TRUE)

A <- rbind(A.l,A.m,A.e)
b <- rbind(b.l,b.m,b.e)

const.dir <- c(rep("<=",c.l)
               ,rep(">=",c.m)
               ,rep("=",c.e))

resultsB <- lp (direction = "min"
                , objective.in=Z, const.mat = A
                , const.dir, const.rhs=b
                , int.vec = int
                , all.int=FALSE, all.bin=FALSE
                , num.bin.solns=1, use.rw=FALSE)

cost <- resultsB$solution*Z

resultB <- cbind(data, resultsB$solution, cost)
resultB
resultsB
# The optimal route incurs a cost of 12 mu

# Question C
# We now want to know what the maximum capacity of the system, i.e.
# the maximum supply possible, while keeping to all constraints.
# Let us symbolise this quantity with s.

# We know that s = x.1 + x.2 + x.3 + x.4 from the first constraint

Z    <- c( 1, 1, 1, 1
           , 0, 0, 0, 0
           , 0, 0, 0, 0
           , 0, 0, 0, 0
           , 0, 0)

# We reset the capacity constraints
g.1  <- c(-s)
g.10 <- c(s)
# This will not work using this algorithm, so we will combine 
# the constraints

f.1  <- c(-1,-1,-1,-1
          , 0, 0, 0, 0
          , 0, 0, 0, 0
          , 0, 0, 0, 1
          , 1, 1)
g.1  <- c(0)

c.l <- 18 # For less-than-or-equal-to constraints
c.m <- 18 # For more-than-or-equal-to constraints
c.e <- 9  # For equal-to constraints

A.l <- matrix(c(f.29,f.30,f.31,f.32
                ,f.33,f.34,f.35,f.36
                ,f.37,f.38,f.39,f.40
                ,f.41,f.42,f.43,f.44
                ,f.45,f.46)
              ,nrow=c.l,ncol=n, byrow=TRUE) 
b.l <- matrix(c(g.29,g.30,g.31,g.32
                ,g.33,g.34,g.35,g.36
                ,g.37,g.38,g.39,g.40
                ,g.41,g.42,g.43,g.44
                ,g.45,g.46)
              ,nrow=c.l,ncol=1, byrow=TRUE)
A.m <- matrix(c(f.11,f.12,f.13,f.14
                ,f.15,f.16,f.17,f.18
                ,f.19,f.20,f.21,f.22
                ,f.23,f.24,f.25,f.26
                ,f.27,f.28)
              ,nrow=c.m,ncol=n, byrow=TRUE)
b.m <- matrix(c(g.11,g.12,g.13,g.14
                ,g.15,g.16,g.17,g.18
                ,g.19,g.20,g.21,g.22
                ,g.23,g.24,g.25,g.26
                ,g.27,g.28)
              ,nrow=c.m,ncol=1, byrow=TRUE)
A.e <- matrix(c(f.1,f.2,f.3,f.4
                ,f.5,f.6,f.7,f.8
                ,f.9) # We erase const 10
              ,nrow=c.e,ncol=n, byrow=TRUE)
b.e <- matrix(c(g.1,g.2,g.3,g.4
                ,g.5,g.6,g.7,g.8
                ,g.9)
              ,nrow=c.e,ncol=1, byrow=TRUE)

A <- rbind(A.l,A.m,A.e)
b <- rbind(b.l,b.m,b.e)

const.dir <- c(rep("<=",c.l),rep(">=",c.m),rep("=",c.e))

resultsC <- lp (direction = "max", objective.in=Z, const.mat = A 
                , const.dir, const.rhs=b
                , int.vec = int
                , all.int=FALSE, all.bin=FALSE
                , num.bin.solns=1, use.rw=FALSE)

cost <- resultsC$solution*as.vector(data$Cost)

resultC <- cbind(data, resultsC$solution, cost)
resultC
resultsC
# The maximum the system can carry is 760 totalling costs of 11'440 mu


### --------------------PLOT--------------------

### ------------------PLOT END-------------------

# END

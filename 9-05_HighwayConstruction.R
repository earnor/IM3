# *-------------------------------------------------------------------
# | PROBLEM NUMBER: 9.5
# | PROBLEM NAME: Highway Construction Program
# | UPDATE: AE     
# | DESCRIPTION: This program optimises a Network using the simplex
# |              algorithm. The network, based of nodes and links,
# |              has a certain capacity, which is found in question C.
# |              The optimal way to transport 700 truckloads over the
# |              network is the task of question A, and the cheapest 
# |              route is found in question B.
# | 
# | KEYWORDS: Network, optimisation, simplex, cost, highway, route,
# |           choice
# *-------------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
library(linprog)
library(lpSolve)

setwd("Y:/common/IBI/03-Employees Folders/Martani/IM3 Script - Workspace/Exercises/R-files/Updated")
data <- read.csv("9-05_HighwayConstruction_csvData.csv"
                 ,header=TRUE,sep=";") 
attach(data)

### ------------------VARIABLE DEFINITION---------------------

# The data frame "data" includes
# Origin,         the original node of a particular link
# Destination,    the destination of the node.
# Cost,           the cost of using the particular link

# We add a column called linkID

linkID <- 1:nrow(data)
data <- cbind(linkID,data)

### ---------------VARIABLE INPUT-------------------
# Based on the following objective function and constraints 
# we minimise costs.
# We set up the objective function for all 21 links:
# Z =   1*x.1 +  3*x.2 + 2*x.3 +  4*x.4 +  3*x.5 +  2*x.6 +  2*x.7    
#   +   3*x.8 +  4*x.9 +1*x.10 + 3*x.11 + 3*x.12 + 2*x.13 + 2*x.14 
#   +  3*x.15 + 2*x.16 +1*x.17 + 2*x.18 + 3*x.19 + 5*x.20 + 3*x.21
#    
# The indices of the variables are representing the linkID

# We set the functional constraints accordingly, so that inflow of all
# nodes equal the outflow of the node, when supply and demand has been
# taken into account. 
# The constraints 1-12 are thus for node flux equality.
# These constraints, when all variables have been moved to the LHS 
# and constants to the RHS:

# Positive values are inflow, and negative the outflow.

#                          - x.1 - x.2 - x.3 = -1     
#   x.1                          - x.4 - x.5 = 0
#   x.2                                - x.6 = 0    
#   x.3                    - x.7 - x.8 - x.9 = 0
#   x.4                 - x.10 - x.11 - x.12 = 0  
#   x.5 + x.6 + x.7 + x.10            - x.13 = 0    
#   x.8                        - x.14 - x.15 = 0   
#   x.11                              - x.16 = 0
#   x.9 + x.12 + x.13 + x.14   - x.17 - x.18 = 0
#   x.15                       - x.19 - x.20 = 0
#   x.16 + x.17 + x.19                - x.21 = 0
#   x.18 + x.20 + x.21                       = 1

# The variables x.1 - x.21 should be limited to positive integers.
# Constraints 13-33 are non-negativity constraints :    
#   x.1,...,x.21 >= 0



### ---------------PROGRAM OUTPUT-------------------

# Objective function
Z    <- as.vector(data$cost)
Z

n   <- length(Z)

# Inflow = Outflow
f.1  <- c(-1,-1,-1, 0, 0, 0, 0
          , 0, 0, 0, 0, 0, 0, 0
          , 0, 0, 0, 0, 0, 0, 0)
g.1  <- c(-1)
f.2  <- c( 1, 0, 0,-1,-1, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.2  <- c(0)
f.3  <- c( 0, 1, 0, 0, 0,-1, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.3  <- c(0)
f.4  <- c( 0, 0, 1, 0, 0, 0,-1
           ,-1,-1, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.4  <- c(0)
f.5  <- c( 0, 0, 0, 1, 0, 0, 0
           , 0, 0,-1,-1,-1, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.5  <- c(0)
f.6  <- c( 0, 0, 0, 0, 1, 1, 1
           , 0, 0, 1, 0, 0,-1, 0
           , 0, 0, 0, 0, 0, 0, 0)  
g.6  <- c(0)                                        
f.7  <- c( 0, 0, 0, 0, 0, 0, 0
           , 1, 0, 0, 0, 0, 0,-1
           ,-1, 0, 0, 0, 0, 0, 0)                        
g.7  <- c(0)
f.8  <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 1, 0, 0, 0
           , 0,-1, 0, 0, 0, 0, 0)  
g.8  <- c(0)
f.9  <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 1, 0, 0, 1, 1, 1
           , 0, 0,-1,-1, 0, 0, 0)        
g.9  <- c(0)
f.10 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 1, 0, 0, 0,-1,-1, 0)
g.10 <- c(0)
f.11 <- c( 1, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 1, 1, 0, 1, 0,-1)
g.11 <- c(0)
f.12 <- c( 0, 1, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 1, 0, 1, 1)
g.12 <- c(1)

# Non-negativity

f.13 <- c( 1, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.13 <- c(0)
f.14 <- c( 0, 1, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.14 <- c(0)
f.15 <- c( 0, 0, 1, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.15 <- c(0)
f.16 <- c( 0, 0, 0, 1, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.16 <- c(0)
f.17 <- c( 0, 0, 0, 0, 1, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.17 <- c(0)
f.18 <- c( 0, 0, 0, 0, 0, 1, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.18 <- c(0)
f.19 <- c( 0, 0, 0, 0, 0, 0, 1
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.19 <- c(0)
f.20 <- c( 0, 0, 0, 0, 0, 0, 0
           , 1, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.20 <- c(0)
f.21 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 1, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.21 <- c(0)
f.22 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 1, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.22 <- c(0)
f.23 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 1, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.23 <- c(0)
f.24 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 1, 0, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.24 <- c(0)
f.25 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 1, 0
           , 0, 0, 0, 0, 0, 0, 0)
g.25 <- c(0)
f.26 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 1
           , 0, 0, 0, 0, 0, 0, 0)
g.26 <- c(0)
f.27 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 1, 0, 0, 0, 0, 0, 0)
g.27 <- c(0)
f.28 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 1, 0, 0, 0, 0, 0)
g.28 <- c(0)
f.29 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 1, 0, 0, 0, 0)
g.29 <- c(0)
f.30 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 1, 0, 0, 0)
g.30 <- c(0)
f.31 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 1, 0, 0)
g.31 <- c(0)
f.32 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 1, 0)
g.32 <- c(0)
f.33 <- c( 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 0
           , 0, 0, 0, 0, 0, 0, 1)
g.33 <- c(0)

# Capacity constraints

# As all variables should be integers, we define int

int <- 1:n

# For our constraints:

# Manually the number of constraints are typed in
c.l <- 0  # For less-than-or-equal-to constraints
c.m <- 21 # For more-than-or-equal-to constraints
c.e <- 12 # For equal-to constraints

n  <- length(Z)   # amount of x-variables.

A.l <- NULL
b.l <- NULL
A.m <- matrix(c(f.13,f.14,f.15,f.16,f.17,f.18,f.19
                ,f.20,f.21,f.22,f.23,f.24,f.25,f.26
                ,f.27,f.28,f.29,f.30,f.31,f.32,f.33)
              ,nrow=c.m,ncol=n, byrow=TRUE)
b.m <- matrix(c(g.13,g.14,g.15,g.16,g.17,g.18,g.19
                ,g.20,g.21,g.22,g.23,g.24,g.25,g.26
                ,g.27,g.28,g.29,g.30,g.31,g.32,g.33)
              ,nrow=c.m,ncol=1, byrow=TRUE)
A.e <- matrix(c(f.1,f.2,f.3,f.4,f.5,f.6
                ,f.7,f.8,f.9,f.10,f.11,f.12)
              ,nrow=c.e,ncol=n, byrow=TRUE)
b.e <- matrix(c(g.1,g.2,g.3,g.4,g.5,g.6
                ,g.7,g.8,g.9,g.10,g.11,g.12)
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


### --------------------PLOT--------------------

### ------------------PLOT END-------------------

# END

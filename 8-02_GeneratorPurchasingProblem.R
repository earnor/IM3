# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 8.2
# | PROBLEM NAME: Generator Purchasing Program
# | UPDATE: AE     
# | DESCRIPTION: Minimising purchase cost is the ultimate goal in 
# |              the search for generators to be puchased to meet
# |              demand for a five year period.
# | 
# | KEYWORDS: generating capacity, purchasing, program, 
# |           generators
# *-----------------------------------------------------------------

# Note: To run this program, you must have installed the packages 
#       'linprog' and 'lpSolve'

### ------------------DATA AND PACKAGE IMPORT-----------------
library(linprog)
library(lpSolve)

### ------------------VARIABLE DEFINITION---------------------

# Z is a vector of coefficients of the objective function.
# f is a vector of left side of functional constraints, with indices
# from 1
# g is the corresponding right side of the constraint

# A.l is the collection of all constraints' left sides (f) 
# that have a less than or equal to sign.
# b.l is the corresponding right side (g).
# A and b also carry the indices .m for constraints that has a more 
# than or equal to sign, and .e for one that has an equal sign.
# The constraint is then constructed using A*x=b where x is a vector 
# of variables

### ---------------VARIABLE INPUT-------------------
# Based on the following objective function and constraints to 
# minimise costs.
# We set up the objective function for all 4 generator types 
# and every year.
# Z = 300*x.11 + 460*x.12 + 670*x.13 + 950*x.14    # First year
#   + 250*x.21 + 375*x.22 + 558*x.23 + 790*x.24    # Second year etc.
#   + 200*x.31 + 350*x.32 + 465*x.33 + 670*x.34
#   + 170*x.41 + 280*x.42 + 380*x.43 + 550*x.44
#   + 145*x.51 + 235*x.52 + 320*x.53 + 460*x.54
# The indices of the variables are replaced by the numbers 1-20, so 
# that indices 1-4 are for year 1, 5-8 for year 2, and so on.

# We set the functional constraints for each year equal to those
# stated in the exercise.
# The types of generators generate 10 MW, 25 MW, 50 MW and 100 MW
# respectively.
# The power company currently has a generating capacity of 750 MW.
# Constraints 1-5 are the functional constraints for years 1-5
#   P.1 = 750 + 10*x.1  + 25*x.2  + 50*x.3  + 100*x.4  >= 780
#   P.2 = P.1 + 10*x.5  + 25*x.6  + 50*x.7  + 100*x.8  >= 860
#   P.3 = P.2 + 10*x.9  + 25*x.10 + 50*x.11 + 100*x.12 >= 950
#   P.4 = P.3 + 10*x.13 + 25*x.14 + 50*x.15 + 100*x.16 >= 1060
#   P.5 = P.4 + 10*x.17 + 25*x.18 + 50*x.19 + 100*x.20 >= 1180
# Before entering these into the models, we get all constants to the
# right-hand side.
# The variables x.1 - x.20 should be limited to positive integers.
# Constraints 6-25 are non-negativity constraints :     
#     x.1,...,x.20 >= 0

### ---------------PROGRAM OUTPUT-------------------

# Objective function
Z    <- c(300,460,670,950,
          250,375,558,790,
          200,350,465,670,
          170,280,380,550,
          145,235,320,460)

# Functionality constraints
f.1  <- c(10,25,50,100
          , 0, 0, 0,  0
          , 0, 0, 0,  0
          , 0, 0, 0,  0
          , 0, 0, 0,  0)
g.1  <- c(30)                               
f.2  <- c(10,25,50,100
          ,10,25,50,100
          , 0, 0, 0,  0
          , 0, 0, 0,  0
          , 0, 0, 0,  0)
g.2  <- c(110)
f.3  <- c(10,25,50,100
          ,10,25,50,100
          ,10,25,50,100
          , 0, 0, 0,  0
          , 0, 0, 0,  0)
g.3  <- c(200)
f.4  <- c(10,25,50,100
          ,10,25,50,100
          ,10,25,50,100
          ,10,25,50,100
          , 0, 0, 0,  0) 
g.4  <- c(310)
f.5  <- c(10,25,50,100
          ,10,25,50,100
          ,10,25,50,100
          ,10,25,50,100
          ,10,25,50,100)       
g.5  <- c(430)

# non-negativity constraints
f.6  <- c(1,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0)  # x.1 >= 0
g.6  <- c(0)                                        
f.7  <- c(0,1,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0)  # x.2 >= 0                       
g.7  <- c(0)
f.8  <- c(0,0,1,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0)  # x.3 >= 0 and so on..
g.8  <- c(0)
f.9  <- c(0,0,0,1
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0)        
g.9  <- c(0)
f.10 <- c(0,0,0,0
          ,1,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0)
g.10 <- c(0)
f.11 <- c(0,0,0,0
          ,0,1,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0)
g.11 <- c(0)
f.12 <- c(0,0,0,0
          ,0,0,1,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0)
g.12 <- c(0)
f.13 <- c(0,0,0,0
          ,0,0,0,1
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0)
g.13 <- c(0)
f.14 <- c(0,0,0,0
          ,0,0,0,0
          ,1,0,0,0
          ,0,0,0,0
          ,0,0,0,0)
g.14 <- c(0)
f.15 <- c(0,0,0,0
          ,0,0,0,0
          ,0,1,0,0
          ,0,0,0,0
          ,0,0,0,0)
g.15 <- c(0)
f.16 <- c(0,0,0,0
          ,0,0,0,0
          ,0,0,1,0
          ,0,0,0,0
          ,0,0,0,0)
g.16 <- c(0)
f.17 <- c(0,0,0,0
          ,0,0,0,0
          ,0,0,0,1
          ,0,0,0,0
          ,0,0,0,0)
g.17 <- c(0)
f.18 <- c(0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,1,0,0,0
          ,0,0,0,0)
g.18 <- c(0)
f.19 <- c(0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,1,0,0
          ,0,0,0,0)
g.19 <- c(0)
f.20 <- c(0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,1,0
          ,0,0,0,0)
g.20 <- c(0)
f.21 <- c(0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,1
          ,0,0,0,0)
g.21 <- c(0)
f.22 <- c(0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,1,0,0,0)
g.22 <- c(0)
f.23 <- c(0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,1,0,0)
g.23 <- c(0)
f.24 <- c(0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,1,0)
g.24 <- c(0)
f.25 <- c(0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,0
          ,0,0,0,1)
g.25 <- c(0)

# We now build the input into our simplex function.

# Noting that variables 1-20 should be integers
int <- 1:20

# For our constraints:

# Manually the number of constraints are typed in
c.l <- 0 # For less-than-or-equal-to constraints
c.m <- 25 # For more-than-or-equal-to constraints
c.e <- 0 # For equal-to constraints

n  <- length(Z)   # amount of x-variables.

A.l <- NULL 
b.l <- NULL
A.m <- matrix(c(f.1,f.2,f.3,f.4,f.5,   # functionality constraints
                f.6 ,f.7 ,f.8 ,f.9 ,   # non-negativity constraints
                f.10,f.11,f.12,f.13,
                f.14,f.15,f.16,f.17,
                f.18,f.19,f.20,f.21,
                f.22,f.23,f.24,f.25)
              ,nrow=c.m,ncol=n, byrow=TRUE)
b.m <- matrix(c(g.1,g.2,g.3,g.4,g.5,   # functionality constraints
                g.6 ,g.7 ,g.8 ,g.9 ,   # non-negativity constraints
                g.10,g.11,g.12,g.13,
                g.14,g.15,g.16,g.17,
                g.18,g.19,g.20,g.21,
                g.22,g.23,g.24,g.25)
              ,nrow=c.m,ncol=1, byrow=TRUE)
A.e <- NULL
b.e <- NULL

A <- rbind(A.l,A.m,A.e)
b <- rbind(b.l,b.m,b.e)

const.dir <- c(rep("<=",c.l),rep(">=",c.m),rep("=",c.e))

### ------------------CALCULATIONS-------------------


results <- lp (direction = "min", objective.in=Z, const.mat = A, 
               const.dir, const.rhs=b,
               int.vec = int, all.int=FALSE, all.bin=FALSE,
               num.bin.solns=1, use.rw=FALSE)

results$solution
results$objval
# The result of the simplex algorithm is that the 
# optimal value is 3130
sum(Z*results$solution)
# In fact, the optimal value is 3115 for the vector
resvec <- c(0,0,0,1
            ,1,0,0,0
            ,0,0,0,1
            ,0,0,0,1
            ,0,1,0,1)
resvec
sum(Z*resvec)

### --------------------PLOT--------------------

### ------------------PLOT END-------------------

# END

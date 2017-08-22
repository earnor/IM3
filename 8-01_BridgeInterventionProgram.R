# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 8.1
# | PROBLEM NAME: Bridge Intervention Program
# | UPDATE: AE     
# | DESCRIPTION: An optimisation problem based on maximising 
# |              profit within possible resource constraints.
# |              The problem is solved using integer constraints.
# | 
# | KEYWORDS: optimisation, linear programming, resources
# *----------------------------------------------------------------

# Note: To run this program, you must have installed the packages
#       'linprog'and 'lpSolve'

### ------------------DATA AND PACKAGE IMPORT-----------------
library(linprog)
library(lpSolve)

### ------------------VARIABLE DEFINITION---------------------

# Z is a vector of coefficients of the objective function.
# f is a vector of left side of functional constraints, with
# indices from 1
# g is the corresponding right side of the constraint

# A.l is the collection of all constraints' left sides (f) that have 
# a less than or equal to sign.
# b.l is the corresponding right side (g).
# A and b also carry the indices .m for constraints that has a more 
# than or equal to sign, and .e for one that has an equal sign.
# The constraint is then constructed using A*x=b where x is a vector
# of variables

### ---------------VARIABLE INPUT-------------------
# Based on the following objective function and constraints, 
# we build the model
# Z =   4800*x.1 +   5500*x.2 +   5000*x.3 - 
#     10'000*x.4 - 80'000*x.5 - 90'000*x.6
# where x.4, x.5 and x.6 are binary variables. 
# If x.1 > 0, x.4 = 1 and so on.

# Constraint 1 is engineer capacity   : 
# 200*x.1 + 300*x.2 + 600*x.3 <= 60'000
# Constraint 2 is contractor capacity : 
# 600*x.1 + 300*x.2 + 400*x.3 <= 30'000 
# Constraint 3 is PM capacity         : 
# 500*x.1 + 600*x.2 + 200*x.3 <= 40'000
# In order to make sure that the number of interventions do not 
# exceed the possible amount of hours to be worked on a particular 
# bridge type, we introduce a link to the maximum potential value 
# they can have. The 600 hours needed by contractors is limiting for
# steel bridges. If all available 30'000 hours are spent on steel 
# bridges, we can say:
# Constraint 4 is a linking constraint for steel : 
#  (30'000/600)*x.4 - x.1 >= 0
# Constraint 5 is a linking constraint for concr : 
#  (40'000/600)*x.5 - x.2 >= 0
# Constraint 6 is a linking constraint for mason : 
#  (30'000/400)*x.6 - x.3 >= 0
# Constraints 7, 8 and 9 are non-negativity constraints :     
#   x.1,x.2,x.3 >= 0

### ---------------PROGRAM OUTPUT-------------------

# Objective function of length n
Z   <- c(4800,5500,5000
         ,-10000,-80000,-90000) 
# Constraints
f.1 <- c(200,300,600,0,0,0)       
g.1 <- c(60000)                               
f.2 <- c(600,300,400,0,0,0)
g.2 <- c(30000)
f.3 <- c(500,600,200,0,0,0)
g.3 <- c(40000)
f.4 <- c(-1,0,0,30000/600,0,0) 
g.4 <- c(0)
f.5 <- c(0,-1,0,0,40000/600,0)       
g.5 <- c(0)
f.6 <- c(0,0,-1,0,0,30000/400)
g.6 <- c(0)
f.7 <- c(1,0,0,0,0,0)                # non-negativity constraints
g.7 <- c(0)
f.8 <- c(0,1,0,0,0,0)       
g.8 <- c(0)
f.9 <- c(0,0,1,0,0,0)       
g.9 <- c(0)

# We now build the input into our simplex function.

# Noting that variables 1-3 the objective function should be 
# integers and 4-6 binary:
int <- 1:3
bin <- 4:6

# For our constraints:

# Manually the number of constraints are typed in
c.l <- 3 # For less-than-or-equal-to constraints
c.m <- 6 # For more-than-or-equal-to constraints
c.e <- 0 # For equal-to constraints

n  <- length(Z)   # amount of x-variables.

A.l <- matrix(c(f.1,f.2,f.3),            nrow=c.l,ncol=n, byrow=TRUE)
b.l <- matrix(c(g.1,g.2,g.3),            nrow=c.l,ncol=1, byrow=TRUE)
A.m <- matrix(c(f.4,f.5,f.6,f.7,f.8,f.9),nrow=c.m,ncol=n, byrow=TRUE)
b.m <- matrix(c(g.4,g.5,g.6,g.7,g.8,g.9),nrow=c.m,ncol=1, byrow=TRUE)
A.e <- NULL
b.e <- NULL

A <- rbind(A.l,A.m,A.e)
b <- rbind(b.l,b.m,b.e)

const.dir <- c(rep("<=",c.l),rep(">=",c.m),rep("=",c.e))

### ------------------CALCULATIONS-------------------


solveLP(cvec=Z, bvec=b, Amat=A, maximum = TRUE,
        const.dir,
        maxiter = 1000, zero = 1e-9, tol = 1e-6, dualtol = tol,
        lpSolve = FALSE, solve.dual = FALSE, verbose = 0 )

# We note that solveLP cannot operate the binary and integer 
# constraints. Instead we use the lpSolve package, which is 
# similarly built, 


resultsint <- lp (direction = "max"
                  , objective.in =Z
                  , const.mat = A
                  , const.dir
                  , const.rhs=b
                  , int.vec = int
                  , binary.vec=bin
                  , all.int=FALSE, all.bin=FALSE
                  , num.bin.solns=1, use.rw=FALSE)

resultsnoint <- lp (direction = "max"
                    , objective.in=Z
                    , const.mat = A
                    , const.dir
                    , const.rhs=b,
                    int.vec = NULL
                    , binary.vec=bin
                    , all.int=FALSE, all.bin=FALSE,
                    num.bin.solns=1, use.rw=FALSE)

resultsint

### --------------------PLOT--------------------

### ------------------PLOT END-------------------

# END

# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 7.2
# | PROBLEM NAME: Road Section Monitoring, Part B
# | UPDATE: AE     
# | DESCRIPTION: This script is based on its original for problem 
# |              7.1. If changes are made, so that the optimal 
# |              solution changes, the value 'op' will need to be
# |              changed in order to correct the graphing of the
# |              objective function at the optimal value.
# | 
# | KEYWORDS: simplex, optimisation, objective function, operational
# |           research,
# *-----------------------------------------------------------------

# Note that the packes linprog and lpSolve are needed for running 
# this program

### ------------------DATA AND PACKAGE IMPORT-----------------
library(linprog)
library(lpSolve)
### ------------------VARIABLE DEFINITION---------------------

## This exercise is mainly about changing variables to maximise value 
## of Z. Try altering the variables, and observe the change to the 
## optimal solution given by the simplex code. Noting these values on 
## paper may be helpful, as well as noticing graphically how altering 
## certain values affects the drawn constraints and objective function

Z   <- c(1400,1600) # For question A, we change these
f.1 <- c(2,4)       
g.1 <- c(28)        # For question B
f.2 <- c(5,5)
g.2 <- c(50)
f.3 <- c(1,0)
g.3 <- c(8)
f.4 <- c(0,1)
g.4 <- c(6)
f.5 <- c(1,0)       # non-negativity constraints
g.5 <- c(0)
f.6 <- c(0,1)
g.6 <- c(0)

# Below, are the original values of the model, to make it easier 
# to restore 

# Z = 1400*x.1 + 1600*x.2
# Constraint 1 is the capacity of data checker's time : 
# 2*x.1 + 4*x.2 <= 28 
# Constraint 2 is the capacity of inspector's time    : 
# 5*x.1 + 5*x.2 <= 50 
# Constraint 3 is the maximum friction test measurement capacity : 
# x.1 <= 8 
# Constraint 4 is the maximum load bearing measurement capacity : 
# x.2 <= 6
# Constraints 5 and 6 are the non-negativity constraints. 
# x.1,x.2 >= 0

### ---------------VARIABLE INPUT-------------------

c.l <- 4 # Manually the number of constraints are typed in
c.m <- 2
c.e <- 0

n  <- length(Z)   # amount of x-variables.

A.l <- matrix(c(f.1,f.2,f.3,f.4),nrow=c.l,ncol=n, byrow=TRUE) 
b.l <- matrix(c(g.1,g.2,g.3,g.4),nrow=c.l,ncol=1, byrow=TRUE)
A.m <- matrix(c(f.5,f.6),        nrow=c.m,ncol=n, byrow=TRUE)
b.m <- matrix(c(g.5,g.6),        nrow=c.m,ncol=1, byrow=TRUE)
A.e <- NULL
b.e <- NULL


A <- rbind(A.l,A.m,A.e)
b <- rbind(b.l,b.m,b.e)

const.dir <- c(rep("<=",c.l),rep(">=",c.m),rep("=",c.e))

### ---------------PROGRAM OUTPUT-------------------

### ------------------CALCULATIONS-------------------

results <- solveLP(cvec=Z, bvec=b, Amat=A
                   , maximum = TRUE, const.dir
                   , maxiter = 1000, zero = 1e-9, dualtol = 1e-6
                   , lpSolve = FALSE, solve.dual = TRUE, verbose = 0 )

results

# We now analyse the results, by calling the command:

results$allvar

# This is the matrix of results regarding all variables
# (including constraints (S)):

# 1st column = optimal values;
# 2nd column = values of vector c;
# 3rd column = minimum of vector c that does not change the solution;
# 4th column = maximum of vector c that does not change the solution;
# 5th column = derivatives to the objective function;
# 6th column = valid region for these derivatives

# We can thus change the profit of friction tests to 1'600 mu/km, 
# given there are no other changes and the profit of load carrying 
# capacity tests to 2'800 mu/km without changing the optimal point. 

# Let us now introduce the maximum value for the first coefficient

Z.o <- Z

Z <- c(results$allvar[1,4],Z.o[2])

# Z <- c(Z.o[1],1400)

results <- solveLP(cvec=Z, bvec=b, Amat=A
                   , maximum = TRUE, const.dir
                   , maxiter = 1000, zero = 1e-9, dualtol = 1e-6
                   , lpSolve = FALSE, solve.dual = TRUE, verbose = 0 )


### --------------------PLOT--------------------
# We plot the objective function through 0 to show the function's 
# slope. It is then plotted in the middle as well as through the 
# optimal point. The optimal point must be changed manually when 
# altered as part of the exercise.

# We begin by defining vectors of equal length
x  <- c(-2:12)
y0 <- c(-2:12)
y1 <- c(-2:12)
y2 <- c(-2:12)
op <- results$opt            # optimal point
for(i in 1:length(x))
{
  # The objective function in form y = rx + s
  y0[i] <- (0    -Z[1]*x[i])/Z[2] 
  y1[i] <- (7400 -Z[1]*x[i])/Z[2]
  y2[i] <- (op   -Z[1]*x[i])/Z[2]
  # We can alter the optimal point above as per results of simplex
  # calculations
}

plot(x,y0,type="l", lty=2
     ,ylim=c(-1,10)
     ,xlim=c(-1,10)
     ,xlab="x.1"
     ,ylab="x.2")
par(new=TRUE)
plot(x,y1,type="l", lty=2
     ,ylim=c(-1,10)
     ,xlim=c(-1,10)
     ,xlab="x.1"
     ,ylab="x.2")
par(new=TRUE)
plot(x,y2,type="l", lty=2
     ,ylim=c(-1,10)
     ,xlim=c(-1,10)
     ,xlab="x.1"
     ,ylab="x.2")
plot.window(c(-1,10),c(-1,10))
par(mar=c(5, 5, 5, 5) + 0.1)

for(i in 1:length(b))
{
  # for y = rx + s
  if(A[i,2]>0)
  {
    r <- -A[i,1]/A[i,2]
    s <- b[i]/A[i,2]
    abline(b=r,a=s,col=i)
  }
  else
  {
    abline(v=b[i],col=i) # Special function to create vertical line
  }
}

# In comparison to the graph before, the objective function line now 
# tilts and lies on the constraint and still has the same optimal 
# point. We can see that more tilting will result in another optimal
# solution.

# Likewise, to answer question b, by moving the black line data 
# checker's constraint, we alter the optimal point. But changing the 
# capacity to more than a maximum of 32 hours, would not be wise as 
# another constraint will become the limiting constraint.

# To answer question c, we consider the difference of the objective 
# function results of relieving the system of their constraints. We 
# see graphically that this will yield an optimal point of 
# (x.1,x.2)=(8,6). The optimal yield is 20'800, an increase of 
# 6'000 mu. People should thus not be hired for more than 6'000 mu.

### ------------------PLOT END-------------------

# END

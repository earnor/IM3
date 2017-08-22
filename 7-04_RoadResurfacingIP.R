# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 7.4
# | PROBLEM NAME: Road resurfacing intervention program
# | UPDATE: AE     
# | DESCRIPTION: This program uses the simplex function from the 
# |              boot package and displays the optimal value of the 
# |              objective function in the R console. 
# | 
# | KEYWORDS: simplex, optimisation, objective function, operational
# |           research,
# *-----------------------------------------------------------------

# Note: To run this program, you must have installed the packages
#       'linprog' and 'lpSolve'

### ------------------DATA AND PACKAGE IMPORT-----------------
library(linprog)
library(lpSolve)

### ------------------VARIABLE DEFINITION---------------------

# Z is a vector of coefficients of the objective function.
# f is a vector of left side of functional constraints, with 
# indices from 1
# g is the corresponding right side of the constraint

# A.l is the collection of all constraints' left sides (f) that have a
# less than or equal to sign.
# b.l is the corresponding right side (g).
# A and b also carry the indices .m for constraints that has a more 
# than or equal to sign, and .e for one that has an equal sign.
# The constraint is then constructed using A*x=b where x is a vector
# of variables

# Lastly, m is the length of constraint set, and n is the amount 
# of variables.

### ---------------VARIABLE INPUT-------------------
# Based on the following objective function and constraints, 
# we build the model:
#   Z = 3*x.1 + 4*x.2

# But let us transform this, so the variables represent the number 
# of days spent producing each type of asphalt.
# Z = 13'750*x.1 + 15'000*x.2
# where x.1 is the number of days HMAC paved and x.2 of WMAC paved 
# in days.

# We want to maximise profit.
 
# Constraint of production, producing all 20 km of road : 
#                    550*x.2    + 450*x.2    = 15'600
# Cost constraint is 91'667*x.1 + 93'750*x.2 < 2'860'000 
# Constraint on emission is the following : 
#                    458.3*x.1  + 300*x.2    < 13

# Boundary and non-negativity constraints from 0 to 25 days are:
#                    1*x.1      + 0*x.2      < 25
#                    0*x.1      + 1*x.2      < 25
#                    1*x.1      + 0*x.2      > 0
#                    0*x.1      + 1*x.2      > 0

# We set up the model
Z    <- c(13750,15000) # Objective function
f.1  <- c(550,450)
g.1  <- c(15600)     
f.2  <- c(91667,93750)
g.2  <- c(2860000)        
f.3  <- c(0.4583,0.300)
g.3  <- c(13)        
f.4  <- c(1,0)
g.4  <- c(25)        
f.5  <- c(0,1)
g.5  <- c(25)        
f.6  <- c(1,0)
g.6  <- c(0)        
f.7  <- c(0,1)
g.7  <- c(0)        

# We now build the input into our simplex function.

# For more information on the function, type '?simplex' in the 
# console. To see the programming of the function, hold Ctrl and 
# mouse-click on the function below.

# In our example, constraints 1-4 are less than or equal to 
# inequalities and are thus inserted into vector A.l, and the 
# non-negativity constraints into A.m. A.e is empty. 

c.l <- 4  # Manually the number of constraints are typed in
c.m <- 2
c.e <- 1  

n  <- length(Z)   # amount of x-variables.

A.l <- matrix(c(f.2,f.3,f.4,f.5)
              , nrow=c.l
              , ncol=n
              , byrow=TRUE)
b.l <- matrix(c(g.2,g.3,g.4,g.5)
              , nrow=c.l
              , ncol=1
              , byrow=TRUE)
A.m <- matrix(c(f.6,f.7)
              , nrow=c.m
              , ncol=n
              , byrow=TRUE)
b.m <- matrix(c(g.6,g.7)
              , nrow=c.m
              , ncol=1
              , byrow=TRUE)
A.e <- matrix(c(f.1)
              , nrow=c.e
              , ncol=n
              , byrow=TRUE)
b.e <- matrix(c(g.1)
              , nrow=c.e
              , ncol=1
              , byrow=TRUE) 

A <- rbind(A.l,A.m,A.e)
b <- rbind(b.l,b.m,b.e)

const.dir <- c(rep("<=",c.l),rep(">=",c.m),rep("=",c.e))

### ---------------PROGRAM OUTPUT-------------------

### ------------------CALCULATIONS-------------------

results <- solveLP(cvec=Z, bvec=b, Amat=A, maximum = TRUE
                   , const.dir, maxiter = 1000
                   , zero = 1e-9
                   , tol = 1e-6, dualtol = tol
                   , lpSolve = TRUE, solve.dual = FALSE
                   , verbose = 0 )

results

# The amount of days spent on each pavement type production
results$solution

### --------------------PLOT--------------------


# We plot the objective function through 0 to show the 
# function's slope. It is then plotted in the middle as well as
# through the optimal point

# We begin by defining vectors of equal length
x  <- c(-2:40)
y0 <- c(-2:40)
y1 <- c(-2:40)
y2 <- c(-2:40)
for(i in 1:length(x))
{
  # The objective function in form y = rx + s
  y0[i] <- (0        -Z[1]*x[i])/Z[2] 
  y1[i] <- (220000   -Z[1]*x[i])/Z[2]
  y2[i] <- (441999   -Z[1]*x[i])/Z[2]
  # 441999 is the value of the objective function at optimal point.
  # 220000 in middle
}

plot(x,y0,type="l", lty=2
     ,ylim=c(-1,35)
     ,xlim=c(-1,35)
     ,xlab="x.1"
     ,ylab="x.2")
par(new=TRUE)
plot(x,y1,type="l", lty=2
     ,ylim=c(-1,35)
     ,xlim=c(-1,35)
     ,xlab="x.1"
     ,ylab="x.2")
par(new=TRUE)
plot(x,y2,type="l", lty=2
     ,ylim=c(-1,35)
     ,xlim=c(-1,35)
     ,xlab="x.1"
     ,ylab="x.2")
plot.window(c(-1,35),c(-1,35))
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

legend("topright",c("obj funct."
                    ,"constraints"
                    ,"solution set"
                    ,"optim. pt")
       ,cex=0.6,lty=c(2,1,NA,NA),col=c(1,1,rgb(0,0,0.7,0.5),1),lwd=1
       ,pch=c(NA,NA,15,1),pt.cex=c(NA,NA,0.8,0.8)) # add legend

# The following is manually input:

# Enter the coordinates of the polygon of the possible region
coord.x <- c(0,0,5,17.0,25,25,0)
coord.y <- c(0,25,25,13.8,4,0,0)

polygon(coord.x,coord.y, col=rgb(0,0,0.7,0.5),border=NA)
points(17,13.8,type="p") # In our case, the optimal point is here.


### ------------------PLOT END-------------------

# END

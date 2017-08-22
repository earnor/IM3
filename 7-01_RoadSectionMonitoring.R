# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 7.1
# | PROBLEM NAME: Road Section Monitoring
# | UPDATE: AE     
# | DESCRIPTION: This program uses the simplex function from the 
# |              boot package and displays the optimal value of the 
# |              objective function in the R console. The program
# |              then plots the constraints of the problem, 
# |              objective function, region within constraints, and
# |              the optimal point.
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

# A.l is the collection of all constraints' left sides (f) that 
# have a less than or equal to sign.
# b.l is the corresponding right side (g).
# A and b also carry the indices .m for constraints that has a more 
# than or equal to sign, and .e for one that has an equal sign.
# The constraint is then constructed using A*x=b where x is a vector 
# of variables

# Lastly, m is the length of constraint set, and n is the amount 
# of variables.

### ---------------VARIABLE INPUT-------------------
# Based on the following objective function and constraints, we build 
# the model 
# Z = 1400*x.1 + 1600*x.2
# Constraint 1 is the capacity of data checker's time : 
# 2*x.1 + 4*x.2 <= 28 
# Constraint 2 is the capacity of inspector's time    : 
# 5*x.1 + 5*x.2 <= 50 
# Constraint 3 is the maximum road length measurement capacity : 
# x.1 <= 8 
# Constraint 4 is the maximum road length measurement capacity : 
# x.2 <= 6
# Constraints 5 and 6 are the non-negativity constraints. 
# x.1,x.2 >= 0

# Objective function of length n
Z   <- c(1400,1600) 
# f defines coefficients of x1 and x2 value of first constraint
f.1 <- c(2,4)       
# g defines constraint value.
g.1 <- c(28)        
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

# We now build the input into our simplex function.

# For more information on the function, type '?simplex'
# in the console. To see the programming of the function, hold 
# Ctrl and mouse-click on the function below.

# In our example, constraints 1-4 are less than or equal to 
# inequalities and are  thus inserted into vector A.l, and the 
# non-negativity constraints into A.m. A.e is empty. 

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

solveLP(cvec=Z, bvec=b, Amat=A, maximum = TRUE,
         const.dir,
         maxiter = 1000, zero = 1e-9, tol = 1e-6, dualtol = tol,
         lpSolve = FALSE, solve.dual = FALSE, verbose = 0 )

### --------------------PLOT--------------------
# We plot the objective function through 0 to show the function's 
# slope. It is then plotted in the middle as well as through the 
# optimal point

# We begin by defining vectors of equal length
x  <- c(-2:12)
y0 <- c(-2:12)
y1 <- c(-2:12)
y2 <- c(-2:12)

for(i in 1:length(x))
{
  # The objective function in form y = rx + s
  y0[i] <- (0    -Z[1]*x[i])/Z[2] 
  y1[i] <- (7400 -Z[1]*x[i])/Z[2]
  y2[i] <- (14800-Z[1]*x[i])/Z[2]
  # 14800 is the value of the objective function at optimal point. 
  # 7400 in middle
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

legend("topright",c("obj funct."
                    ,"constraints"
                    ,"solution set"
                    ,"optim. pt")
       ,cex=0.6,lty=c(2,1,NA,NA),col=c(1,1,rgb(0,0,0.7,0.5),1),lwd=1
       ,pch=c(NA,NA,15,1),pt.cex=c(NA,NA,0.8,0.8)) # ADD LEGEND

# The following is manually input:
# Enter the coordinates of the polygon of the possible region
coord.x <- c(0,0,2,6,8,8,0)
coord.y <- c(0,6,6,4,2,0,0)

polygon(coord.x,coord.y, col=rgb(0,0,0.7,0.5),border=NA)
points(6,4,type="p") # In our case, the optimal point is here.


### ------------------PLOT END-------------------

# END

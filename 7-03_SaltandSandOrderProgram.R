# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 7.3
# | PROBLEM NAME: Salt and Sand Ordering Program
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
# f is a vector of left side of functional constraints, with indices 
# from 1. 
# g is the corresponding right side of the constraint

# A.l is the collection of all constraints' left sides (f) that have 
# a 'less than or equal to' sign.
# b.l is the corresponding right side (g).
# A and b also carry the indices .m for constraints that has a more 
# than or equal to sign, and .e for one that has an equal sign.
# The constraint is then constructed using A*x=b where x is a vector
# of variables

# Lastly, m is the length of constraint set, and n is the amount of
# variables.

### ---------------VARIABLE INPUT-------------------
# Based on the following objective function and constraints, we 
# build the model:
# C.1 = 200*x.1 + 160*y.1
# C.2 = 250*x.2 + 140*y.2
# C.3 = 300*x.3 + 170*y.3
# where x is the amount of sand purchased and y the amount of salt.

# These are the cost sums from each supplier. In two years, we thus 
# have 12 variables. Notice that the indices of the variables change 
# to conform with the output of the simplex calculations. Our model
# is thus sum of all C

# Z = 200*x.1 + 160*x.2 + 250*x.3 + 140*x.4  + 300*x.5  + 170*x.6 +
#     200*x.7 + 160*x.8 + 250*x.9 + 140*x.10 + 300*x.11 + 170*x.12
# We further note that indices 1-6 are for year 1 and 7-12 are for 
# year 2 as well as odd numbers being for sand and even indices for
# salt.

# Constraints 1-12 are the minimum purchase of salt and sand : 
# x.1,...,x.12 >= 50
# Constraints 13-16 amount spent on each supplier is same : 
#       200*x.1 + 160*x.2 = 250*x.3 + 140*x.4 = 300*x.5 + 170*x.6
#                   can thus be translated to : 
#                   200*x.1 + 160*x.2 - 250*x.3 - 140*x.4 = 0 , and
#                   250*x.3 + 140*x.4 - 300*x.5 - 170*x.6 = 0
#                   and repeated for the second year :
#                   200*x.7 + 160*x.8  - 250*x.9 -  140*x.10 = 0 and
#                   250*x.9 + 140*x.10 - 300*x.11 - 170*x.12 = 0
# Constraint 17-20 are demand constraints : 
#                   x.1 + x.3  + x.5  = 350 for sand in year 1
#                   x.2 + x.4  + x.6  = 500 for salt in year 1
#                   x.7 + x.9  + x.11 = 600 for sand in year 2
#                   x.8 + x.10 + x.12 = 700 for salt in year 2

# We set up the model
Z    <- c(200,160,250,140,300,170,200,160,250,140,300,170) 
# Objective function
# Now we set up constraints, where the index is the number of the
# constraint.
# The f is the left side of the constraint with length being 12, the
# number of
# variables in the objective function.
f.1  <- c(1,0,0,0,0,0,0,0,0,0,0,0) # Constraint: x.1 >= 50
g.1  <- c(50)     
f.2  <- c(0,1,0,0,0,0,0,0,0,0,0,0) # Constraint: x.2 >= 50
g.2  <- c(50)        
f.3  <- c(0,0,1,0,0,0,0,0,0,0,0,0) # and so on...
g.3  <- c(50)        
f.4  <- c(0,0,0,1,0,0,0,0,0,0,0,0)
g.4  <- c(50)        
f.5  <- c(0,0,0,0,1,0,0,0,0,0,0,0)
g.5  <- c(50)        
f.6  <- c(0,0,0,0,0,1,0,0,0,0,0,0)
g.6  <- c(50)        
f.7  <- c(0,0,0,0,0,0,1,0,0,0,0,0)
g.7  <- c(50)        
f.8  <- c(0,0,0,0,0,0,0,1,0,0,0,0)
g.8  <- c(50)     
f.9  <- c(0,0,0,0,0,0,0,0,1,0,0,0)
g.9  <- c(50)     
f.10 <- c(0,0,0,0,0,0,0,0,0,1,0,0)
g.10 <- c(50)     
f.11 <- c(0,0,0,0,0,0,0,0,0,0,1,0)
g.11 <- c(50)     
f.12 <- c(0,0,0,0,0,0,0,0,0,0,0,1)
g.12 <- c(50)     

f.13 <- c(200,160,-250,-140,  0 ,  0 ,   0,   0,   0,   0,   0,   0)
g.13 <- c(0)     
f.14 <- c(  0,  0, 250, 140,-300,-170,   0,   0,   0,   0,   0,   0)
g.14 <- c(0)     
f.15 <- c(  0,  0,   0,   0,   0,   0, 200, 160,-250,-140,   0,   0)
g.15 <- c(0)     
f.16 <- c(  0,  0,   0,   0,   0,   0,  0 ,  0 , 250, 140,-300,-170)
g.16 <- c(0)  


f.17 <- c(1,0,1,0,1,0,0,0,0,0,0,0)
g.17 <- c(350)     
f.18 <- c(0,1,0,1,0,1,0,0,0,0,0,0)
g.18 <- c(500) 
f.19 <- c(0,0,0,0,0,0,1,0,1,0,1,0)
g.19 <- c(600)     
f.20 <- c(0,0,0,0,0,0,0,1,0,1,0,1)
g.20 <- c(700) 

# We now build the input into our simplex function, solveLP.

# For more information on the function, type '?solveLP' in the
# console. To see the programming of the function, hold Ctrl and
# mouse-click on the function below.

c.l <- 0  # Manually the number of constraints are typed in
c.m <- 12
c.e <- 8  # Constraints 13-20 are all equality constraints

n  <- length(Z)   # amount of x-variables.

A.l <- NULL
b.l <- NULL
A.m <- matrix(c(f.1,f.2,f.3,f.4,f.5,f.6,f.7,f.8,f.9,f.10,f.11,f.12)
              , nrow=c.m
              , ncol=n
              , byrow=TRUE)
b.m <- matrix(c(g.1,g.2,g.3,g.4,g.5,g.6,g.7,g.8,g.9,g.10,g.11,g.12)
              , nrow=c.m
              , ncol=1
              , byrow=TRUE)
A.e <- matrix(c(f.13,f.14,f.15,f.16,f.17,f.18,f.19,f.20)
              , nrow=c.e
              , ncol=n
              , byrow=TRUE)
b.e <- matrix(c(g.13,g.14,g.15,g.16,g.17,g.18,g.19,g.20)
              , nrow=c.e
              , ncol=1
              , byrow=TRUE) 

A <- rbind(A.l,A.m,A.e)
b <- rbind(b.l,b.m,b.e)

const.dir <- c(rep("<=",c.l),rep(">=",c.m),rep("=",c.e))

### ---------------PROGRAM OUTPUT-------------------

### ------------------CALCULATIONS-------------------

results <- solveLP(cvec=Z, bvec=b, Amat=A
                   , maximum = FALSE
                   , const.dir, maxiter = 1000
                   , zero = 1e-9, tol = 1e-6
                   , dualtol = tol
                   , lpSolve = TRUE, solve.dual = FALSE
                   , verbose = 0 )

results
table1 <- matrix(Z,               nrow=2,ncol=6,byrow=FALSE)
table2 <- matrix(results$solution,nrow=2,ncol=6,byrow=FALSE)    
tab <- rbind(table1,table2)
table <- data.frame(tab)
rownames(table) <- c("Unit Cost Sand"
                     ,"Unit Cost Salt"
                     ,"Quantity Sand"
                     ,"Quantity Salt")
colnames(table) <- c("Year 1; Supplier A"
                     ,"Supplier B"
                     ,"Supplier C"
                     ,"Year 2; Supplier A"
                     ,"Supplier B"
                     ,"Supplier C")
table
results$opt

### --------------------PLOT--------------------
# No plot
### ------------------PLOT END-------------------

# END

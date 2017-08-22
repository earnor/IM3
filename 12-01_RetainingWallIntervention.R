# *----------------------------------------------------------------
# | PROBLEM NUMBER: 12.1
# | PROBLEM NAME: Retaining Wall Intervention Program
# | UPDATE: AE     
# | DESCRIPTION: The optimal intervention time of a retaining 
# |              wall is found using the optim package.
# | 
# | KEYWORDS: retaining, wall, non-linear, cost per unit time
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
library(graphics)

### ------------------VARIABLE DEFINITION---------------------

# Z is a vector of coefficients of the objective function.
# grr is the gradient, the derivative of Z

### ---------------VARIABLE INPUT-------------------

### ---------------PROGRAM OUTPUT-------------------

### ------------------CALCULATIONS-------------------


Z <- function(x) {   
  x <- x[1]
  v <- 10000
  d <- 0.2
  c <- 20000
  i <- 0.1
  (v*d/x)+(c*d)+(x/2*i*c)
}

grr <- function(x) { 
  x <- x[1]
  v <- 10000
  d <- 0.2
  c <- 20000
  i <- 0.2
  i*c/2-v*d/(x^2)
}

optim(c(2), Z, grr, method = "L-BFGS-B",lower=1) 


# The results show the wall should be upgraded by 1.39 states each 
# time meaning an intervention is required every 
1.39/0.2
# time units

# END

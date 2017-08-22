# *----------------------------------------------------------------
# | PROBLEM NUMBER: 11.1
# | PROBLEM NAME: Communication Tower Construction Program
# | UPDATE: AE     
# | DESCRIPTION: The optimal location of a communication tower is
# |              found using the optim package.
# | 
# | KEYWORDS: Communication, non-linear, minimum, distance
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------
library(graphics)

### ------------------VARIABLE DEFINITION---------------------

# Z is a vector of coefficients of the objective function.
# grr is the gradient, the derivative of Z

# The x and y coordinates of each city are also defined.

### ---------------VARIABLE INPUT-------------------

### ---------------PROGRAM OUTPUT-------------------

### ------------------CALCULATIONS-------------------


Z <- function(x) {   
  x1 <- x[1]
  x2 <- x[2]
  x.a <- 5
  y.a <- 45
  x.b <- 12
  y.b <- 21
  x.c <- 17
  y.c <- 5
  x.d <- 52
  y.d <- 21
  sqrt((x.a-x1)^2+(y.a-x2)^2)+
    sqrt((x.b-x1)^2+(y.b-x2)^2)+
    sqrt((x.c-x1)^2+(y.c-x2)^2)+
    sqrt((x.d-x1)^2+(y.d-x2)^2)
}

grr <- function(x) { 
  x1 <- x[1]
  x2 <- x[2]
  x.a <- 5
  y.a <- 45
  x.b <- 12
  y.b <- 21
  x.c <- 17
  y.c <- 5
  x.d <- 52
  y.d <- 21
  c(2*(x1-x.a)/sqrt((x.a-x1)^2+(y.a-x2)^2)+
      2*(x1-x.b)/sqrt((x.b-x1)^2+(y.b-x2)^2)+
      2*(x1-x.c)/sqrt((x.c-x1)^2+(y.c-x2)^2)+
      2*(x1-x.d)/sqrt((x.d-x1)^2+(y.d-x2)^2),
    2*(x2-y.a)/sqrt((x.a-x1)^2+(y.a-x2)^2)+
      2*(x2-y.b)/sqrt((x.b-x1)^2+(y.b-x2)^2)+
      2*(x2-y.c)/sqrt((x.c-x1)^2+(y.c-x2)^2)+
      2*(x2-y.d)/sqrt((x.d-x1)^2+(y.d-x2)^2))
}

optim(c(-1.2,1), Z, grr, method = "L-BFGS-B") 

# The results show the optimal location of the tower is at 12.2,21.0


# END

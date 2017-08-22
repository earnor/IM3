# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 10.1
# | PROBLEM NAME: Conference Facility Construction
# | UPDATE: AE     
# | DESCRIPTION: For the modelling of programs with multiple 
# |              objectives, we place weights on the various 
# |              objectives and maximise the fulfilment of all 
# |              of them. They thus become relative to each other.
# | 
# | KEYWORDS: Deviation, conference, multiple, objective, cost,
# |           area, 
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------

library(linprog)
library(lpSolve)


### ------------------VARIABLE DEFINITION---------------------

## First we look at the problem. We know we should have five small
## rooms, 10 medium and 15 large. We also want the total area to be 
## larger than 9000 m2 for marketing reasons. We know costs of each 
## room which should not total more than 3'000 mus.

# We provide the variables with the following indices
# 1: small rooms
# 2: medium rooms
# 3: large rooms
# 4: floor area
# 5: cost

# Instead of having variables in the objective function, we instead 
# look at the deviation made from the expected value, and place a value
# on the positive and negative deviation. For example, there is no 
# direct value in more than 5 small rooms, but we weigh it heavily if 
# we do not include 5 of them. At the same time we weigh both negative 
# and positive deviations of the total area negatively. We want to have
# the largest conference center, but there is no need for it to be
# larger than it must, i.e. 9000 m2.




### ---------------VARIABLE INPUT-------------------

# Given d stands for deviation, n stands for the negative deviation, 
# p for positive and w for weight. An example is thus wn.1 for the
# weighting of the negative deviation for variable 1, small rooms.
# We set the weighting beforehand, but optimise for the deviation.

# In addition we add the actual optimised values as variables to the 
# Objective function to be able to use them in the constraints, but
# they are not used as part of the minimisation process and therefore 
# have a value of 0.

# We define the objective function as the following.

# Z =    wn.1 / 5 * dn.1   +    wp.1 / 5 * dp.1 + 
#       wn.2 / 10 * dn.2   +   wp.2 / 10 * dp.2 +
#       wn.3 / 15 * dn.3   +   wp.3 / 15 * dp.3 +
#     wn.4 / 9000 * dn.4   + wp.4 / 9000 * dp.4 +
#     wn.5 / 3000 * dn.5   + wp.5 / 3000 * dp.5 +
#      0* x.1 + 0* x.2 + 0* x.3 + 0* x.4 + 0* x.5

# For indexing in the model, we give the negative deviations odd 
# numbers and the positive deviations positive numbers ranging from 
# 1-10 and x 11-15.

# For the weights:
wn.1 <- 1 # Having less than 5 small rooms is given an importance of 1
wp.1 <- 0 # We don't care if there are more than 5 small rooms.
wn.2 <- 1 
wp.2 <- 0
wn.3 <- 1 
wp.3 <- 0
wn.4 <- 1 # It's considered very important to have more than 9'000 m2.
wp.4 <- 1 # It is also important to not have more.
wn.5 <- 0 # We don't mind reaching our objectives on a lower budget
wp.5 <- 1 # We do mind if the budget exceeds 3'000 mus.


# The constraints we set up in the following manner:

# We know that all variables take a positive value, or 0: 
#   x.1,...,x.15 >= 0
# All variables should also be considered integers.

# Constraints 16-18 are the room constraints
# The modelled amount of rooms, plus the negative deviations, minus 
# positive deviations should be equal to the minimum number of rooms.
#   x.1  + dn.1 - dp.1 = 5
# is translated into
#   x.11 + x.1  - x.2  = 5
#   x.12 + x.3  - x.4  = 10 # for medium rooms
#   x.13 + x.5  - x.6  = 15 # for large rooms.

# For floor area, knowing that small rooms are 125 m2, medium 250 m2 
# and large 300 m2, we can say that
#   125*x.11 + 250*x.12 + 300*x.13 + x.7 - x.8 = 9000

# For Budget, knowing small rooms cost 60 mus, medium 100 mus and 
# large 152 mus we can set up the budget constraint
#   60*x.11 + 100*x.12 + 152*x.13 + x.9 - x.10 = 3000

### ---------------PROGRAM OUTPUT-------------------

Z <- c(wn.1/5 , wp.1/5 
       , wn.2/10 , wp.2/10 
       , wn.3/15 , wp.3/15 
       , wn.4/9000 , wp.4/9000 
       , wn.5/3000 , wp.5/3000 
       ,0,0,0,0,0)
Z

# Non-negativity constraints
f.1  <- c( 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.1  <- c(0)
f.2  <- c( 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.2  <- c(0)
f.3  <- c( 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.3  <- c(0)
f.4  <- c( 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.4  <- c(0)
f.5  <- c( 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.5  <- c(0)
f.6  <- c( 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
g.6  <- c(0)
f.7  <- c( 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
g.7  <- c(0)
f.8  <- c( 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
g.8  <- c(0)
f.9  <- c( 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
g.9  <- c(0)
f.10 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
g.10 <- c(0)
f.11 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
g.11 <- c(0)
f.12 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
g.12 <- c(0)
f.13 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
g.13 <- c(0)
f.14 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
g.14 <- c(0)
f.15 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
g.15 <- c(0)

# Room constraints

f.16 <- c( 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
g.16 <- c(5)
f.17 <- c( 0, 0, 1,-1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
g.17 <- c(10)
f.18 <- c( 0, 0, 0, 0, 1,-1, 0, 0, 0, 0, 0, 0, 1, 0, 0)
g.18 <- c(15)

# Area constraints

f.19 <- c( 0, 0, 0, 0, 0, 0, 1,-1, 0, 0,125,250,300,  0,  0)
g.19 <- c(9000)

# Budget constraints

f.20 <- c( 0, 0, 0, 0, 0, 0, 0, 0, 1,-1, 60,100,152,  0,  0)
g.20 <- c(3000)


# As all variables should be integers, we define int

n   <- length(Z)    # amount of x-variables.
int <- 1:n

# For our constraints:

# Manually the number of constraints are typed in
c.l <- 0  # For less-than-or-equal-to constraints
c.m <- 15 # For more-than-or-equal-to constraints
c.e <- 5  # For equal-to constraints


A.l <- NULL 
b.l <- NULL
A.m <- matrix(c(f.1,f.2,f.3,f.4,f.5
                ,f.6,f.7,f.8,f.9,f.10
                ,f.11,f.12,f.13,f.14,f.15)
              ,nrow=c.m,ncol=n, byrow=TRUE)
b.m <- matrix(c(g.1,g.2,g.3,g.4,g.5
                ,g.6,g.7,g.8,g.9,g.10
                ,g.11,g.12,g.13,g.14,g.15)
              ,nrow=c.m,ncol=1, byrow=TRUE)
A.e <- matrix(c(f.16,f.17,f.18,f.19,f.20)
              ,nrow=c.e,ncol=n, byrow=TRUE)
b.e <- matrix(c(g.16,g.17,g.18,g.19,g.20)
              ,nrow=c.e,ncol=1, byrow=TRUE)

A <- rbind(A.l,A.m,A.e)
b <- rbind(b.l,b.m,b.e)

const.dir <- c(rep("<=",c.l),rep(">=",c.m),rep("=",c.e))


### ------------------CALCULATIONS-------------------

results <- lp (direction = "min", objective.in=Z, const.mat = A, 
               const.dir, const.rhs=b,
               int.vec = int, all.int=FALSE, all.bin=FALSE,
               num.bin.solns=1, use.rw=FALSE)

# Although the objective function includes for the variables of cost 
# and area, they are not accounted for in the above constraints. We  
# thus correct the results vector using the derivations.

results$solution[14] <- g.19 - 
  results$solution[7] + 
  results$solution[8]
results$solution[15] <- g.20 - 
  results$solution[9] + 
  results$solution[10]

# And we then print the results

results$solution

# Based on the weightings given to the w-variables, we can deduce from 
# the results vector the following information:

r.s <- results$solution[11]
r.m <- results$solution[12]
r.l <- results$solution[13]
a.c <- results$solution[14]
C.c <- results$solution[15]

cat("The amount of rooms are"
    , r.s,"small"
    , r.m,"medium, and"
    , r.l,"large rooms.")
cat("The total area of the conference center is"
    , a.c,"m2, costing"
    , C.c,"monetary units")

# If the weighting is changed, say for wn.4 to the number 2, i.e. 
# given more importance that the conference center is not less than 
# 9'000 m2, we observe different results. Try it for yourself.

### --------------------PLOT--------------------

### ------------------PLOT END-------------------

# END

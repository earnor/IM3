# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 4.7
# | PROBLEM NAME: Lock Gates
# | UPDATE: AE     
# | DESCRIPTION: The semi-Markov model is applied for parts A-C of 
# |              this exercise. Parts D to E are repeating parts 
# |              A, B and C and are thus not re-modelled. The code
# |              goes through the methodology of semi-Markov models
# |              for the case of Lock Gates. The program is based
# |              on the package 'markovchain'
# | 
# | KEYWORDS: Lock gates, semi-Markov, steady-state probability 
# |           
# *-----------------------------------------------------------------

## NOTE: The package 'markovchain' must be installed to run this 
##       program

### ------------------DATA AND PACKAGE IMPORT-----------------
rm(list=ls()) # clear the memory and items in R console
library(markovchain) # call the package "markovchain"
### ------------------VARIABLE DEFINITION---------------------


### ---------------VARIABLE INPUT-------------------

N.cr  <- 2    # critical age
N     <- 5    # Maximum age of fan
C.1   <- 1000 # costs if only one fan is replaced
C.2   <- 1600 # costs if both fans are replaced.

# vector of possible values for t, used to calculate 
# conditional probabilities
T <- seq(0,N.cr,by=1) 

p0.a <- c(  0.05
          , 0.1
          , 0.3
          , 0.35
          , 0.2)

p0.b <- c(  0.1
          , 0.2
          , 0.42
          , 0.18
          , 0.1)

### ---------------PROGRAM OUTPUT-------------------

states  <- c(1:(2*N.cr+1))
a       <- matrix(nrow=length(states))
b       <- matrix(nrow=length(states))

# The conditional probabilities are developed for both fans a and b
p.a     <- matrix(nrow=N,ncol=length(T)) 
p.b     <- matrix(nrow=N,ncol=length(T)) 

# The following matrices are used to calculate transition matrix
A       <- matrix(nrow=length(states))
M       <- matrix(nrow=length(states),ncol=length(states)) 
S       <- array(dim=c(length(states))) # Steady state probabilities

# P is a matrix collecting all values for probabilities of leaving 
# a state.
P       <- array(dim=c(N,length(states)))

# X multiplies the probability i,j multiplied by the respective time.
# The sum of this over all time values then equals the mean 
# transition time v.ij
X       <- array(dim=c(N,length(states)))
v       <- array(dim=c(length(states)))

# Omega is the weighted cost, dependent on transitional probability 
omega   <- array(dim=c(length(states)))

# Cost per tu, these matrices collect numerator and denominator 
# for all ij
num     <- array(dim=c(length(states))) # the numerator
den     <- array(dim=c(length(states))) # the denominator

### ------------------CALCULATIONS-------------------

## Step 0: We create the vector of possible states
for (k in 1:(2*N.cr+1))
{
  if (k<=(N.cr+1))
  {
    # For the kth state, this is the age of fan a
    a[k]      <- 0               
    # and this the relative age of fan b
    b[k]      <- k-1              
    # The kth state is then represented by the ages
    states[k] <- paste(a[k],b[k]) 
  } 
  else 
  {
    a[k]      <- k-(N.cr+1)
    b[k]      <- 0
    states[k] <- paste(a[k],b[k])   
  }
}
cat('the state space of the model \n')
print(states)

## Step 1: The conditional probabilities of failure in time i

for (i in 1:N)
{
  for (j in (1:length(T)))
  {
    if (j==1)
    {
      # the probabilities at t=0 are given and go in column 1
      p.a[i,j] <- p0.a[i] 
      p.b[i,j] <- p0.b[i]
    } 
    else if (i<j)
    {        
      # Probability of failure at t=i given currently t=i+1
      p.a[i,j] <- 0       # is obviously 0.
      p.b[i,j] <- 0       
    } 
    else 
    {
      p.a[i,j] <- p.a[i,j-1] / (1-p.a[T[j],T[j]])
      p.b[i,j] <- p.b[i,j-1] / (1-p.b[T[j],T[j]])
    }
  }
}
cat('Probability of failing in year i, fan a \n')
print(p.a)
cat('Probability of failing in year i, fan b \n')
print(p.b)

## Step 2: Create transition matrix of Markov chain 

# In this loop, we first calculate for end-states 0,1 0,2 1,0 
# and 2,0.
# End-state 0,0 will then be calculated in the following loop.

for (i in 1:length(states))
{
  for (j in 1:length(states))
  {     
    if (i==j) 
    {
      # This is true for all i=j except for 0,0->0,0
      M[i,j] <- 0           
    }
    else if (i==3 && j==2)  # 0,2->0,1 is not possible.
    {
      M[i,j] <- 0
    }  
    else if (i==5 && j==4)  # 2,0->1,0 is not possible
    {
      M[i,j] <- 0
    }      
    # When j={2,3}, we know it is fan a that fails.
    else if (j==2 || j==3)  
    {
      B <- 0
      # Time that passes until failure
      u <- abs((b[j]-a[j])-(b[i]-a[i])) 
      # Life-time of non-failed fan
      w <- max(a[j],b[j]) 
      M[i,j] <- p.a[u,(a[i]+1)] * sum(p.b[c((w+1):N),(b[i]+1)])
    } 
    # when end-state, j is 1,0 or 2,0 , fan b fails.
    else if (j==4 || j==5)  
    {
      B <- 0
      u <- abs((b[j]-a[j])-(b[i]-a[i]))
      w <- max(a[j],b[j])
      M[i,j] <- p.b[u,(b[i]+1)] * sum(p.a[c((w+1):N),(a[i]+1)])
    }
    else
    {
      # Here we say transitions for all i->0,0 are equal to zero. 
      M[i,j] <- 0 
    }
  }
}
# To find probabilities of transition i -> 0,0 we first sum all 
# probabilities that have already be found.
# Knowing sum of probabilities must be 1, we can find the first 
# column by subtracting the sum of other probabilities from 1, and
# insert into the Matrix.
for (i in 1:length(states))
{
  for (j in 1:length(states))
  {
    A[i] <- sum(M[i,]) 
  }
}

for (i in 1:length(states))
{
  M[i,1] <- 1-A[i]
}
cat ('Transition probability Matrix \n')
print(M)

# Step 3: Using calculated transition matrix, we find steady 
# state probabilities

classMarkov <- new("markovchain", states=states, transitionMatrix=M)
cat('The steady state probability \n')
S <- t(steadyStates(classMarkov))
print(S)

#Step 4a: Estimating the probability of system in a state failing 
# in a certain year

for (j in 1:length(states)) ## (LOOK AT RETURN LINES)
{
  for (i in 1:N)
  {
    c <- abs(a[j]-b[j]) # Difference between fan age. 
    if (j==1)
    {
      if ((i+c)<N)
      {
        r1 <- p.a[i,(a[j]+1)]*p.b[i,(b[j]+1)]
        r2 <- p.a[i,(a[j]+1)]*sum(p.b[c((i+1):N),(b[j]+1)])
        r3 <- p.b[i,(b[j]+1)]*sum(p.a[c((i+1):N),(a[j]+1)])
        P[i,j] <- r1 + r2 + r3
      }
      else if ((i+c)==N) 
        # for j=1, then c=0. So for i=N, P is calc only with r1
      {
        P[i,j] <- p.a[i,(a[j]+1)]*p.b[i,(b[j]+1)]
      }
      else 
      {
        # This is never the case for j=1, but keep it consistent
        P[i,j] <- 0
      }     
    }
    else if (j==2 || j==3) 
    {
      if ((i+c)<N)
      {
        r1 <- p.a[i,(a[j]+1)]*p.b[(i+c),(b[j]+1)]
        r2 <- p.a[i,(a[j]+1)]*sum(p.b[c((i+c+1):N),(b[j]+1)])
        r3 <- p.b[(i+c),(b[j]+1)]*sum(p.a[c((i+1):N),(a[j]+1)])
        P[i,j] <- r1 + r2 + r3
      }
      else if ((i+c)==N) 
      {
        r1 <- p.a[i,(a[j]+1)]*p.b[(i+c),(b[j]+1)]
        r2 <- 0         # for this condition, sum(p.b)=0
        r3 <- p.b[(i+c),(b[j]+1)]*sum(p.a[c((i+1):N),a[j]+1])
        P[i,j] <- r1 + r2 + r3
      }
      else 
      {
        P[i,j] <- 0
      }      
    }
    else if (j==4 || j==5)
    {
      if ((i+c)<N)
      {
        r1 <- p.a[(i+c),(a[j]+1)]*p.b[i,(b[j]+1)]
        r2 <- p.a[(i+c),(a[j]+1)]*sum(p.b[c((i+1):N),(b[j]+1)])
        r3 <- p.b[i,(b[j]+1)]*sum(p.a[c((i+c+1):N),(a[j]+1)])
        P[i,j] <- r1 + r2 + r3
      }
      else if ((i+c)==N)
      {
        r1 <- p.a[(i+c),(a[j]+1)]*p.b[i,(b[j]+1)]
        r2 <- p.a[(i+c),(a[j]+1)]*sum(p.b[c((i+1):N),b[j]+1])
        r3 <- 0
        P[i,j] <- r1 + r2 + r3
      }      
      else 
      {
        P[i,j] <- 0
      }      
    }
  }
}


cat('value of probability of failing for both fan a and fan b \n')
print(P)

## Step 4b: Find mean transition time and average cost of transition
# Start by finding mean transition times for each x from each state 
for (i in 1:N)
{
  for (j in 1: length(states))
  {
    X[i,j] <- P[i,j]*i
  }
}
# Their sum then makes v for each state.
for (j in 1:length(states))
{
  v[j] <- sum(X[(c(1:N)),j])
}
cat('value of mean time the fan system stays in each state \n')
print(v)

for (j in 1:length(states))
{
  omega[j] <- M[j,1]*C.2+(1-M[j,1])*C.1
}
cat('value of average cost for one step transition from 
    condition state i \n')
print(omega)

## Step 4c: calculate the mean cost per units of time

for (i in 1:length(states))
{
  num[i] <- omega[i]*S[i]
  den[i] <- v[i]*S[i]
}

## Step 4d: Costs per unit time:
eta.1 <- sum(num)/sum(den) # mean cost per unit time for IS 1
cat('value of mean costs per units of time \n')
print(eta.1)

## QUESTION B:
# To answer question B we use costs incurred by intervention on both 
# fans divided by mean time the fans operate from a like new
# condition, i.e state 0,0

eta.2 <- C.2 / v[1]
print(eta.2)


## QUESTION C:
# When does it make sense to use IS2 instead of IS1. Let us try to 
# lower the cost of repairing both gates at the same time, and see if
# eta.2<eta.1

C.1   <- 1000 # This cost stays constant
C.2   <- 1600 # Let this be our starting value
# We reduce C.2 by 10 at the beginning of every loop.

while (eta.2>eta.1) 
{
  C.2 <- C.2 - 10
  for (k in 1:(2*N.cr+1))
  {
    if (k<=(N.cr+1))
    {
      a[k]      <- 0               
      b[k]      <- k-1              
      states[k] <- paste(a[k],b[k])
    } 
    else 
    {
      a[k]      <- k-(N.cr+1)
      b[k]      <- 0
      states[k] <- paste(a[k],b[k])   
    }
  }
  for (i in 1:N)
  {
    for (j in (1:length(T)))
    {
      if (j==1)
      {
        p.a[i,j] <- p0.a[i] 
        p.b[i,j] <- p0.b[i]
      } 
      else if (i<j)
      {        
        p.a[i,j] <- 0       
        p.b[i,j] <- 0       
      } 
      else 
      {
        p.a[i,j] <- p.a[i,j-1] / (1-p.a[T[j],T[j]])
        p.b[i,j] <- p.b[i,j-1] / (1-p.b[T[j],T[j]])
      }
    }
  }
  for (i in 1:length(states))
  {
    for (j in 1:length(states))
    {     
      if (i==j) 
      {
        M[i,j] <- 0           
      }
      else if (i==3 && j==2)  
      {
        M[i,j] <- 0
      }  
      else if (i==5 && j==4)  
      {
        M[i,j] <- 0
      }      
      else if (j==2 || j==3)  
      {
        B <- 0
        u <- abs((b[j]-a[j])-(b[i]-a[i])) 
        w <- max(a[j],b[j]) 
        
        M[i,j] <- p.a[u,(a[i]+1)] * sum(p.b[c((w+1):N),(b[i]+1)])
      } 
      else if (j==4 || j==5) 
      {
        B <- 0
        u <- abs((b[j]-a[j])-(b[i]-a[i]))
        w <- max(a[j],b[j])
        
        M[i,j] <- p.b[u,(b[i]+1)] * sum(p.a[c((w+1):N),(a[i]+1)])
      }
      else
      {
        M[i,j] <- 0 
      }
    }
  }
  
  for (i in 1:length(states))
  {
    for (j in 1:length(states))
    {
      A[i] <- sum(M[i,]) 
    }
  }
  
  for (i in 1:length(states))
  {
    M[i,1] <- 1-A[i]
  }
  
  classMarkov <- new("markovchain"
                     , states=states
                     , transitionMatrix=M)
  cat('The steady state probability \n')
  S <- t(steadyStates(classMarkov))
  
  for (j in 1:length(states)) 
  {
    for (i in 1:N)
    {
      c <- abs(a[j]-b[j]) 
      if (j==1)
      {
        if ((i+c)<N)
        {
          r1 <- p.a[i,(a[j]+1)]*p.b[i,(b[j]+1)]
          r2 <- p.a[i,(a[j]+1)]*sum(p.b[c((i+1):N),(b[j]+1)])
          r3 <- p.b[i,(b[j]+1)]*sum(p.a[c((i+1):N),(a[j]+1)])
          P[i,j] <- r1 + r2 + r3
        }
        else if ((i+c)==N) 
        {
          P[i,j] <- p.a[i,(a[j]+1)]*p.b[i,(b[j]+1)]
        }
        else 
        {
          P[i,j] <- 0
        }     
      }
      else if (j==2 || j==3) 
      {
        if ((i+c)<N)
        {
          r1 <- p.a[i,(a[j]+1)]*p.b[(i+c),(b[j]+1)]
          r2 <- p.a[i,(a[j]+1)]*sum(p.b[c((i+c+1):N),(b[j]+1)])
          r3 <- p.b[(i+c),(b[j]+1)]*sum(p.a[c((i+1):N),(a[j]+1)])
          P[i,j] <- r1 + r2 + r3
        }
        else if ((i+c)==N) 
        {
          r1 <- p.a[i,(a[j]+1)]*p.b[(i+c),(b[j]+1)]
          r2 <- 0                                   
          r3 <- p.b[(i+c),(b[j]+1)]*sum(p.a[c((i+1):N),a[j]+1])
          P[i,j] <- r1 + r2 + r3
        }
        else 
        {
          P[i,j] <- 0
        }      
      }
      else if (j==4 || j==5)
      {
        if ((i+c)<N)
        {
          r1 <- p.a[(i+c),(a[j]+1)]*p.b[i,(b[j]+1)]
          r2 <- p.a[(i+c),(a[j]+1)]*sum(p.b[c((i+1):N),(b[j]+1)])
          r3 <- p.b[i,(b[j]+1)]*sum(p.a[c((i+c+1):N),(a[j]+1)])
          P[i,j] <- r1 + r2 + r3
        }
        else if ((i+c)==N)
        {
          r1 <- p.a[(i+c),(a[j]+1)]*p.b[i,(b[j]+1)]
          r2 <- p.a[(i+c),(a[j]+1)]*sum(p.b[c((i+1):N),b[j]+1])
          r3 <- 0
          P[i,j] <- r1 + r2 + r3
        }      
        else 
        {
          P[i,j] <- 0
        }      
      }
    }
  }
  
  for (i in 1:N)
  {
    for (j in 1: length(states))
    {
      X[i,j] <- P[i,j]*i
    }
  }
  for (j in 1:length(states))
  {
    v[j] <- sum(X[(c(1:N)),j])
  }
  
  for (j in 1:length(states))
  {
    omega[j] <- M[j,1]*C.2+(1-M[j,1])*C.1
  }
  
  for (i in 1:length(states))
  {
    num[i] <- omega[i]*S[i]
    den[i] <- v[i]*S[i]
  }
  eta.1 <- sum(num)/sum(den) # mean cost per unit time for IS 1
  cat('value of mean costs per units of time for IS1 \n')
  print(eta.1)
  
  eta.2 <- C.2 / v[1]
  cat('value of mean costs per units of time for IS2 \n')
  print(eta.2)
}
cat('When C.1 stays constant, IS2 becomes feasible when C.2 is')
print(C.2)
### --------------------PLOT--------------------
### ------------------PLOT END-------------------

# END

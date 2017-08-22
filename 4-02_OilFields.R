# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 4.2
# | PROBLEM NAME: Oil Fields
# | UPDATE: AE     
# | DESCRIPTION: The problem revolves around the optimal triggering 
# |              state of oil fields with differing numbers of oil
# |              pumps based on efficiency, chi. The second part of
# |              the problem considers a change in the fixed time 
# |              of the intervention.
# | 
# | KEYWORDS: Oil, gamma, optimisation, efficiency, oil pump, 
# |           
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------

rm(list=ls()) # clear the memory and items in R console

library(ggplot2)
library(tidyr)

### ------------------VARIABLE DEFINITION---------------------

## To define the variables used for the program, we extract the 
## values from the text of Example 4.2 in the script.

# N stands for the number of items
# t.dI.i is the fixed time of the intervention
# t.dI.n is the variable time of the intervention per item
# THETA is the rate parameter of the Gamma distribution

# While programming, we use Upper Case letters to represent items A, 
# B, C etc. Lower case letters relate to the number of the 
# sub-example, for example 4.2.b

### ---------------VARIABLE INPUT-------------------

# The numbers of items at oil fields A-D are the following

N.A <- 40
N.B <- 60   
N.C <- 80
N.D <- 120

t.dI.i    <- 5 # the time unit is days
t.dI.n    <- 1
THETA     <- 0.002


### ---------------PROGRAM OUTPUT-------------------

## We now define the outputs of our program. 

# The following matrices collect the results of the calculations for 
# different variables.

chi.A <- matrix(nrow=N.A)
chi.B <- matrix(nrow=N.B)
chi.C <- matrix(nrow=N.C)
chi.D <- matrix(nrow=N.D)

chi.b0 <- matrix(nrow=N.A)
chi.b1 <- matrix(nrow=N.A)
chi.b2 <- matrix(nrow=N.A)

# The following function is used to calculate the efficiency, chi, as
# a function of different triggering states, k.

chifunction <- function(N,t.dI.i,t.dI.n,THETA,C.bI.o)
{
  W     <- matrix(nrow=N) #total system  time during 1 repair cycle
  t.bI  <- matrix(nrow=N) #failure-free operation time of line i
  t.dI  <- matrix(nrow=N) 
  res   <- matrix(nrow=N) 
  chi   <- matrix(nrow=N) 
  A     <- matrix(nrow=N) 
  for (k in 1:N)
  {
    W[k]  <- k/THETA
    for (i in 1:k)
    {
      if (i==1)
      {
        A[i]=1/(N-i+1)
      } 
      else 
      {
        A[i]=A[i-1]+1/(N-i+1)
      }
    }
    t.bI <- A
  }
  for (k in 1:N)
  {
    t.dI[k]=t.dI.i+t.dI.n*k
    chi[k]=(1/N)*(k/THETA)/((t.bI[k]/THETA)+t.dI[k])
    res[k] <- chi[k]
  }
  return(res)
}
### ------------------CALCULATIONS-------------------

# We now insert different variables into the function, and save the 
# results.

cat('----------------------Question A---------------------------\n')

results <- chifunction(N.A,t.dI.i,t.dI.n,THETA)
chi.A   <- results

results <- chifunction(N.B,t.dI.i,t.dI.n,THETA)
chi.B   <- results

results <- chifunction(N.C,t.dI.i,t.dI.n,THETA)
chi.C   <- results

results <- chifunction(N.D,t.dI.i,t.dI.n,THETA)
chi.D   <- results

cat('----------------------Question B---------------------------\n')

# For Question B

# Question B considers if the fixed intervention time is longer than
# expected
t.dI.i.b1 <- 6
t.dI.i.b2 <- 8

results <- chifunction(N.A,t.dI.i,t.dI.n,THETA)
chi.b0  <- results

results <- chifunction(N.A,t.dI.i.b1,t.dI.n,THETA)
chi.b1  <- results

results <- chifunction(N.A,t.dI.i.b2,t.dI.n,THETA)
chi.b2  <- results


### --------------------PLOT--------------------

# The results are plotted.

cat('----------------------Question A-----------------------  \n')

k.max <- (max(c(N.A,N.B,N.C,N.D)))
# We see that there are various number of items in each oil field
# We thus set up the data frame with NA before gathering
rdfA <- as.data.frame(cbind(1:k.max
                            ,rbind(chi.A,matrix(NA,nrow=(k.max-N.A)))
                            ,rbind(chi.B,matrix(NA,nrow=(k.max-N.B)))
                            ,rbind(chi.C,matrix(NA,nrow=(k.max-N.C)))
                            ,chi.D))
names(rdfA) <- c("k","chiA","chiB","chiC","chiD")
rdfAtidy <- gather(rdfA
                      ,chiA,chiB,chiC,chiD
                      ,key="key",value="value")
# We now remove all rows where the value is NA
rdfAtidy <- rdfAtidy[!is.na(rdfAtidy$value),]

plotA <- ggplot(data=rdfAtidy,aes(x=k,y=value
                                    ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Oil Field",      # Set legend title
                   breaks=c("chiA","chiB","chiC","chiD"),
                   labels=c("\nField A\n"
                            , "\nField B\n"
                            , "\nField C\n"
                            , "\nField D\n")
  )  +
  scale_linetype_discrete(name="Oil Field") +
  xlab("k - number of failed pumps [-]") + 
  ylab("Efficiency [-]") + # Set axis labels
  ylim(0,1) +
  theme_bw(base_size=36) +
  theme(legend.position=c(.85, .8))           # Position legend

# Let us collect the optimal points in the data frame optimal

optimal <- as.data.frame(matrix(nrow=2,ncol=3))
names(optimal) <- c("key","x","y")
key <- c("chiA","chiB","chiC","chiD")

# We remove the NAs for the finding of optimal points.
# This function works for this case since both optimals
# are found within the non-deleted range, k = (0,40]
data <- na.omit(rdfA)
for (i in 1:4)
{
  optimal[i,1] <- key[i]
  optimal[i,2] <- na.omit(data$k[which.max(data[,i+1])])
  optimal[i,3] <- max(data[,i+1])
}

# Now we add these points to the graph before exporting
plotA + geom_point(data=optimal
                      , aes(x=x
                            , y=y)
                      , colour="navyblue"
                      , size=6)

dev.copy(png,'E4-02_A.png',width=2000,height=1330)
dev.off()
cat('END ----------------------------- \n')


cat('----------------------Question B-------------------------- \n')
rdfB <- as.data.frame(cbind(1:N.A,chi.b0,chi.b1,chi.b2))
names(rdfB) <- c("k","t.dI.i5","t.dI.i6","t.dI.i8")
rdfBtidy <- gather(rdfB
                   ,t.dI.i5,t.dI.i6,t.dI.i8
                      ,key="key",value="value")

plotchiB <- ggplot(data=rdfBtidy,aes(x=k,y=value
                                     ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Set-up time",      # Set legend title
                   breaks=c("t.dI.i5","t.dI.i6","t.dI.i8"),
                   labels=c("\nt.dI.i=5\n"
                            , "\nt.dI.i=6\n"
                            , "\nt.dI.i=8\n")
  )  +
  scale_linetype_discrete(name="Set-up time") +
  xlab("k - number of failed items [-]") + 
  ylab("Efficiency [-]") + # Set axis labels
  ylim(0,1) +
  theme_bw(base_size=36) +
  theme(legend.position=c(.85, .8))           # Position legend 

# Let us collect the optimal points in the data frame optimal
optimal <- as.data.frame(matrix(nrow=3,ncol=3))
names(optimal) <- c("key","x","y")
key <- c("t.dI.i5","t.dI.i6","t.dI.i8")

data <- rdfB
for (i in 1:3)
{
  optimal[i,1] <- key[i]
  optimal[i,2] <- data$k[which.max(data[,i+1])]
  optimal[i,3] <- max(data[,i+1])
}

# Now we add these points to the graph before exporting
plotchiB + geom_point(data=optimal
                      , aes(x=x
                            , y=y)
                      , colour="navyblue"
                      , size=6) 

dev.copy(png,'E4-02_B.png',width=2000,height=1330)
dev.off()

### ------------------PLOT END-------------------

# END

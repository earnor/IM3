# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 5.1
# | PROBLEM NAME: Two-lane Slab-on Girder Bridge
# | UPDATE: AE     
# | DESCRIPTION: xxx This program calculates the steady state 
# |              probability of a system dependent on two different
# |              intervention strategies. We use the steady state
# |              probabilities and assumed costs of intervention to
# |              compare the costs of the two IS. We then find an
# |              
# | 
# | KEYWORDS: Markov, transition, intervention, strategy
# |           
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------

rm(list=ls()) #clear the memory and items in R console

# this package will be used for the first passage
library(markovchain) 

library(ggplot2)
library(tidyr)
library(grid)

setwd("Y:/common/IBI/Martani/IM Script - Workspace/Exercises/R-files/Updated")
PdataA <- read.csv("5-01_TwoLaneSlabOnGirderBridge_A-P_csvData.csv"
                  ,header=TRUE,sep=";") 
attach(PdataA)
PdataB <- read.csv("5-01_TwoLaneSlabOnGirderBridge_B-P_csvData.csv"
                  ,header=TRUE,sep=";") 
attach(PdataB)
Rdata1 <- read.csv("5-01_TwoLaneSlabOnGirderBridge_1-R_csvData.csv"
                   ,header=TRUE,sep=";") 
attach(Rdata1)
Rdata2 <- read.csv("5-01_TwoLaneSlabOnGirderBridge_2-R_csvData.csv"
                   ,header=TRUE,sep=";") 
attach(Rdata2)

Cdata <- read.csv("5-01_TwoLaneSlabOnGirderBridge-C_csvData.csv"
                   ,header=TRUE,sep=";") 
attach(Cdata)


### ------------------VARIABLE DEFINITION---------------------

## First we want to define the variables 

# The matrix P contains the transition matrix of the system
# The matrix R contains the transition effectiveness vectors 
# of different IS
# The vector C contains the different costs. 


### ---------------VARIABLE INPUT-------------------

N.c <- length(PdataA[,1]) # maximum number of condition states
T.max <- 200 # number of years

# define the dimension for transition matrix P
P.A <- array(dim=c(N.c,N.c)) 
P.B <- array(dim=c(N.c,N.c)) 
# intervention effectiveness vectors
R.1 <- array(dim=c(2,N.c))
R.2 <- array(dim=c(3,N.c))

C <- array(dim=c(N.c,4))

# Read value of transition matrix and effectiveness- and 
# cost-vectors from csv file
R.1 <- Rdata1
R.2 <- Rdata2
P.A <- PdataA
P.B <- PdataB

C <- Cdata

### ---------------PROGRAM OUTPUT-------------------

### ------------------CALCULATIONS-------------------

# transition matrix corresponding to each MGMT Strategy
Q <- array(dim=c(N.c,N.c,4)) 
for (i in 1:N.c)
{
  for (j in 1:N.c)
  {
    for (k in 1:4)
    {
      if (k==1||k==3) # Intervention strategy 1
      {
        if (k==1)
        {
        Q[i,j,k] <- P.A[i,j]
        Q[4,j,k] <- R.1[1,j]
        Q[5,j,k] <- R.1[2,j]
        } 
        else if (k==3)
        {
            Q[i,j,k] <- P.B[i,j]
            Q[4,j,k] <- R.1[1,j]
            Q[4,1:2,k] <- c(1,0)
            Q[5,j,k] <- R.1[2,j]
        }
        Q[i,j,k] <- Q[i,j,k]
      } 
      else if (k==2||k==4)
      {
        if (k==2)
        {
          Q[i,j,k] <- P.A[i,j]
          Q[3,j,k] <- R.2[1,j]
          Q[4,j,k] <- R.2[2,j]
          Q[5,j,k] <- R.2[3,j]
        } else if (k==4){
          Q[i,j,k] <- P.B[i,j]
          Q[3,j,k] <- R.2[1,j]
          Q[4,j,k] <- R.2[2,j]
          Q[5,j,k] <- R.2[3,j]
          
        }
        Q[i,j,k] <- Q[i,j,k]
      }
    }
  }
}

#
cat('Intervention repair matrix Q \n')
print(Q)

# this is the steady state probability
pi <- array(dim=c(N.c,T.max,4)) 

for (k in 1:4)
{
  for (t in 1:T.max)
  {
    if (t==1)
    {
        pi[,t,k] <- c(1,0,0,0,0)
    } 
    else 
    {
      pi[,t,k] <- pi[,t-1,k]%*%Q[,,k]   # %*% is matrix multiplic.
    }
  }
}

cat('the steady state corresponding to each IS \n')
print(pi[,T.max,])

cat('estimating the cost associated with each strategy \n')
C.is <- pi[,T.max,]*C
print(C.is)
cat('Management strategy 1 : ')
print(sum(C.is[,1]))
cat('Management strategy 2 : ')
print(sum(C.is[,2]))
cat('Management strategy 3 : ')
print(sum(C.is[,3]))
cat('Management strategy 4 : ')
print(sum(C.is[,4]))

cat('The Management strategy with lowest costs is MGMT 3')


### --------------------PLOT--------------------
# The following section is for plotting purposes. 


data <- as.data.frame(t(pi[,,1]))
data <- cbind(1:T.max,data)
names(data) <- c("t","CS1","CS2","CS3","CS4","CS5")
datatidy <- gather(data
                   ,CS1,CS2,CS3,CS4,CS5
                   ,key="key",value="value")

plotMS1 <- ggplot(datatidy, aes(x=t,y=value,group=key)) +
  ggtitle("Management Strategy 1") +
  geom_area(aes(fill = key)) +
  geom_line(aes(group = key), position = "stack") +
  # scale_colour_hue(name="Condition state",      # Set legend title
  #                  breaks=c("CS1","CS2","CS3","CS4","CS5"),
  #                  labels=c("\nCS1\n"
  #                           , "\nCS2\n"
  #                           , "\nCS3\n"
  #                           , "\nCS4\n"
  #                           , "\nCS5\n")
  # )  +
  xlim(0,50)+
  scale_fill_discrete(name="Condition\nstate") +
  xlab("") +
  ylab("Share in each CS [-]") + # Set axis labels
  theme_bw(base_size=36) +
  # This removes all legends
  theme(legend.position="none")

plotMS1

# New IS
data <- as.data.frame(t(pi[,,2]))
data <- cbind(1:T.max,data)
names(data) <- c("t","CS1","CS2","CS3","CS4","CS5")
datatidy <- gather(data
                   ,CS1,CS2,CS3,CS4,CS5
                   ,key="key",value="value")

plotMS2 <- ggplot(datatidy, aes(x=t,y=value,group=key)) +
  ggtitle("Management Strategy 2") +
  geom_area(aes(fill = key)) +
  geom_line(aes(group = key), position = "stack") +
  # scale_colour_hue(name="Condition state",      # Set legend title
  #                  breaks=c("CS1","CS2","CS3","CS4","CS5"),
  #                  labels=c("\nCS1\n"
  #                           , "\nCS2\n"
  #                           , "\nCS3\n"
  #                           , "\nCS4\n"
  #                           , "\nCS5\n")
  # )  +
  xlim(0,50)+
  scale_fill_discrete(name="Condition\nstate") +
  xlab("") +
  ylab("") + # Set axis labels
  theme_bw(base_size=36) +
  # This removes all legends
  theme(legend.position="none")

plotMS2

# New IS
data <- as.data.frame(t(pi[,,3]))
data <- cbind(1:T.max,data)
names(data) <- c("t","CS1","CS2","CS3","CS4","CS5")
datatidy <- gather(data
                   ,CS1,CS2,CS3,CS4,CS5
                   ,key="key",value="value")

plotMS3 <- ggplot(datatidy, aes(x=t,y=value,group=key)) +
  ggtitle("Management Strategy 3") +
  geom_area(aes(fill = key)) +
  geom_line(aes(group = key), position = "stack") +
  # scale_colour_hue(name="Condition state",      # Set legend title
  #                  breaks=c("CS1","CS2","CS3","CS4","CS5"),
  #                  labels=c("\nCS1\n"
  #                           , "\nCS2\n"
  #                           , "\nCS3\n"
  #                           , "\nCS4\n"
  #                           , "\nCS5\n")
  # )  +
  xlim(0,50)+
  scale_fill_discrete(name="Condition\nstate") +
  xlab("Time [years]") +
  ylab("Share in each CS [-]") + # Set axis labels
  theme_bw(base_size=36) +
  # This removes all legends
  theme(legend.position="none")

plotMS3

# New IS
data <- as.data.frame(t(pi[,,4]))
data <- cbind(1:T.max,data)
names(data) <- c("t","CS1","CS2","CS3","CS4","CS5")
datatidy <- gather(data
                   ,CS1,CS2,CS3,CS4,CS5
                   ,key="key",value="value")

plotMS4 <- ggplot(datatidy, aes(x=t,y=value,group=key)) +
  ggtitle("Management Strategy 4") +
  geom_area(aes(fill = key)) +
  geom_line(aes(group = key), position = "stack") +
  scale_colour_hue(name="Condition\nstate\n",      # Set legend title
                   breaks=c("CS1","CS2","CS3","CS4","CS5"),
                   labels=c("\nCS1\n"
                            , "\nCS2\n"
                            , "\nCS3\n"
                            , "\nCS4\n"
                            , "\nCS5\n")
  )  +
  xlim(0,50)+
  scale_fill_discrete(name="Condition\nstate\n") +
  xlab("Time [years]") +
  ylab("") + # Set axis labels
  theme_bw(base_size=36)


plotMS4

# Let us plot the figures on a multiplot

# We first define the function:
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) 
  {
  require(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) 
  {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) 
  {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout)
                                               , ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) 
    {
      # Get the i,j matrix positions of the regions 
      # that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# Now the function has been defined, we proceed to create the plot

l <- 81 #length
s <- 8  #align with legend
c <- as.integer((l-s)/2) #center
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, l)))
vplayout <- function(x, y) viewport(layout.pos.row = x
                                    , layout.pos.col = y)
print(plotMS1, vp = vplayout(1, 1:c))  
# key is to define vplayout
print(plotMS2, vp = vplayout(1, (c+1):(l-s)) )
print(plotMS3, vp = vplayout(2, 1:c) ) 
print(plotMS4, vp = vplayout(2, (c+1):l) ) 

dev.copy(png,'E5-01_MS.png',width=1900,height=1600)
dev.off()


### ------------------PLOT END-------------------


# END

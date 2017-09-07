# *-----------------------------------------------------------------
# | PROBLEM NUMBER: 4.1
# | PROBLEM NAME: Wind Mill Parks
# | UPDATE: AE     
# | DESCRIPTION: As an energy company operating Wind Mill Parks, the
# |              efficiency of operating the park with an IS is 
# |              considered, as well as the effect of a change in
# |              duration of intervention.
# | 
# | KEYWORDS: Wind mill, trigger, optimisation, cost, efficiency, 
# |           state-dependent, 
# *-----------------------------------------------------------------

### ------------------DATA AND PACKAGE IMPORT-----------------

rm(list=ls()) # clear the memory and items in R console

library(ggplot2)
library(tidyr)

### ------------------VARIABLE DEFINITION---------------------

## First we want to define the variables used for the program.

### ---------------VARIABLE INPUT-------------------

# Question A

N       <- 10   # number of items
C.bI.o  <- 100  # Money received if item in operation
C.bI.f  <- 0    # Penalty if item not operating
t.dI.i  <- 2    # fixed down-time
t.dI.n  <- 1    # variable down-time per failed item
THETA   <- 0.02 # the rate parameter

# For Question B, the original Wind mill park becomes Park B and 
# Park A isassigned the following variables:

N.A       <- 5
C.bI.o.A  <- 100  # Money received if the system in operation
t.dI.i.A  <- 2    # weeks on the site (fix)
t.dI.n.A  <- 1    # additional time L
THETA.A   <- 0.02

N.B       <- N        # number of items
C.bI.o.B  <- C.bI.o   # Money received if item in operation
t.dI.i.B  <- t.dI.i   # fixed down-time
t.dI.n.B  <- t.dI.n   # variable down-time per failed item
THETA.B   <- THETA 

t.dI.i.c <- seq(from=0, to=5, by=1)
t.dI.n.d <- seq(from=0.5, to=4, by=0.5)

### ---------------PROGRAM OUTPUT-------------------

# Define empty matrices for eta and chi for parts c and d of 
# the exercise

eta.c <- matrix(ncol=length(t.dI.i.c), nrow=N)
eta.d <- matrix(ncol=length(t.dI.n.d), nrow=N)
chi.c <- matrix(ncol=length(t.dI.i.c), nrow=N)
chi.d <- matrix(ncol=length(t.dI.n.d), nrow=N)


chifunction <- function(N,t.dI.i,t.dI.n,THETA,C.bI.o)
{
  W     <- matrix(nrow=N) #total system time during 1 repair cycle
  t.bI  <- matrix(nrow=N) 
  t.dI  <- matrix(nrow=N) 
  res   <- matrix(ncol=2, nrow=N) 
  eta   <- matrix(nrow=N) 
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
    eta[k]=C.bI.o*(k/THETA)/((t.bI[k]/THETA)+t.dI[k])
    chi[k]=(1/N)*(k/THETA)/((t.bI[k]/THETA)+t.dI[k])
    res[k,1] <- eta[k]
    res[k,2] <- chi[k]
  }
  return(res)
}




### ------------------CALCULATIONS-------------------

cat('----------------------Question A--------------------------\n')

results <- chifunction(N,t.dI.i,t.dI.n,THETA,C.bI.o)
eta.a   <- results[,1]
chi.a   <- results[,2]
print('Optimal Intervention Strategy is T=')
print(which.max(eta.a))
print('where eta=')
print(max(eta.a))
print('Sensitivity can be analysed on the Graph.')

cat('----------------------Question B--------------------------\n')

eta.B <- eta.a
chi.B <- chi.a

results <- chifunction(N.A,t.dI.i.A,t.dI.n.A,THETA.A,C.bI.o.A)
eta.A   <- results[,1]
chi.A   <- results[,2]

print('When k=2 is the triggering state, the efficiency for each
      wind mill park')
print('Park A')
print(chi.A[2])
print('Park B')
print(chi.B[2])

cat('----------------------Question C--------------------------\n')

# For Question C, the original wind mill park is used to consider  
# how sensitive fixed down-time is to change. We consider t0 to be
# on the range 1-5 weeks.

for (u in 1:length(t.dI.i.c))
{
  results <- chifunction(N,t.dI.i.c[u],t.dI.n,THETA,C.bI.o)
  eta   <- results[,1]
  chi   <- results[,2]
  
  eta.c[,u] <- eta
  chi.c[,u] <- chi
}
print('As the fixed duration of intervention varies, so does the
      optimal IS')
print('For N=1 to N=5, k varies from k=2 to k=4')

cat('----------------------Question D--------------------------\n')

# For Question D, the original wind mill park is used to consider 
# how sensitive variable down-time is to change. We consider L to be
# on the range 0.5-4 weeks.



for (v in 1:length(t.dI.n.d))
{
  results <- chifunction(N,t.dI.i,t.dI.n.d[v],THETA,C.bI.o)
  eta   <- results[,1]
  chi   <- results[,2]
  
  eta.d[,v] <- eta
  chi.d[,v] <- chi
}

print('Regardless of length of variable intervention time, the
      optimal IS is k=3')

### --------------------PLOT--------------------
## The following code is only for plotting the figure given as 
## a solution to the Exercise

rdf <- as.data.frame(cbind(1:N,eta.a))
names(rdf) <- c("k","eta")

plotetaT <- ggplot(data=rdf,aes(x=k,y=eta)) + 
  geom_line(size=2,color="red") + 
  xlab("k - number of failed items [-]") + 
  ylab("Average cost per year [mu/tu]") + # Set axis labels
  scale_x_continuous(breaks = seq(0, max(rdf$k), by = 1)) +
  ylim(0,1000) +
  theme_bw(base_size=36) 

# Let us collect the optimal points in the data frame optimal

optimal <- as.data.frame(matrix(nrow=1,ncol=2))
names(optimal) <- c("x","y")

data <- rdf
optimal[1,1] <- data$k[which.max(data$eta)]
optimal[1,2] <- max(data$eta)

optlines <- data.frame(key = c("z","z")
                       , x0 = c(min(rdf$k)
                                ,optimal$x)
                       , x1 = c(optimal$x
                                ,optimal$x)
                       , y0 = c(optimal$y,optimal$y)
                       , y1 = c(optimal$y,0))

# Now we add these points to the graph before exporting
plotetaT + geom_point(data=optimal
                      , aes(x=x
                            , y=y)
                      , colour="navyblue"
                      , size=6) +
  annotate("text", label="Optimal - 688\nk=3"
           , x=optimal[1,1]+1, y=optimal[1,2]-80
           , size=12) +
  annotate("text", label="n=10\nt.dI.i=2\nt.dI.n=1\ntheta=0.02"
           , x=9.5, y=900
           , size=12) +
  geom_segment(aes(x = x0, y = y0
                   , xend = x1, yend = y1)
               , data = optlines
               , size = 1.5
               , color= "grey") # Also draw lines to optimal point



dev.copy(png,'E4-01_etaA.png',width=2000,height=1330)
dev.off()


# Plot for Question B

rdfB <- as.data.frame(cbind(1:N,eta.A,eta.B))
names(rdfB) <- c("k","etaA","etaB")
# We see that there are only 5 items in Park A
# We thus empty the data frame before gathering
rdfB$etaA[6:10] <- NA 
rdfBtidy <- gather(rdfB
                   ,etaA,etaB
                   ,key="key",value="value")
# We now remove all rows where the value is NA
rdfBtidy <- rdfBtidy[!is.na(rdfBtidy$value),]

plotetaB <- ggplot(data=rdfBtidy,aes(x=k,y=value
                                     ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Wind mill park",      # Set legend title
                   breaks=c("etaA","etaB"),
                   labels=c("\nPark A\n"
                            , "\nPark B\n")
  )  +
  scale_x_continuous(breaks = seq(0, max(rdfBtidy$k), by = 1)) +
  scale_linetype_discrete(name="Wind mill park") +
  xlab("k - number of failed items [-]") + 
  ylab("Average cost per year [mu/tu]") + # Set axis labels
  ylim(0,1000) +
  theme_bw(base_size=36) +
  theme(legend.position=c(.85, .8))           # Position legend

# Let us collect the optimal points in the data frame optimal

optimal <- as.data.frame(matrix(nrow=2,ncol=3))
names(optimal) <- c("key","x","y")
key <- c("etaA","etaB")

# We remove the NAs for the finding of optimal points.
# This function works for this case since both optimals
# are found within the non-deleted range, k = (0,5]
data <- na.omit(rdfB)
for (n in 1:2)
{
  optimal[n,1] <- key[n]
  optimal[n,2] <- na.omit(data$k[which.max(data[,n+1])])
  optimal[n,3] <- max(data[,n+1])
  opt <- data.frame(key = c("z","z")
                    , x0 = c(min(data$k)
                             ,optimal$x[n])
                    , x1 = c(optimal$x[n]
                             ,optimal$x[n])
                    , y0 = c(optimal$y[n],optimal$y[n])
                    , y1 = c(optimal$y[n],0))
  if      (n==1) { optlines <- opt} 
  else if (n!=1) { optlines <- rbind(optlines,opt)}
}

# Now we add these points to the graph before exporting
plotetaB + geom_point(data=optimal
                      , aes(x=x
                            , y=y)
                      , colour="navyblue"
                      , size=6) +
  annotate("text", label="Optimal B - 688\nk=3"
           , x=optimal[2,2]+0.75, y=optimal[2,3]-70
           , size=12) +
  annotate("text", label="Optimal A - 384\nk=1"
           , x=optimal[1,2]+0.75, y=optimal[1,3]-70
           , size=12) +
  geom_segment(aes(x = x0, y = y0
                   , xend = x1, yend = y1)
               , data = optlines
               , size = 1.5
               , color= "grey") # Also draw lines to optimal point



dev.copy(png,'E4-01_etaB.png',width=2000,height=1330)
dev.off()

# Find efficiency

rdfBchi <- as.data.frame(cbind(1:N,chi.A,chi.B))
names(rdfBchi) <- c("k","chiA","chiB")
# We see that there are only 5 items in Park A
# We thus empty the data frame before gathering
rdfBchi$chiA[6:10] <- NA 
rdfBchitidy <- gather(rdfBchi
                      ,chiA,chiB
                      ,key="key",value="value")
# We now remove all rows where the value is NA
rdfBchitidy <- rdfBchitidy[!is.na(rdfBchitidy$value),]

plotchiB <- ggplot(data=rdfBchitidy,aes(x=k,y=value
                                        ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Wind mill park",      # Set legend title
                   breaks=c("chiA","chiB"),
                   labels=c("\nPark A\n"
                            , "\nPark B\n")
  )  +
  scale_linetype_discrete(name="Wind mill park") +
  scale_x_continuous(breaks = seq(0, max(rdfBchitidy$k), by = 1)) +
  xlab("k - number of failed items [-]") + 
  ylab("Efficiency [-]") + # Set axis labels
  ylim(0,1) +
  theme_bw(base_size=36) +
  theme(legend.position=c(.85, .8))           # Position legend 

# Let us collect the optimal points in the data frame optimal

optimal <- as.data.frame(matrix(nrow=2,ncol=3))
names(optimal) <- c("key","x","y")
key <- c("chiA","chiB")

# We remove the NAs for the finding of optimal points.
# This function works for this case since both optimals
# are found within the non-deleted range, k = (0,5]
data <- na.omit(rdfBchi)
for (n in 1:2)
{
  optimal[n,1] <- key[n]
  optimal[n,2] <- na.omit(data$k[which.max(data[,n+1])])
  optimal[n,3] <- max(data[,n+1])
  opt <- data.frame(key = c("z","z")
                    , x0 = c(min(data$k)
                             ,optimal$x[n])
                    , x1 = c(optimal$x[n]
                             ,optimal$x[n])
                    , y0 = c(optimal$y[n],optimal$y[n])
                    , y1 = c(optimal$y[n],0))
  if      (n==1) { optlines <- opt} 
  else if (n!=1) { optlines <- rbind(optlines,opt)}
}

# Now we add these points to the graph before exporting
plotchiB + geom_point(data=optimal
                      , aes(x=x
                            , y=y)
                      , colour="navyblue"
                      , size=6) +
  annotate("text", label="Optimal B - 0.688\nk=3"
           , x=optimal[2,2]+0.5, y=optimal[2,3]+0.060
           , size=12) +
  annotate("text", label="Optimal A - 0.755\nk=1"
           , x=optimal[1,2]+0.5, y=optimal[1,3]+0.060
           , size=12) +
  geom_segment(aes(x = x0, y = y0
                   , xend = x1, yend = y1)
               , data = optlines
               , size = 1.5
               , color= "grey") # Also draw lines to optimal point



dev.copy(png,'E4-01_chiB.png',width=2000,height=1330)
dev.off()


# Plot for Question C

rdfC <- as.data.frame(cbind(1:N,eta.c))
names(rdfC) <- c("k","t0","t1","t2","t3","t4","t5")

rdfCtidy <- gather(rdfC
                   ,t0,t1,t2,t3,t4,t5
                   ,key="key",value="value")

plotC <- ggplot(data=rdfCtidy,aes(x=k,y=value
                                  ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Set-up time",      # Set legend title
                   breaks=c("t0","t1","t2","t3","t4","t5"),
                   labels=c("\nt.dI.i=0\n"
                            , "\nt.dI.i=1\n"
                            , "\nt.dI.i=2\n"
                            , "\nt.dI.i=3\n"
                            , "\nt.dI.i=4\n"
                            , "\nt.dI.i=5\n")
  )  +
  scale_linetype_discrete(name="Set-up time") +
  scale_x_continuous(breaks = seq(0, max(rdfCtidy$k), by = 1)) +
  xlab("k - number of failed items [-]") + 
  ylab("Average cost per year [mu/tu]") + # Set axis labels
  ylim(0,1000) +
  theme_bw(base_size=36) +
  theme(legend.position=c(.9, .7))           # Position legend

# Let us collect the optimal points in the data frame optimal

optimal <- as.data.frame(matrix(nrow=6,ncol=3))
names(optimal) <- c("key","x","y")
key <- c("t0","t1","t2","t3","t4","t5")

data <- rdfC
for (i in 1:6)
{
  optimal[i,1] <- key[i]
  optimal[i,2] <- na.omit(data$k[which.max(data[,i+1])])
  optimal[i,3] <- max(data[,i+1])
}

# Now we add these points to the graph before exporting
plotC + geom_point(data=optimal
                   , aes(x=x
                         , y=y)
                   , colour="navyblue"
                   , size=6) 

dev.copy(png,'E4-01_etaC.png',width=2000,height=1330)
dev.off()

# Plot for Question D

rdfD <- as.data.frame(cbind(1:N,eta.d))
names(rdfD) <- c("k","t0.5","t1.0","t1.5","t2.0"
                 ,"t2.5","t3.0","t3.5","t4.0")

rdfDtidy <- gather(rdfD
                   ,t0.5,t1.0,t1.5,t2.0,t2.5,t3.0,t3.5,t4.0
                   ,key="key",value="value")

plotD <- ggplot(data=rdfDtidy,aes(x=k,y=value
                                  ,group=key,color=key)) + 
  geom_line(size=2) + 
  scale_colour_hue(name="Set-up time",      # Set legend title
                   breaks=c("t0.5","t1.0","t1.5","t2.0"
                            ,"t2.5","t3.0","t3.5","t4.0"),
                   labels=c( "\nt.dI.n=0.5\n"
                             , "\nt.dI.n=1.0\n"
                             , "\nt.dI.n=1.5\n"
                             , "\nt.dI.n=2.0\n"
                             , "\nt.dI.n=2.5\n"
                             , "\nt.dI.n=3.0\n"
                             , "\nt.dI.n=3.5\n"
                             , "\nt.dI.n=4.0\n")
  )  +
  scale_linetype_discrete(name="Repair time") +
  scale_x_continuous(breaks = seq(0, max(rdfDtidy$k), by = 1)) +
  xlab("k - number of failed items [-]") + 
  ylab("Average cost per year [mu/tu]") + # Set axis labels
  ylim(0,1000) +
  theme_bw(base_size=36) +
  theme(legend.position=c(.9, .7))           # Position legend

# Let us collect the optimal points in the data frame optimal

optimal <- as.data.frame(matrix(nrow=8,ncol=3))
names(optimal) <- c("key","x","y")
key <- c("t0.5","t1.0","t1.5","t2.0"
         ,"t2.5","t3.0","t3.5","t4.0")

data <- rdfD
for (i in 1:8)
{
  optimal[i,1] <- key[i]
  optimal[i,2] <- na.omit(data$k[which.max(data[,i+1])])
  optimal[i,3] <- max(data[,i+1])
}

# Now we add these points to the graph before exporting
plotD + geom_point(data=optimal
                   , aes(x=x
                         , y=y)
                   , colour="navyblue"
                   , size=6) 

dev.copy(png,'E4-01_etaD.png',width=2000,height=1330)
dev.off()

### ------------------PLOT END-------------------

# END
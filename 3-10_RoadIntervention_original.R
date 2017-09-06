# MINIMAL REPAIR - Road intervention
#------------------------------------------------------
###            INPUT      ........................
rm(list=ls())
Imin=15
alpha=0.15
Ipi=500
Ici=1000
T=20
K=30
mnfr<-array(dim=c(T,K)) #mean number of failure
tcnk<-array(dim=c(T,K))   	#total cost (item n, time t, interval k)
tcnke<-array(dim=c(T,K))  #total cost extended (item n, time t, interval k)
eta_part<-array(dim=c(T,K))   #cost per unit time (item n, time t, interval k)
A<-array(dim=c(T,K))
B<-array(dim=c(T,K))
C<-array(dim=c(T,K))
h<-matrix(nrow=T) #deterioration function
for (k in 1:K){
  for (t in 1:T){
    h[t]=t^2
    mnfr[t,k]<-exp((k-1)*alpha)
    if (k==1){
      tcnk[t,k]<-mnfr[t,k]
    } else {
      tcnk[t,k]<-tcnk[t,(k-1)]+mnfr[t,k]
    }
    
    A[t,k]=(Imin*h[t]*tcnk[t,k])/(k*t)
    B[t,k]=(k-1)*Ipi/(k*t)
    C[t,k]=Ici/(k*t)
    eta_part[t,k]<-A[t,k]+B[t,k]+C[t,k]
  }
}
results=data.frame(A[16,],B[16,],C[16,],eta_part[16,])
file.remove("results.csv")
file.create("results.csv")
write.table(results, file="results.csv", sep = ",", append = TRUE,col.names = FALSE)
plot.new()
par(mar=c(5, 4, 4, 6) + 0.3)
yunit=400
xunit=20
plot(eta_part[10,],type="b",lwd=2,col="blue",ylab="",xlab="",xlim=c(0,xunit),ylim=c(0,yunit),axes=FALSE,lty=1,pch=4) 
axis(2,c(seq(0,yunit,by=100)),c(seq(0,yunit,by=100)), las=1)
axis(1,c(seq(0,xunit,by=5)),c(seq(0,xunit,by=5)))
mtext(expression(paste("K")),side=1,col="black",line=2.2) 
mtext(expression(paste('cost  ',eta, '(',' mu)')),side=2,col="black",line=2.5)
points(which.min(eta_part[10,]),min(eta_part[10,]),pch=23,bg="black",lwd=3)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
par(new=TRUE)
plot(eta_part[6,],type="b",lwd=2,col="red",ylab="",xlab="",xlim=c(0,xunit),ylim=c(0,yunit),axes=FALSE,lty=1,pch=2) 
points(which.min(eta_part[6,]),min(eta_part[6,]),pch=23,bg="black",lwd=3)
par(new=TRUE)
plot(eta_part[16,],type="b",lwd=2,col="cyan4",ylab="",xlab="",xlim=c(0,xunit),ylim=c(0,yunit),axes=FALSE,lty=1,pch=1) 
points(which.min(eta_part[16,]),min(eta_part[16,]),pch=23,bg="black",lwd=3)
legend("topright",inset=.06,legend=c("T= 6,K*=6","T=10,K*=5","T=16,K*=4"),lty=c(1,1,1), lwd=c(2,2,2),col=c("red","blue","cyan4") ,cex=1,y.intersp=1,box.lwd='1',pch=c(2,4,1))
print(which.min(eta_part[6,]))
print(which.min(eta_part[10,]))
print(which.min(eta_part[16,]))

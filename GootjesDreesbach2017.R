rm(list = ls())

n<-84     # Number of samples for each experiment
m_b<-0   # Mean of blue distribution 
sd_b<-1   # SD of blue distribution
m_r<-0.5    # SD of red distribution
sd_r<-2   # Mean of red distribution

modelb2datab<-vector()
modelr2datab<-vector()
modelb2datar<-vector()
modelr2datar<-vector()
for (i in c(1:1000)){
  data_b<-rnorm(n,m_b,sd_b) # Draw n random samples of blue model 
  data_r<-rnorm(n,m_r,sd_r) # Draw n random samples of red model 
  
  modelb2datab[i]<-prod(dnorm(data_b,m_b,sd_b)) # Get total likelihood of blue data under blue model
  modelr2datab[i]<-prod(dnorm(data_b,m_r,sd_r)) # Get total likelihood of blue data under red model
  
  modelb2datar[i]<-prod(dnorm(data_r,m_b,sd_b)) # Get total likelihood of red data under blue model
  modelr2datar[i]<-prod(dnorm(data_r,m_r,sd_r)) # Get total likelihood of red data under red model
}

par(mfrow=c(1,2))
x<-seq(-100,100,length.out=1000)
plot(-20,-20,xlim=c(-10,10),ylim=c(0,0.5),xlab="x",ylab="P(x)",cex.axis=0.6, cex.lab = 0.8)
lines(x,dnorm(x,m_b,sd_b),col="blue")
lines(x,dnorm(x,m_r,sd_r),col="red")
legend("topleft", c("Blue Model M=0 SD=1","Red Model M=0.5 SD=2"), lty=c(1,1), col=c("blue","red"),cex=0.65,bty = "n")

# Plot blue data likelihood under the blue (x) and red (y) model 
plot(log(modelb2datab)/n,log(modelr2datab)/n,col='blue',xlim=c(-8,0),ylim=c(-8,0),xlab=c("log(t)/n : Blue Model"),ylab=c("log(t)/n : Red Model"),cex.axis=0.6, cex.lab = 0.8)
# Plot red data likelihood under the blue (x) and red (y) model
points(log(modelb2datar)/n,log(modelr2datar)/n,col='red')
points(-3,-2,pch = 19)
legend("bottomleft", c("Data from Blue Model","Data from Red Model","Hypothetical real dataset"), pch=c(1,1,19), col=c("blue","red","black"),cex=0.65,bty = "n")



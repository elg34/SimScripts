rm(list = ls())
library(stats4)

verghese<-function(mu2){
  mu1<-6
  sd1<-1
  sd2<-1
  numit<-8
  
  x<-seq(0,12,by=0.01)
  y_dist<-dnorm(x,mu1,sd1)
  y_targ<-dnorm(x,mu2,sd2)
  
  # plot(-1,-1,ylim=c(0,1),xlim=c(0,12),xlab="Magnitude of Response (Target Detector)",ylab="P(Response)",main="Response to distractor (blue) and target (red)")
  # lines(x,y_dist,col='blue')
  # lines(x,y_targ,col='red')

  sim<-100000
  d<-rnorm(sim,mu1,sd1)
  t<-rnorm(sim,mu2,sd2)
  p<-t-d
  p<-length(p[p>0])/sim
  text(11, 0.9, paste("p=",p))
  
  d<-vector(length = sim)
  t<-vector(length = sim)
  for (i in 1:sim){ 
    d[i]<-max(rnorm(numit,mu1,sd1)) # get numit samples from distractor distribution, take maximum
    t[i]<-max(c(rnorm(numit-1,mu1,sd1),rnorm(1,mu2,sd2))) # get numit-1 samples from distractor distribution and 1 from target distribution, take maximum
  }
  
  p<-t-d
  p<-length(p[p>0])/sim 
}





rm(list = ls())
library(model)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # Replicating Figure 1 in Verghese (2001) # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Luise, 25th Nov 2016
verghese<-function(sim,numit,mu1,mu2){
  # # # # # # # # # # # # PARAMETERS
  #numit<-32  # Number of items in the search display (Default: 8 or 32 for last two rows of fig. 1, respectively)
  #mu1<-6.5  # Mean target detector response to distractor (Default: 3.5 for left panels; 6.5 for right panels (in fig 1))
  sd1<-1    # SD of target detector responses to distractor (Default: 1)
  #mu2<-8.5  # Mean target detector response to target (Default: 8.5)
  sd2<-1    # SD of target detector responses to target (Default: 1)
  
  x<-seq(0,12,by=0.01)
  
  d<-vector(length = sim)
  t<-vector(length = sim)
  for (i in 1:sim){ 
    d[i]<-max(rnorm(numit,mu1,sd1)) # get numit samples from distractor distribution, take maximum
    t[i]<-max(c(rnorm(numit-1,mu1,sd1),rnorm(1,mu2,sd2))) # get numit-1 samples from distractor distribution and 1 from target distribution, take maximum
  }
  # In what proportion of trials do we correctly choose the display containing the target?
  p<-t-d
  p<-length(p[p>0])/sim 
  
}

n<-1000
pred<-mapply(verghese,100,8,2,3)



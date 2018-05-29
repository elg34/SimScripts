rm(list = ls())
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # Replicating Figure 1 in Verghese (2001) # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Luise, 25th Nov 2016
verghese<-function(samp,numit,mu1,mu2){
# # # # # # # # # # # # PARAMETERS
#numit<-32  # Number of items in the search display (Default: 8 or 32 for last two rows of fig. 1, respectively)
#mu1<-6.5  # Mean target detector response to distractor (Default: 3.5 for left panels; 6.5 for right panels (in fig 1))
sd1<-1    # SD of target detector responses to distractor (Default: 1)
#mu2<-8.5  # Mean target detector response to target (Default: 8.5)
sd2<-1    # SD of target detector responses to target (Default: 1)

# # # # # # # # # # # # Figure 1: Detector sensitivity
# Task:                 1st interval 1 target display
#                       2nd interval 1 distractor dislay
#                       Which was the interval with the target in it?
# Decision criterion:   Get response of target detector in each interval
#                       Report interval with higher response as containing the target
x<-seq(0,12,by=0.01)
y_dist<-dnorm(x,mu1,sd1)
y_targ<-dnorm(x,mu2,sd2)
plot(-1,-1,ylim=c(0,1),xlim=c(0,12),xlab="Magnitude of Response (Target Detector)",ylab="P(Response)",main="Response to distractor (blue) and target (red)")
lines(x,y_dist,col='blue')
lines(x,y_targ,col='red')

# In what proportion of trials do we correctly choose the display containing the target?
sim<-100000
d<-rnorm(sim,mu1,sd1)
t<-rnorm(sim,mu2,sd2)
p<-t-d
p<-length(p[p>0])/sim
text(11, 0.9, paste("p=",p))

if (samp==1){
  # # # # # # # # # # # # Figure 1: Monitoring several items per display
  # Task:                 1st interval NUMIT distractors in display, 
  #                       2nd interval the NUMIT-1 distractors + 1 target in display
  #                       - which was the interval with the target in it?
  # Decision criterion:   Get response of target detector for each item in each interval
  #                       For each interval get maximum response, interval with higher response is
  #                       reported to contain the target
  d<-vector(length = sim)
  t<-vector(length = sim)
  for (i in 1:sim){ 
    d[i]<-max(rnorm(numit,mu1,sd1)) # get numit samples from distractor distribution, take maximum
    t[i]<-max(c(rnorm(numit-1,mu1,sd1),rnorm(1,mu2,sd2))) # get numit-1 samples from distractor distribution and 1 from target distribution, take maximum
  }
  # In what proportion of trials do we correctly choose the display containing the target?
  p<-t-d
  p<-length(p[p>0])/sim 
  distPDF = density(d, from=0, to = 12, n = 1201) # PDF of detector response to distractors
  targPDF = density(t, from=0, to = 12, n = 1201) # PDF of detector response to targets
  
  plot(-1,-1,ylim=c(0,1),xlim=c(0,12),xlab="Magnitude of Response (Target Detector)",ylab="P(Response)",main=paste("Reponse to",numit,"distractors (blue) and",(numit-1),"+ 1 target (red)"))
  lines(x,distPDF$y,col='blue')
  lines(x,targPDF$y,col='red')
  text(11, 0.9, paste("p=",p))
}
}





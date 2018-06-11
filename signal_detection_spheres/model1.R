rm(list = ls())

library(psyphy)
library(ggplot2)
setwd("C:/Users/Luise/Desktop/verghese_fit/")

verghese_mod<-function(sig,n_targ,n_dist,sim){
  mu1<-0
  mu2<-sig # signal size (should be dprime of single target ident.)
  sd1<-1    # SD of target detector responses to distractor (Default: 1)
  sd2<-1    # SD of target detector responses to target (Default: 1)

  sign<-vector(length = round(sim/2))
  nosign<-vector(length = round(sim/2))
  for (i in 1:round(sim/2)){ 
    # 2 kinds of trials
    nosign[i]<-max(rnorm(n_dist+n_targ,mu1,sd1)) # get numit samples from distractor distribution, take maximum
    sign[i]<-if (n_dist==0){max(rnorm(n_targ,mu2,sd2))}else if (n_targ==0) {max(rnorm(n_dist,mu1,sd1))}else{max(c(rnorm(n_dist,mu1,sd1),rnorm(n_targ,mu2,sd2)))} # get numit-1 samples from distractor distribution and 1 from target distribution, take maximum
  }
  
  fp<-length(nosign[nosign>(mu2/2)])
  hit<-length(sign[sign>(mu2/2)])
  rej <-length(nosign[nosign<(mu2/2)])
  miss<-length(sign[sign<(mu2/2)])
  p<-(hit+rej)/sim
  list('p'=p,'hit'=hit/(sim/2),'fp'=fp/(sim/2),'rej'=rej/(sim/2),'miss'=miss/(sim/2))
}

numit<-8
#props<-seq(0,1,0.1)#c(1,7,8),#,8),
#targ=unique(mapply(function(x) round(x*numit), props))
targ=c(0,1,4,7,8)
dist=mapply(function(x) numit-x, targ)
label=mapply(function(x,y) paste("T:",x,"/","D:",y,sep=''), targ, dist)

sim<-100000
xvals<-seq(0,6,0.5)
pc<-NULL
hit<-NULL
fp<-NULL
for (i in xvals){
  pc<-c(pc,unlist(mapply(verghese_mod, rep(i,length(targ)), targ, dist, sim)[1,]))
  hit<-c(hit,unlist(mapply(verghese_mod, rep(i,length(targ)), targ, dist, sim)[2,]))
  fp<-c(fp,unlist(mapply(verghese_mod, rep(i,length(targ)), targ, dist, sim)[3,]))
}

#fp_dp<-fp
#fp_dp[fp_dp>=hit]<-hit[fp_dp>=hit]
#fp_dp<-apply(as.matrix(fp_dp),1,function(x) if(x==0){x+(1/sim)}else{x})
#dp<-mapply(function(a,b) dprime.ABX(a,b), hit, fp_dp)
df<-data.frame(
  x=sort(rep(xvals,length(targ))),
  pc=pc,
#  dp=dp,
  fp=fp,
  hit=hit,
  group=c(t(mapply(function(x) rep(x,length(xvals)),label)))
)


p1<-ggplot(df, aes(x = x, y = pc, color=group, linetype=group)) +
  geom_point(size=3) + geom_line() +
  #xlim(0, 6) + ylim(0.5, 1) +
  #ggtitle("Performance in Exp given ") +
  xlab("Underlying Signal") + ylab("Pred. Performance") +
  ggtitle("Model 1") + 
  theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))
p1

# sometimes odd values for hit rate!
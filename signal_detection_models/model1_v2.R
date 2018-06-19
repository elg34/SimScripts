rm(list = ls())

library(psyphy)
library(ggplot2)
setwd("C:/Users/Luise/Desktop/verghese_fit/")

# given responses on signal/no-signal trials, what are the fp and hit rates for all criterion values
get_roc<-function(sign,nosign){
  cs<-sort(c(sign,nosign))
  fp<-vector(length = length(cs))
  hit<-vector(length = length(cs))
  count<-1
  for (c in cs){
    fp[count]<-length(nosign[nosign>c])/(sim/2)
    hit[count]<-length(sign[sign>c])/(sim/2)
    count<-count+1
  }
  AUC <- abs(sum(diff(d$fp)*rollmean(d$hit,2)))
  dp <- qnorm(AUC)*sqrt(2)
  list('fp'=fp,'hit'=hit,'AUC'=AUC,'dp'=dp)
}

verghese_mod<-function(sig,n_targ,n_dist,sim, t_type = FALSE){
  mu1<-0
  mu2<-sig # signal size (should be dprime of single target ident.)
  sd1<-1    # SD of target detector responses to distractor (Default: 1)
  sd2<-1    # SD of target detector responses to target (Default: 1)
  
  sign<-vector(length = round(sim/2))
  nosign<-vector(length = round(sim/2))
  for (i in 1:round(sim/2)){ 
    # 2 kinds of trials
    nosign[i]<-max(rnorm(n_dist+n_targ,mu1,sd1)) # get numit samples from distractor distribution, take maximum
    sign[i]<-if (n_dist==0){max(rnorm(n_targ,mu2,sd2))}else if (n_targ==0) {max(rnorm(n_dist,mu1,sd1))}else if (t_type==TRUE){max(c(rnorm(n_dist,mu2,sd2),rnorm(n_targ,sig^2,sd2)))}else{max(c(rnorm(n_dist,mu1,sd1),rnorm(n_targ,mu2,sd2)))} # get numit-1 samples from distractor distribution and 1 from target distribution, take maximum
  }
  
  get_roc(sign,nosign)
}

numit<-8
#props<-seq(0,1,0.1)#c(1,7,8),#,8),
#targ<-unique(mapply(function(x) round(x*numit), props))
#dist<-mapply(function(x) numit-x, targ)
targ<-c(1,7,8,1)
dist<-c(7,1,0,7)
t_type<-c(FALSE,FALSE,FALSE,TRUE)
label<-mapply(function(x,y,z) paste("T:",x,"/","D:",y,'/T:B^2=',z,sep=''), targ, dist, t_type)

sim<-10000
xvals<-seq(0,6,0.5)
pc<-NULL
hit<-NULL
fp<-NULL
for (i in xvals){
  pc<-c(pc,unlist(mapply(verghese_mod, rep(i,length(targ)), targ, dist, sim,t_type)[1,]))
  hit<-c(hit,unlist(mapply(verghese_mod, rep(i,length(targ)), targ, dist, sim,t_type)[2,]))
  fp<-c(fp,unlist(mapply(verghese_mod, rep(i,length(targ)), targ, dist, sim,t_type)[3,]))
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
  xlim(0, 6) + ylim(0.45, 1) +
  xlab("Underlying Signal") + ylab("Pred. Performance") +
  ggtitle("Model 1: c halfway between sampled means") + 
  theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))
p1

# sometimes odd values for hit rate!
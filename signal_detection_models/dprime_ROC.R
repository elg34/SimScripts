rm(list = ls())
library(ggplot2)
library(zoo)

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
  list('fp'=fp,'hit'=hit)
}

#
plot_roc<-function(sig,n_targ,n_dist,sim,t_type){
  mu1<-0
  mu2<-sig  # signal size (should be dprime of single target ident.)
  sd1<-1    # SD of target detector responses to distractor (Default: 1)
  sd2<-1    # SD of target detector responses to target (Default: 1)
  
  sign<-vector(length = round(sim/2))
  nosign<-vector(length = round(sim/2))
  for (i in 1:round(sim/2)){ 
    nosign[i]<-max(rnorm(n_dist+n_targ,mu1,sd1)) # get numit samples from distractor distribution, take maximum
    sign[i]<-if (n_dist==0){max(rnorm(n_targ,mu2,sd2))}else if (n_targ==0) {max(rnorm(n_dist,mu1,sd1))}else if (t_type==TRUE){max(c(rnorm(n_dist,mu2,sd2),rnorm(n_targ,sig^2,sd2)))}else{max(c(rnorm(n_dist,mu1,sd1),rnorm(n_targ,mu2,sd2)))} # get numit-1 samples from distractor distribution and 1 from target distribution, take maximum
  }
  
  d<-get_roc(sign,nosign)
  AUC1 <- abs(sum(diff(d$fp)*rollmean(d$hit,2)))
  dp <- qnorm(AUC1)*sqrt(2)
  d2<-get_roc(rnorm(round(sim/2),dp,1),rnorm(round(sim/2),0,1))
  AUC2 <- abs(sum(diff(d2$fp)*rollmean(d2$hit,2)))
  dp2 <- qnorm(AUC2)*sqrt(2)
  df<-data.frame(
    hit=c(d$hit,d2$hit),
    fp=c(d$fp,d2$fp),
    group_AUC_dp=c(rep(paste('Model',signif(AUC1,digits=2),signif(dp,digits=2),sep='_'),length(d$fp)),rep(paste('Norm',signif(AUC2,digits=2),signif(dp2,digits=2),sep='_'),length(d2$fp)))
  )
  
  p1<-ggplot(df, aes(x = fp, y = hit, color=group_AUC_dp, linetype=group_AUC_dp)) +
    geom_point(size=1) + #geom_line() +
    xlab("FP") + ylab("Hit") +
    ggtitle(paste('ROC\ndprime=',signif(dp,digits=2),' / sig.=',sig,'\nCond=T',n_targ,'/D',n_dist,'/D^2',t_type,sep='')) + 
    theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))
  p1
}

sig<-0.01
n_targ<-7
n_dist<-1
sim<-10000
t_type<-FALSE
plot_roc(sig,n_targ,n_dist,sim,t_type)

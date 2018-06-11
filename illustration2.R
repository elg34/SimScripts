rm(list = ls())

library(ggplot2)

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
  list('sign'=sign,'nosign'=nosign)
}

sig<-2.5
sim<-10000
t1d7<-verghese_mod(sig,1,7,sim)
t7d1<-verghese_mod(sig,7,1,sim)

df<-data.frame(
  dv=c(t1d7$sign,t1d7$nosign,t7d1$sign,t7d1$nosign),
  s=rep(c(rep('Signal',sim/2),rep('NoSignal',sim/2)),2),
  c=c(rep('T:1/D:7',sim),rep('T:7/D:1',sim))
)


ggplot(df, aes(x=dv, fill=s)) + geom_density(alpha=.3) + geom_vline(xintercept = sig/2) + facet_wrap(~ c)

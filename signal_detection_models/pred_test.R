rm(list = ls())

library(psyphy)
library(ggplot2)
library(stats)
library(zoo)

verghese_mod2<-function(sig_gl,sig_rel,n_targ,n_dist,sim, t_type = FALSE,opt=FALSE){
  numit<-n_dist+n_targ
  sign<-vector(length = round(sim/2))
  nosign<-vector(length = round(sim/2))
  for (i in 1:round(sim/2)){ 
    nosign_gl<-rnorm(numit,0,1)
    nosign_rel<-rnorm(numit,0,1)
    sign_gl<-if (n_dist==0){rnorm(n_targ,sig_gl,1)}else if (n_targ==0) {rnorm(n_dist,0,1)}else if (t_type==TRUE){c(rnorm(n_dist,sig_gl,1),rnorm(n_targ,sig_gl^2,1))}else{c(rnorm(n_dist,0,1),rnorm(n_targ,sig_gl,1))}
    sign_rel<-if (n_dist==0){rnorm(numit,0,1)}else{c(rnorm(numit-1,0,1),rnorm(1,sig_rel,1))}
    
    nosign[i]<-max(c(nosign_gl,nosign_rel))
    sign[i]<-max(c(sign_gl,sign_rel))
  }
  
  cs<-sort(c(sign,nosign))
  fp<-mapply(function(c,sim) length(nosign[nosign>c])/(sim/2),cs,sim)
  hit<-mapply(function(c,sim) length(sign[sign>c])/(sim/2),cs,sim)
  AUC <- abs(sum(diff(fp)*rollmean(hit,2)))
  dp <- qnorm(AUC)*sqrt(2)
  if (opt==FALSE){
    dp
  }else{
    abs(dp-opt)
  }
}

verghese_mod<-function(sig,n_targ,n_dist,sim, t_type = FALSE,opt=FALSE){
  numit<-n_dist+n_targ
  sign<-vector(length = round(sim/2))
  nosign<-vector(length = round(sim/2))
  for (i in 1:round(sim/2)){ 
    nosign[i]<-max(rnorm(n_dist+n_targ,0,1)) # get numit samples from distractor distribution, take maximum
    sign[i]<-if (n_dist==0){max(rnorm(n_targ,sig,1))}else if (n_targ==0) {max(rnorm(n_dist,0,1))}else if (t_type==TRUE){max(c(rnorm(n_dist,sig,1),rnorm(n_targ,sig^2,1)))}else{max(c(rnorm(n_dist,0,1),rnorm(n_targ,sig,1)))}
  }
  cs<-sort(c(sign,nosign))
  fp<-mapply(function(c,sim) length(nosign[nosign>c])/(sim/2),cs,sim)
  hit<-mapply(function(c,sim) length(sign[sign>c])/(sim/2),cs,sim)
  AUC <- abs(sum(diff(fp)*rollmean(hit,2)))
  dp <- qnorm(AUC)*sqrt(2)
  if (opt==FALSE){
    dp
  }else{
    abs(dp-opt)
  }
}

c1B1<- 0.5460
cBB1<- 0.9761
sim<-10000

opt_c1B1<-optimize(verghese_mod, interval=c(0,5),n_targ=1, n_dist=7, sim=sim, opt=c1B1)
pBB1<-verghese_mod(sig=opt_c1B1$minimum,n_targ=8, n_dist=0, sim=sim)
print(paste('Model1 Based on 1B1 :: Actual BB1:',cBB1,'Pred BB1:',pBB1))

opt_cBB1<-optimize(verghese_mod, interval=c(0,5),n_targ=8, n_dist=0,sim=sim, opt=cBB1)
p1B1<-verghese_mod(sig=opt_cBB1$minimum,n_targ=1, n_dist=7,sim=sim)
print(paste('Model1 Based on BB1 :: Actual 1B1:',c1B1,'Pred 1B1:',p1B1))

opt_c1B1<-optimize(verghese_mod2, interval=c(0,5),n_targ=1, n_dist=7, sim=sim, opt=c1B1)
pBB1<-verghese_mod(sig=opt_c1B1$minimum,n_targ=8, n_dist=0, sim=sim)
print(paste('Model2 Based on 1B1 :: Actual BB1:',cBB1,'Pred BB1:',pBB1))

opt_cBB1<-optimize(verghese_mod2, interval=c(0,5),n_targ=8, n_dist=0,sim=sim, opt=cBB1)
p1B1<-verghese_mod(sig=opt_cBB1$minimum,n_targ=1, n_dist=7,sim=sim)
print(paste('Model2 Based on BB1 :: Actual 1B1:',c1B1,'Pred 1B1:',p1B1))
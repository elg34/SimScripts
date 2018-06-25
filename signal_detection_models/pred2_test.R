rm(list = ls())

library(psyphy)
library(ggplot2)
library(stats)
library(zoo)

get_roc<-function(sign,nosign,sim){
  cs<-sort(c(sign,nosign))
  fp<-vector(length = length(cs))
  hit<-vector(length = length(cs))
  rej<-vector(length = length(cs))
  miss<-vector(length = length(cs))
  count<-1
  for (c in cs){
    fp[count]<-length(nosign[nosign>c])/(sim/2)
    hit[count]<-length(sign[sign>c])/(sim/2)
    rej[count] <-length(nosign[nosign<c])/(sim/2)
    miss[count]<-length(sign[sign<c])/(sim/2)
    count<-count+1
  }
  AUC <- abs(sum(diff(fp)*rollmean(hit,2)))
  dp <- qnorm(AUC)*sqrt(2)
  list('AUC'=AUC,'dp'=dp)
}

verghese_mod2_opt<-function(sig,n_targ,n_dist,sim, t_type = FALSE,t){
  mu1<-0
  mu2<-sig # signal size (should be dprime of single target ident.)
  sd1<-1    # SD of target detector responses to distractor (Default: 1)
  sd2<-1    # SD of target detector responses to target (Default: 1)
  
  sign<-vector(length = round(sim/2))
  nosign<-vector(length = round(sim/2))
  for (i in 1:round(sim/2)){ 
    # 2 kinds of trials
    nosign_gl<-rnorm(n_dist+n_targ,mu1,sd1)
    sign_gl<-if (n_dist==0){rnorm(n_targ,mu2,sd2)}else if (n_targ==0) {rnorm(n_dist,mu1,sd1)}else if (t_type==TRUE){c(rnorm(n_dist,mu2,sd2),rnorm(n_targ,sig^2,sd2))}else{c(rnorm(n_dist,mu1,sd1),rnorm(n_targ,mu2,sd2))}
    nosign_rel<-max(rnorm(n_dist+n_targ,mu1,sd1))
    sign_rel<-if (n_dist==0){rnorm(n_dist+n_targ,mu1,sd1)}else if (t_type==TRUE){c(rnorm(7,mu2,sd2),rnorm(1,mu2^2,sd2))}else{c(rnorm(7,mu1,sd1),rnorm(1,mu2,sd2))}
    
    
    nosign[i]<-max(c(nosign_gl,nosign_rel))
    sign[i]<-max(c(sign_gl,sign_rel))
  }
  
  abs(get_roc(sign,nosign,sim)$dp-t)
}

verghese_mod2_opt<-function(sig,n_targ,n_dist,sim, t_type = FALSE){
  mu1<-0
  mu2<-sig # signal size (should be dprime of single target ident.)
  sd1<-1    # SD of target detector responses to distractor (Default: 1)
  sd2<-1    # SD of target detector responses to target (Default: 1)
  
  sign<-vector(length = round(sim/2))
  nosign<-vector(length = round(sim/2))
  for (i in 1:round(sim/2)){ 
    # 2 kinds of trials
    nosign_gl<-rnorm(n_dist+n_targ,mu1,sd1)
    sign_gl<-if (n_dist==0){rnorm(n_targ,mu2,sd2)}else if (n_targ==0) {rnorm(n_dist,mu1,sd1)}else if (t_type==TRUE){c(rnorm(n_dist,mu2,sd2),rnorm(n_targ,sig^2,sd2))}else{c(rnorm(n_dist,mu1,sd1),rnorm(n_targ,mu2,sd2))}
    nosign_rel<-max(rnorm(n_dist+n_targ,mu1,sd1))
    sign_rel<-if (n_dist==0){rnorm(n_dist+n_targ,mu1,sd1)}else if (t_type==TRUE){c(rnorm(7,mu2,sd2),rnorm(1,mu2^2,sd2))}else{c(rnorm(7,mu1,sd1),rnorm(1,mu2,sd2))}
    
    
    nosign[i]<-max(c(nosign_gl,nosign_rel))
    sign[i]<-max(c(sign_gl,sign_rel))
  }
  
  get_roc(sign,nosign,sim)
}


c1B1<- 0.5460
cBB1<- 0.9761

opt_c1B1<-optimize(verghese_mod2_opt, interval=c(0,5),n_targ=1, n_dist=7, sim=10000, t=c1B1)
pBB1<-verghese_mod2(sig=opt_c1B1$minimum,n_targ=8, n_dist=0, sim=10000)
print(paste('Based on 1B1 :: Actual BB1:',cBB1,'Pred BB1:',pBB1$dp))

opt_cBB1<-optimize(verghese_mod2_opt, interval=c(0,5),n_targ=8, n_dist=0, sim=10000, t=cBB1)
p1B1<-verghese_mod2(sig=opt_cBB1$minimum,n_targ=1, n_dist=7, sim=10000)
print(paste('Based on BB1 :: Actual 1B1:',c1B1,'Pred 1B1:',p1B1$dp))
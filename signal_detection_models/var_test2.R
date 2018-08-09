rm(list = ls())

library(zoo)

get_dp<-function(sign,nosign){
  cs<-sort(c(sign,nosign))
  sim<-length(cs)
  fp<-mapply(function(c,sim) length(nosign[nosign>c])/(sim/2),cs,sim)
  hit<-mapply(function(c,sim) length(sign[sign>c])/(sim/2),cs,sim)
  AUC <- abs(sum(diff(fp)*rollmean(hit,2)))
  
  qnorm(AUC)*sqrt(2)
}

full_model<-function(sig_gl,sig_rel,n_targ,n_dist,sim, t_type = FALSE,opt=FALSE){
  numit<-n_dist+n_targ
  
  nosign_gl<-matrix(rnorm((sim/2)*numit,0,1), nrow = sim/2,ncol=numit, byrow = TRUE)
  nosign_rel<-matrix(rnorm((sim/2)*numit,0,1), nrow = sim/2,ncol=numit, byrow = TRUE)
  nosign<-apply(cbind(nosign_gl,nosign_rel),1,max)
  
  if (n_dist==0){
    sign_rel<-matrix(rnorm((sim/2)*numit,0,1), nrow = sim/2,ncol=numit, byrow = TRUE)
  }else{
    sign_rel<-cbind(matrix(rnorm((sim/2)*(numit-1),0,1), nrow = sim/2,ncol=numit-1, byrow = TRUE),
                    matrix(rnorm((sim/2),sig_rel,1), nrow = sim/2,ncol=1, byrow = TRUE))
  }
  if (t_type==TRUE){
    sign_gl<-cbind(matrix(rnorm((sim/2)*n_dist,sig_gl,1), nrow = sim/2,ncol=n_dist, byrow = TRUE),
                   matrix(rnorm((sim/2)*n_targ,sig_gl^2,1), nrow = sim/2,ncol=n_targ, byrow = TRUE))
  }else{
    sign_gl<-cbind(matrix(rnorm((sim/2)*n_dist,0,1), nrow = sim/2,ncol=n_dist, byrow = TRUE),
                   matrix(rnorm((sim/2)*n_targ,sig_gl,1), nrow = sim/2,ncol=n_targ, byrow = TRUE))
  }
  sign<-apply(cbind(sign_gl,sign_rel),1,max)
  dp<-get_dp(sign,nosign)
  
  if (opt==FALSE){
    dp
  }else{
    abs(dp-opt)
  }
}

run<-100
sim<-10000
pred1B<-mapply(full_model,sig_gl=rep(1.6,run),sig_rel=rep(1.6,run),n_targ=1,n_dist=7,sim=sim)
hist(pred1B)
mean(pred1B)
sd(pred1B)

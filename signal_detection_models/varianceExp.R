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

verghese2_opt<-function(i){
  sim<-1000
  targ<-c(1,7,8,1)
  dist<-c(7,1,0,7)
  t_type<-c(FALSE,FALSE,FALSE,TRUE)
  opt<-c(0.5460,1.1775,0.9761,0.8691)
  res<-mapply(verghese_mod2,sig_gl=rep(i[1],length(targ)),sig_rel=rep(i[2],length(targ)),n_targ=targ,n_dist=dist,sim=rep(sim,length(targ)),t_type=t_type,opt=opt)
  sqrt(mean(res^2))
}

err<-NULL
for (i in 1:10){
  err<-c(err,verghese2_opt(c(1,2)))
}
# 10000 trials
#> mean(err)
#[1] 0.5469264
#> sd(err)
#[1] 0.01193355
# 1000 trials
#>  mean(err)
#[1] 0.5403271
#> sd(err)
#[1] 0.03644806
# 100 trials
#>  mean(err)
#[1] 0.3719539
#> sd(err)
#[1] 0.0662245

par_gl<-NULL
par_rel<-NULL
for (i in 1:10){
  o<-optim(c(1,1),verghese2_opt,method="L-BFGS-B",lower = 0, upper = 5)
  par_gl<-c(par_gl,o$par[1])
  par_rel<-c(par_rel,o$par[2])
}

# 10000 trials
# can't even be contemplated
# 1000 trials
#> mean(par_gl)
#[1] 0.9097794
#> sd(par_gl)
#[1] 0.1363362
#> mean(par_rel)
#[1] 1.236852
#> sd(par_rel)
#[1] 0.3055021
# 100 trials
#> mean(par_gl)
#[1] 0.9224853
#> sd(par_gl)
#[1] 0.115423
#> mean(par_rel)
#[1] 1.168722
#> sd(par_rel)
#[1] 0.3149511

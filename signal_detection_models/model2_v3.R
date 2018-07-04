rm(list = ls())

library(psyphy)
library(ggplot2)
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
  dp <- if (opt==FALSE){
    dp
  }else{
    abs(dp-opt)
  }
}

targ<-c(1,7,8,1)
dist<-c(7,1,0,7)
opt_lui<-c(0.5460,1.1775,0.9761,0.8691)
t_type<-c(FALSE,FALSE,FALSE,TRUE)
label<-mapply(function(x,y,z) paste("T:",x,"/","D:",y,'/T:B^2=',z,sep=''), targ, dist, t_type)

sim<-1000
xvals<-seq(0,6,0.5)
dp<-NULL
for (i in xvals){
  dp<-c(dp,mapply(verghese_mod2, rep(i,length(targ)),rep(i,length(targ)), targ, dist, sim,t_type))
}

df<-data.frame(
  x=sort(rep(xvals,length(targ))),
  dp=dp,
  group=c(t(mapply(function(x) rep(x,length(xvals)),label)))
)


p1<-ggplot(df, aes(x = x, y = dp, color=group, linetype=group)) +
  geom_point(size=3) + geom_line() +
  xlim(0, 6) + #ylim(0.45, 1) +
  xlab("Underlying Signal") + ylab("Pred. Performance") +
  ggtitle("Model 2:") + 
  theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))
p1

# sometimes odd values for hits?
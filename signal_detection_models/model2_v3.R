rm(list = ls())

library(psyphy)
library(ggplot2)
library(zoo)

verghese_mod2<-function(sig,n_targ,n_dist,sim, t_type = FALSE){
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
    sign_rel<-if (n_dist==0){rnorm(n_dist+n_targ,mu1,sd1)}else{c(rnorm(7,mu1,sd1),rnorm(1,mu2,sd2))}
    
    nosign[i]<-max(c(nosign_gl,nosign_rel))
    sign[i]<-max(c(sign_gl,sign_rel))
  }
  
  cs<-sort(c(sign,nosign))
  fp<-mapply(function(c,sim) length(nosign[nosign>c])/(sim/2),cs,sim)
  hit<-mapply(function(c,sim) length(sign[sign>c])/(sim/2),cs,sim)
  AUC <- abs(sum(diff(fp)*rollmean(hit,2)))
  dp <- qnorm(AUC)*sqrt(2)
  list('AUC'=AUC,'dp'=dp)
}

targ<-c(1,7,8,1)
dist<-c(7,1,0,7)
t_type<-c(FALSE,FALSE,FALSE,TRUE)
label<-mapply(function(x,y,z) paste("T:",x,"/","D:",y,'/T:B^2=',z,sep=''), targ, dist, t_type)

sim<-1000
xvals<-seq(0,6,0.5)
AUC<-NULL
dp<-NULL
for (i in xvals){ # 'fp'=fp,'hit'=hit,'rej'=rej,'miss'=miss,'AUC'=AUC,'dp'=dp
  dat<-mapply(verghese_mod2, rep(i,length(targ)), targ, dist, sim,t_type)
  AUC<-c(AUC,unlist(dat[1,]))
  dp<-c(dp,unlist(dat[2,]))
}

df<-data.frame(
  x=sort(rep(xvals,length(targ))),
  AUC=AUC,
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
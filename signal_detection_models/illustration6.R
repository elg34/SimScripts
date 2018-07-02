rm(list = ls())

library(psyphy)
library(ggplot2)
library(zoo)

c1B1<- 0.5460
cBB1<- 0.9761

diff_true<-cBB1-c1B1


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

targ<-c(1,8)
dist<-c(7,0)
t_type<-c(FALSE,FALSE)
label<-mapply(function(x,y,z) paste("T:",x,"/","D:",y,'/T:B^2=',z,sep=''), targ, dist, t_type)

sim<-1000
xvals<-seq(0,6,0.5)
dp<-NULL
for (i in xvals){ # 'fp'=fp,'hit'=hit,'rej'=rej,'miss'=miss,'AUC'=AUC,'dp'=dp
  dp<-c(dp,mapply(verghese_mod, rep(i,length(targ)), targ, dist, sim,t_type))
}

df<-data.frame(
  x=sort(rep(xvals,length(targ))),
  dp=dp,
  group=c(t(mapply(function(x) rep(x,length(xvals)),label)))
)

p1B1<-df$dp[df$group=='T:1/D:7/T:B^2=FALSE']
pBB1<-df$dp[df$group=='T:8/D:0/T:B^2=FALSE']
diff_pred<-pBB1-p1B1

df<-data.frame(
  x=xvals,
  diff=diff_pred
)

p1<-ggplot(df, aes(x = x, y = diff)) +
  geom_point(size=3) + geom_line() +
  xlim(0, 6) + #ylim(0.45, 1) +
  xlab("Underlying Signal") + ylab("BB1-1B1") +
  ggtitle("Model 1:") + 
  theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_hline(yintercept=diff_true)
p1

# sometimes odd values for hit rate?
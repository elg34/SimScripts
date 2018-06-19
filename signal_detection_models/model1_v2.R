rm(list = ls())

library(psyphy)
library(ggplot2)

# given responses on signal/no-signal trials, what are the fp and hit rates for all criterion values
get_roc<-function(sign,nosign){
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

get_pc<-function(sign,nosign,c=(mean(nosign)+mean(sign))/2){
  fp<-length(nosign[nosign>c])
  hit<-length(sign[sign>c])
  rej <-length(nosign[nosign<c])
  miss<-length(sign[sign<c])
  p<-(hit+rej)/sim
  list('p'=p,'hit'=hit/(sim/2),'fp'=fp/(sim/2),'rej'=rej/(sim/2),'miss'=miss/(sim/2))
}

verghese_mod<-function(sig,n_targ,n_dist,sim, t_type = FALSE){
  mu1<-0
  mu2<-sig # signal size (should be dprime of single target ident.)
  sd<-1
  sign<-vector(length = round(sim/2))
  nosign<-vector(length = round(sim/2))
  for (i in 1:round(sim/2)){ 
    # 2 kinds of trials
    nosign[i]<-max(rnorm(n_dist+n_targ,mu1,sd)) # get numit samples from distractor distribution, take maximum
    sign[i]<-if (n_dist==0){max(rnorm(n_targ,mu2,sd))}else if (n_targ==0) {max(rnorm(n_dist,mu1,sd))}else if (t_type==TRUE){max(c(rnorm(n_dist,mu2,sd),rnorm(n_targ,sig^2,sd)))}else{max(c(rnorm(n_dist,mu1,sd),rnorm(n_targ,mu2,sd)))} # get numit-1 samples from distractor distribution and 1 from target distribution, take maximum
  }
  
  get_roc(sign,nosign)
  #get_pc(sign,nosign)
}

#numit<-8
#props<-seq(0,1,0.1)#c(1,7,8),#,8),
#targ<-unique(mapply(function(x) round(x*numit), props))
#dist<-mapply(function(x) numit-x, targ)
targ<-c(1,7,8,1)
dist<-c(7,1,0,7)
t_type<-c(FALSE,FALSE,FALSE,TRUE)
label<-mapply(function(x,y,z) paste("T:",x,"/","D:",y,'/T:B^2=',z,sep=''), targ, dist, t_type)

sim<-10000
xvals<-seq(0,6,0.5)
AUC<-NULL
dp<-NULL
for (i in xvals){ # 'fp'=fp,'hit'=hit,'rej'=rej,'miss'=miss,'AUC'=AUC,'dp'=dp
  dat<-mapply(verghese_mod, rep(i,length(targ)), targ, dist, sim,t_type)
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
  ggtitle("Model 1:") + 
  theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))
p1

# sometimes odd values for hit rate?
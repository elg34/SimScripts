rm(list = ls())

library(psyphy)
library(ggplot2)
setwd("C:/Users/Luise/Desktop/verghese_fit/")

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
    sign[i]<-max(c(rnorm(n_dist,mu1,sd1),rnorm(n_targ,mu2,sd2))) # get numit-1 samples from distractor distribution and 1 from target distribution, take maximum
  }
  fp<-length(nosign[nosign>(mu2/2)])
  hit<-length(sign[sign>(mu2/2)])
  rej <-length(nosign[nosign<(mu2/2)])
  miss<-length(sign[sign<(mu2/2)])
  p<-(hit+rej)/sim
  list('p'=p,'hit'=hit/sim,'fp'=fp/sim,'rej'=rej/sim,'miss'=miss/sim)
}

n_targ<-1
n_dist<-7
numit<-n_dist+n_targ
sim<-10000
sigs<-seq(0,6,by=0.1)
pc<-vector(length=length(sigs))
hit<-vector(length=length(sigs))
fp<-vector(length=length(sigs))
rej<-vector(length=length(sigs))
miss<-vector(length=length(sigs))
count<-1
for (s in sigs){
  verg<-verghese_mod(s,n_targ,n_dist,sim)
  pc[count]<-verg$p
  hit[count]<-verg$hit
  fp[count]<-verg$fp
  rej[count]<-verg$rej
  miss[count]<-verg$miss
  count<-count+1
}

# put a tiny error rate in if people are at ceiling/floor
fp_dp<-fp
fp_dp[fp_dp>=hit]<-hit[fp_dp>=hit]
fp_dp<-apply(as.matrix(fp_dp),1,function(x) if(x==0){x+(1/sim)}else{x})
df<-data.frame(
  sigs=sigs,
  pc=pc,
  hit=hit,
  fp=fp,
  fp_dp=fp_dp,
  rej=rej,
  miss=miss
)
dp<-mapply(function(a,b) dprime.ABX(a,b), df$hit, df$fp_dp)

p1<-ggplot(df, aes(x = sigs, y = pc)) +
  geom_point(size=3) + geom_line(color="red") +
  xlim(0, 6) + ylim(0.49, 1) +
  xlab("1 item d'") + ylab(paste(numit,"item PC")) +
  ggtitle(paste("Verghese: 2IFC",numit,"items")) +
  theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))
#ggsave(paste("pc_",numit,"items.png",sep=''), plot = p1)
p1

p2<-ggplot(df, aes(x = sigs, y = dp)) +
  geom_point(size=3) + geom_line(color="red") +
  xlim(0, 6) + ylim(0, 6) +
  xlab("1 item d'") + ylab(paste(numit,"item d'")) +
  ggtitle(paste("Verghese: yes/no task",numit,"items")) + 
  theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))
#ggsave(paste("dp_",numit,"items.png",sep=''), plot = p2)
p2
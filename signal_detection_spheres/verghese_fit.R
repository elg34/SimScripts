rm(list = ls())

library(psyphy)
library(ggplot2)
setwd("C:/Users/Luise/Documents/Github/verghese_fit/")

verghese<-function(sig,numit,sim){
  mu1<-0
  mu2<-sig # signal size (should be dprime of single target ident.)
  sd1<-1    # SD of target detector responses to distractor (Default: 1)
  sd2<-1    # SD of target detector responses to target (Default: 1)
  
  d<-vector(length = sim)
  t<-vector(length = sim)
  for (i in 1:sim){ 
    # 2IFC, one with just distractors, one with target and distractors
    d[i]<-max(rnorm(numit,mu1,sd1)) # get numit samples from distractor distribution, take maximum
    t[i]<-max(c(rnorm(numit-1,mu1,sd1),rnorm(1,mu2,sd2))) # get numit-1 samples from distractor distribution and 1 from target distribution, take maximum
  }
  # In what proportion of trials do we correctly choose the display containing the target?
  p<-t-d
  p<-length(p[p>0])/sim 
  p
}

numit<-8
sim<-100000
sigs<-seq(0,6,by=0.1)
pc<-vector(length=length(sigs))
count<-1
for (s in sigs){
  pc[count]<-verghese(s,numit,sim)
  count<-count+1
}

# put a tiny error rate in if people are at ceiling/floor
pc_dpprep<-apply(as.matrix(pc),1,function(x) if(x==1){x-(1/sim)}else if(x==0){x+(1/sim)}else{x})
df<-data.frame(
  sigs=sigs,
  pc=pc,
  dp=apply(as.matrix(pc_dpprep),1,function(x) dprime.mAFC(x,2))
)

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
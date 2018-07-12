rm(list=ls())
library(ggplot2)
########################################################
# Illustration of what can happen when dependent samples are wrongly used in an independent test
########################################################

sims<-1000 # number of experiments
ppt<-10 # number of participants
trials<-100 # number of trials

p_bad <- vector(length=sims)
p <- vector(length=sims)
for (i in 1:sims){
  pptmeans<-rnorm(ppt,5,1) # the smaller the variance in means, the smaller the type 1 inflation
  pptsds<-rep(1,ppt) #rnorm(ppt,2,0.5)
  pptsds[pptsds<0]=0
  
  df <- data.frame(
    idx = factor(1:(ppt*trials)),
    ppt = factor(sort(rep(1:ppt,trials))),
    iv = factor(c(rep(1,(ppt*trials)/2),rep(2,(ppt*trials)/2))),
    dv_null = c(mapply(function(x,y) rnorm(trials,x,y),pptmeans,pptsds))
  )
  df$dv_sig = c(df$dv_null[df$iv==1],df$dv_null[df$iv==2]+2)
  
  # distributions in the two groups
  # ggplot(df, aes(x=dv_null, colour=iv)) + geom_density() + ggtitle('Null')
  # ggplot(df, aes(x=dv_sig, colour=iv)) + geom_density() + ggtitle('Alt')
  
  t_bad<-t.test(df$dv_null[df$iv==1],df$dv_null[df$iv==2]) # note, we are probably also violating eq. of vars and normality for both tests!
  p_bad[i]<-t_bad$p.value
  
  agg<-aggregate(. ~ ppt, df, mean)
  t<-t.test(agg$dv_null[agg$iv==1],agg$dv_null[agg$iv==2])
  p[i]<-t$p.value
  
}

res<-data.frame(
  x = c(p_bad,p),
  g = factor(c(rep("trials",length(p_bad)),rep("ppt",length(p))))
)

ggplot(res, aes(x=x, fill=g)) + geom_histogram(alpha=.5, position="identity")
rm(list = ls())

library(ggplot2)

sig<-2.5
n_targ<-1
n_dist<-7
sim<-10000

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
list('p'=p,'hit'=hit/(sim/2),'fp'=fp/(sim/2),'rej'=rej/(sim/2),'miss'=miss/(sim/2))

df<-data.frame(
  dv=c(sign,nosign),
  iv=c(rep('Signal',length(sign)),rep('NoSignal',length(nosign)))
)


ggplot(df, aes(x=dv, fill=iv)) + geom_density(alpha=.3) + geom_vline(xintercept = sig/2)

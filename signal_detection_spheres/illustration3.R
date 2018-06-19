verghese_mod<-function(sig,n_targ,n_dist,sim, t_type = FALSE){
  mu1<-0
  mu2<-sig # signal size (should be dprime of single target ident.)
  sd1<-1    # SD of target detector responses to distractor (Default: 1)
  sd2<-1    # SD of target detector responses to target (Default: 1)
  
  sign<-vector(length = round(sim/2))
  nosign<-vector(length = round(sim/2))
  for (i in 1:round(sim/2)){ 
    # 2 kinds of trials
    nosign[i]<-max(rnorm(n_dist+n_targ,mu1,sd1)) # get numit samples from distractor distribution, take maximum
    sign[i]<-if (n_dist==0){max(rnorm(n_targ,mu2,sd2))}else if (n_targ==0) {max(rnorm(n_dist,mu1,sd1))}else if (t_type==TRUE){max(c(rnorm(n_dist,mu2,sd2),rnorm(n_targ,sig^2,sd2)))}else{max(c(rnorm(n_dist,mu1,sd1),rnorm(n_targ,mu2,sd2)))} # get numit-1 samples from distractor distribution and 1 from target distribution, take maximum
  }
  
  list('sign'=sign,'nosign'=nosign)
}

verghese_mod2<-function(sig,n_targ,n_dist,sim, t_type = FALSE){
  mu1<-0
  mu2<-sig # signal size (should be dprime of single target ident.)
  sd1<-1    # SD of target detector responses to distractor (Default: 1)
  sd2<-1    # SD of target detector responses to target (Default: 1)
  
  sign<-vector(length = round(sim/2))
  nosign<-vector(length = round(sim/2))
  for (i in 1:round(sim/2)){ 
    # 2 kinds of trials
    nosign_gl<-max(rnorm(n_dist+n_targ,mu1,sd1))
    sign_gl<-if (n_dist==0){max(rnorm(n_targ,mu2,sd2))}else if (n_targ==0) {max(rnorm(n_dist,mu1,sd1))}else if (t_type==TRUE){max(c(rnorm(n_dist,mu2,sd2),rnorm(n_targ,sig^2,sd2)))}else{max(c(rnorm(n_dist,mu1,sd1),rnorm(n_targ,mu2,sd2)))}
    nosign_rel<-max(rnorm(n_dist+n_targ,mu1,sd1))
    sign_rel<-if (n_dist==0){max(rnorm(n_dist+n_targ,mu1,sd1))}else{max(c(rnorm(7,mu1,sd1),rnorm(1,mu2,sd2)))}
    
    nosign[i]<-max(nosign_gl,nosign_rel)
    sign[i]<-max(sign_gl,sign_rel)
  }
  
  list('sign'=sign,'nosign'=nosign)
}


sig<-2.5
sim<-10000
mod1<-verghese_mod(sig,1,7,sim,FALSE)
mod2<-verghese_mod2(sig,1,7,sim,FALSE)

df<-data.frame(
  dv=c(mod1$sign,mod1$nosign,mod2$sign,mod2$nosign),
  s=rep(c(rep('Signal',sim/2),rep('NoSignal',sim/2)),2),
  c=c(rep('Model 1',sim),rep('Model 2',sim))
)


p <- ggplot(df, aes(x=dv, fill=s)) + geom_density(alpha=.3) + facet_wrap(~ c) + ggtitle(paste("Underlying Signal: ",sig)) + theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))
vline.data <- data.frame(z = c((mean(mod1$sign)+mean(mod1$nosign))/2,(mean(mod2$sign)+mean(mod2$nosign))/2), c = c('Model 1', 'Model 2'))
library(zoo)
library(ggplot2)

full_model<-function(sig_gl,sig_rel,n_targ,n_dist,sim, t_type = FALSE,opt=FALSE){
  numit<-n_dist+n_targ
  
  nosign_gl<-matrix(rnorm((sim/2)*numit,0,1), nrow = sim/2,ncol=numit, byrow = TRUE)
  nosign_rel<-matrix(rnorm((sim/2)*numit,0,1), nrow = sim/2,ncol=numit, byrow = TRUE)
  nosign<-apply(cbind(nosign_gl,nosign_rel),1,max)
  
  if (n_dist==0){
    sign_rel<-matrix(rnorm((sim/2)*numit,0,1), nrow = sim/2,ncol=numit, byrow = TRUE)
  }else{
    sign_rel<-cbind(matrix(rnorm((sim/2)*(numit-1),0,1), nrow = sim/2,ncol=numit-1, byrow = TRUE),
                    matrix(rnorm((sim/2),sig_rel,1), nrow = sim/2,ncol=1, byrow = TRUE))
  }
  if (t_type==TRUE){
    sign_gl<-cbind(matrix(rnorm((sim/2)*n_dist,sig_gl,1), nrow = sim/2,ncol=n_dist, byrow = TRUE),
                   matrix(rnorm((sim/2)*n_targ,sig_gl^2,1), nrow = sim/2,ncol=n_targ, byrow = TRUE))
  }else{
    sign_gl<-cbind(matrix(rnorm((sim/2)*n_dist,0,1), nrow = sim/2,ncol=n_dist, byrow = TRUE),
                   matrix(rnorm((sim/2)*n_targ,sig_gl,1), nrow = sim/2,ncol=n_targ, byrow = TRUE))
  }
  sign<-apply(cbind(sign_gl,sign_rel),1,max)
  
  list('sign'=sign,'nosign'=nosign)
}

sig_gl<-1.5
sig_rel<-1.5
n_targ<-7
n_dist<-1
sim<-10000
t_type = FALSE
run<-full_model(sig_gl,sig_rel,n_targ,n_dist,sim, t_type = FALSE)
nosign<-run$nosign
sign<-run$sign

df<-data.frame(
  dv=c(sign,nosign),
  s=c(rep('Signal',sim/2),rep('NoSignal',sim/2))
)
p <- ggplot(df, aes(x=dv, fill=s)) + geom_density(alpha=.3) + ggtitle('1B') + theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))
p
#vline.data <- data.frame(z = c((mean(mod1$sign)+mean(mod1$nosign))/2,(mean(mod2$sign)+mean(mod2$nosign))/2), c = c('Model 1', 'Model 2'))
# + facet_wrap(~ c)
# p + geom_vline(aes(xintercept = z), vline.data)

cs<-sort(c(sign,nosign))
sim<-length(cs)
fp<-mapply(function(c,sim) length(nosign[nosign>c])/(sim/2),cs,sim)
hit<-mapply(function(c,sim) length(sign[sign>c])/(sim/2),cs,sim)
plot(hit,fp)
AUC <- abs(sum(diff(fp)*rollmean(hit,2)))
qnorm(AUC)*sqrt(2)
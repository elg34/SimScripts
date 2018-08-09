# get numbers for poster

rm(list = ls())

library(ggplot2)
library(model)
require(zoo)

verghese2_opt<-function(gl,rel){
  sim<-1000
  targ<-c(1,7,8,1)
  dist<-c(7,1,0,7)
  t_type<-c(FALSE,FALSE,FALSE,TRUE)
  opt<-c(1.0255,1.8361,2.7298,1.6832) #LGD
  # opt<-c(1.5695,1.0598,0.7706,1.9045) #MW
  # opt<-c(1.669,1.0598,1.6832,1.956) #JS
  # opt<-c(0.5883,1.0488,1.366,1.366) #AM
  res<-mapply(full_model,sig_gl=rep(gl,length(targ)),sig_rel=rep(rel,length(targ)),n_targ=targ,n_dist=dist,sim=rep(sim,length(targ)),t_type=t_type,opt=opt)
  sqrt(mean(res^2))
}

verghese2_pred<-function(gl,rel){
  sim<-1000
  targ<-c(1,7,8,1)
  dist<-c(7,1,0,7)
  t_type<-c(FALSE,FALSE,FALSE,TRUE)
  opt<-c(1.0255,1.8361,2.7298,1.6832) #LGD
  # opt<-c(1.5695,1.0598,0.7706,1.9045) #MW
  # opt<-c(1.669,1.0598,1.6832,1.956) #JS
  # opt<-c(0.5883,1.0488,1.366,1.366) #AM
  res<-mapply(full_model,sig_gl=rep(gl,length(targ)),sig_rel=rep(rel,length(targ)),n_targ=targ,n_dist=dist,sim=rep(sim,length(targ)),t_type=t_type,opt=opt)
  res
}

xvals<-seq(0,4,0.2)
yvals<-seq(0,4,0.2)
grid<-expand.grid(x=xvals,y=yvals)
loss<-mapply(verghese2_opt,grid$x,grid$y)

df<-data.frame(
  x=grid$x,
  y=grid$y,
  loss=loss
)

ggplot(df, aes(x=x,y=y,z=loss)) + 
  geom_raster(aes(fill = loss)) +
  geom_contour(colour = "white") + 
  theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5, face="bold"))+
  xlab("Global Signal") + ylab("Relative Signal") +
  ggtitle("Loss of objective function: LGD")

best_comb<-c(df$x[df$loss==min(df$loss)],df$y[df$loss==min(df$loss)])
best_gl<-c(df$x[df$y==0][df$loss[df$y==0]==min(df$loss[df$y==0])],0)
best_rel<-c(0,df$y[df$x==0][df$loss[df$x==0]==min(df$loss[df$x==0])])

print(verghese2_pred(best_comb[1],best_comb[2]))
print(verghese2_pred(best_gl[1],best_gl[2]))
print(verghese2_pred(best_rel[1],best_rel[2]))

n<-100
comb_pred<-mapply(verghese2_pred,rep(best_comb[1],n),rep(best_comb[2],n))
gl_pred<-mapply(verghese2_pred,rep(best_gl[1],n),rep(best_gl[2],n))
rel_pred<-mapply(verghese2_pred,rep(best_rel[1],n),rep(best_rel[2],n))

print(rowMeans(comb_pred))
print(rowMeans(gl_pred))
print(rowMeans(rel_pred))


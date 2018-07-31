rm(list = ls())

library(ggplot2)
library(DEoptim)
library(model)
library(zoo)

verghese2_opt<-function(gl,rel,sim){
  sim<-10000
  targ<-c(1,7,8,1)
  dist<-c(7,1,0,7)
  t_type<-c(FALSE,FALSE,FALSE,TRUE)
  opt<-c(1.0255,1.8361,2.7298,1.6832)#c(0.5460,1.1775,0.9761,0.8691)
  res<-mapply(full_model,sig_gl=rep(gl,length(targ)),sig_rel=rep(rel,length(targ)),n_targ=targ,n_dist=dist,sim=rep(sim,length(targ)),t_type=t_type,opt=opt)
  sqrt(mean(res^2))
}

xvals<-seq(0,6,0.1)
yvals<-seq(0,6,0.1)
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

#o<-DEoptim(verghese2_opt, lower = c(0, 0), upper = c(5, 5), control = DEoptim.control(trace = 10, strategy = 6, itermax = 1000,steptol = 50, reltol = 1e-10))




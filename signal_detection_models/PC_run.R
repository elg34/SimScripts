rm(list = ls())

library(ggplot2)
#require(DEoptim)
library(model)
require(zoo)

verghese2_opt<-function(gl,rel){
  targ<-c(1,7,8,1)
  dist<-c(7,1,0,7)
  t_type<-c(FALSE,FALSE,FALSE,TRUE)
  opt<-c(1.0255,1.8361,2.7298,1.6832) #LGD
  #opt<-c(1.5695,1.0598,0.7706,1.9045) #MW
  #opt<-c(1.669,1.0598,1.6832,1.956) #JS
  #opt<-c(0.5883,1.0488,1.366,1.366) #AM
  res<-mapply(full_model,sig_gl=rep(gl,length(targ)),sig_rel=rep(rel,length(targ)),n_targ=targ,n_dist=dist,t_type=t_type,opt=opt)
  sqrt(mean(res^2))
}

xvals<-seq(0,5,0.1)
yvals<-seq(0,5,0.1)
grid<-expand.grid(x=xvals,y=yvals)
loss<-mapply(verghese2_opt,grid$x,grid$y)

df<-data.frame(
  x=grid$x,
  y=grid$y,
  loss=loss
)
df$loss[df$loss==Inf]=max(df$loss[df$loss!=Inf])

ggplot(df, aes(x=x,y=y,z=loss)) + 
  geom_raster(aes(fill = loss)) + scale_fill_gradientn(colours=c("#FF0000FF","#FFFF00","#000000")) +
  geom_contour(colour = "white") + 
  theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5, face="bold"))+
  xlab("Global Signal") + ylab("Relative Signal") +
  ggtitle("Loss of objective function: LGD")
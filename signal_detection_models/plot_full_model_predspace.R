rm(list = ls())
library(model)

targ<-c(1,7,8,1)
dist<-c(7,1,0,7)
t_type<-c(FALSE,FALSE,FALSE,TRUE)
label<-mapply(function(x,y,z) paste("T:",x,"/","D:",y,'/T:B^2=',z,sep=''), targ, dist, t_type)

cond<-4

sim<-10000
xvals<-seq(0,6,0.5)
yvals<-seq(0,6,0.5)
grid<-expand.grid(x=xvals,y=yvals)
dp<-mapply(full_model,grid$x,grid$y,rep(targ[cond],length(grid$x)), rep(dist[cond],length(grid$x)), rep(sim,length(grid$x)),rep(t_type[cond],length(grid$x)))

df<-data.frame(
  x=grid$x,
  y=grid$y,
  dp=dp
)

ggplot(df, aes(x=x,y=y,z=dp)) + 
  geom_raster(aes(fill = dp)) +
  geom_contour(colour = "white") + 
  theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5, face="bold"))+
  xlab("Global Signal") + ylab("Relative Signal") +
  ggtitle(paste("Full Model:: T:",targ[cond],"/","D:",dist[cond],'/T:B^2=',t_type[cond],sep=''))
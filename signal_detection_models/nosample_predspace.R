rm(list = ls())
library(model)
library(ggplot2)
library(zoo)

targ<-c(1,7,8,1)
dist<-c(7,1,0,7)
t_type<-c(FALSE,FALSE,FALSE,TRUE)
label<-mapply(function(x,y,z) paste("T:",x,"/","D:",y,'/T:B^2=',z,sep=''), targ, dist, t_type)

xvals<-seq(0,6,0.1)
yvals<-seq(0,6,0.1)
grid<-expand.grid(x=xvals,y=yvals)

dp17<-mapply(full_model,grid$x,grid$y,rep(targ[1],length(grid$x)), rep(dist[1],length(grid$x)),rep(t_type[1],length(grid$x)))
dp71<-mapply(full_model,grid$x,grid$y,rep(targ[2],length(grid$x)), rep(dist[2],length(grid$x)),rep(t_type[2],length(grid$x)))
dp80<-mapply(full_model,grid$x,grid$y,rep(targ[3],length(grid$x)), rep(dist[3],length(grid$x)),rep(t_type[3],length(grid$x)))
dp17s<-mapply(full_model,grid$x,grid$y,rep(targ[4],length(grid$x)), rep(dist[4],length(grid$x)),rep(t_type[4],length(grid$x)))

df<-data.frame(
  x=grid$x,
  y=grid$y,
  dp17=dp17,
  dp71=dp71,
  dp80=dp80,
  dp17s=dp17s
)

ggplot(df, aes(x=x,y=y,z=dp17)) + 
  geom_raster(aes(fill = dp17)) +
  geom_contour(colour = "white") + 
  theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5, face="bold"))+
  xlab("Global Signal") + ylab("Relative Signal") +
  ggtitle(paste("Full Model:: T:",targ[1],"/","D:",dist[1],'/T:B^2=',t_type[1],sep=''))

ggplot(df, aes(x=x,y=y,z=dp71)) + 
  geom_raster(aes(fill = dp71)) +
  geom_contour(colour = "white") + 
  theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5, face="bold"))+
  xlab("Global Signal") + ylab("Relative Signal") +
  ggtitle(paste("Full Model:: T:",targ[2],"/","D:",dist[2],'/T:B^2=',t_type[2],sep=''))

ggplot(df, aes(x=x,y=y,z=dp80)) + 
  geom_raster(aes(fill = dp80)) +
  geom_contour(colour = "white") + 
  theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5, face="bold"))+
  xlab("Global Signal") + ylab("Relative Signal") +
  ggtitle(paste("Full Model:: T:",targ[3],"/","D:",dist[3],'/T:B^2=',t_type[3],sep=''))

ggplot(df, aes(x=x,y=y,z=dp17s)) + 
  geom_raster(aes(fill = dp17s)) +
  geom_contour(colour = "white") + 
  theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5, face="bold"))+
  xlab("Global Signal") + ylab("Relative Signal") +
  ggtitle(paste("Full Model:: T:",targ[4],"/","D:",dist[4],'/T:B^2=',t_type[4],sep=''))

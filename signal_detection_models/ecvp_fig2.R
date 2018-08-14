rm(list = ls())
library(model)
library(ggplot2)
library(zoo)

targ<-c(1,7,8,1)
dist<-c(7,1,0,7)
t_type<-c(FALSE,FALSE,FALSE,TRUE)
label<-c('one moves','all but one move','all move','all plus one move')

xvals<-seq(0,5,0.1)
yvals<-seq(0,5,0.1)
grid<-expand.grid(x=xvals,y=yvals)
opt = c(1.0255,1.8361,2.7298,1.6832)

dp17<-mapply(full_model,grid$x,grid$y,rep(targ[1],length(grid$x)), rep(dist[1],length(grid$x)),rep(t_type[1],length(grid$x)),rep(opt[1],length(grid$x)))
dp71<-mapply(full_model,grid$x,grid$y,rep(targ[2],length(grid$x)), rep(dist[2],length(grid$x)),rep(t_type[2],length(grid$x)),rep(opt[2],length(grid$x)))
dp80<-mapply(full_model,grid$x,grid$y,rep(targ[3],length(grid$x)), rep(dist[3],length(grid$x)),rep(t_type[3],length(grid$x)),rep(opt[3],length(grid$x)))
dp17s<-mapply(full_model,grid$x,grid$y,rep(targ[4],length(grid$x)), rep(dist[4],length(grid$x)),rep(t_type[4],length(grid$x)),rep(opt[4],length(grid$x)))


df<-data.frame(
  x=rep(grid$x,length(label)),
  y=rep(grid$y,length(label)),
  Error=abs(c(dp17,dp71,dp80,dp17s)),
  Condition=c((mapply(function(x) rep(x,length(grid$x)),label)))
)

ggplot(df, aes(x=x,y=y,z=Error)) + 
  geom_raster(aes(fill = Error)) + scale_fill_gradientn(colours=c("#FF0000FF","#FFFF00","#000000"),limits=c(0,8)) +
  geom_contour(colour = "white") + 
  theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))+
  xlab("Global Signal") + ylab("Relative Signal") +
  facet_wrap(vars(Condition),nrow = 2) +
  ggtitle('Combined Model:')


# for (i in label){
#   print(i)
#   dfsubs<-subset(df,Condition==i)
#   best_comb<-c(dfsubs$x[dfsubs$Error==min(dfsubs$Error)],dfsubs$y[dfsubs$Error==min(dfsubs$Error)])
#   best_gl<-dfsubs$x[dfsubs$y==0][dfsubs$Error[dfsubs$y==0]==min(dfsubs$Error[dfsubs$y==0])]
#   best_rel<-dfsubs$y[dfsubs$x==0][dfsubs$Error[dfsubs$x==0]==min(dfsubs$Error[dfsubs$x==0])]
#   print(paste('Condc',i,'::: Comb:: x:',best_comb[1],'y:',best_comb[2],
#               'Gl:: x:',best_gl,
#               'Rel:: y:',best_rel))
# }

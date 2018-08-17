rm(list = ls())
library(ggplot2)
library(model)
require(zoo)

setwd("C:/Users/Lobster/Documents/GitHub/SimScripts/signal_detection_models/")

verghese2_opt<-function(gl,rel,it){
  opt<-matrix(c(c(1.0255,1.8361,2.7298,1.6832), #L
                c(1.9098,1.0488,1.0505,1.6217), #M
                c(1.2822,1.5479,1.6632,1.548), #J
                c(1.0158,0.721,0.9488,0.7777)), #A
              ncol = 4,byrow = TRUE)
  
  targ<-c(1,7,8,1)
  dist<-c(7,1,0,7)
  t_type<-c(FALSE,FALSE,FALSE,TRUE)
  
  res<-mapply(full_model,sig_gl=rep(gl,length(targ)),sig_rel=rep(rel,length(targ)),n_targ=targ,n_dist=dist,t_type=t_type,opt=opt[it,])
  sqrt(mean(res^2))
}

subj<-c('S1','S2','S3','S4')
i=4

xvals<-seq(0,5,0.1)
yvals<-seq(0,5,0.1)
grid<-expand.grid(x=xvals,y=yvals)
loss<-mapply(verghese2_opt,grid$x,grid$y,i)

df<-data.frame(
  x=grid$x,
  y=grid$y,
  loss=loss
)
if (length(df$loss[df$loss==Inf])>0){
  print(i,'inf!')
  df$loss[df$loss==Inf]=max(df$loss[df$loss!=Inf])
}

best_comb<-c(df$x[df$loss==min(df$loss)],df$y[df$loss==min(df$loss)])
best_gl<-df$x[df$y==0][df$loss[df$y==0]==min(df$loss[df$y==0])]
best_rel<-df$y[df$x==0][df$loss[df$x==0]==min(df$loss[df$x==0])]
print(paste('S',i,'::: Comb:: x:',best_comb[1],'y:',best_comb[2],
            'Gl:: x:',best_gl,
            'Rel:: y:',best_rel))

p<-ggplot(df, aes(x=x,y=y,z=loss)) + 
  geom_raster(aes(fill = loss)) + scale_fill_gradientn(colours=c("#FF0000FF","#FFFF00","#000000"),limits=c(0,6.9)) +
  geom_contour(colour = "white") + 
  theme(text = element_text(size=24),plot.title = element_text(hjust = 0.5, face="bold"))+
  xlab("Global Signal") + ylab("Relative Signal") +
  ggtitle(subj[i]) +
  geom_point(aes(x=best_comb[1], y=best_comb[2]), colour="white")
p
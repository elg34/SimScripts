rm(list = ls())
library(model)
library(zoo)
library(ggplot2)
setwd("C:/Users/Lobster/Documents/GitHub/SimScripts/signal_detection_models/")

targ<-c(1,7,8,1)
dist<-c(7,1,0,7)
t_type<-c(FALSE,FALSE,FALSE,TRUE)
label<-c('one moves','all but one move','all move','all plus one move')
opt = c(1.0255,1.8361,2.7298,1.6832)

xvals<-seq(0,5,0.1)
Error<-NULL
B<-1.25
for (i in xvals){
  Error<-c(Error,mapply(full_model, rep(0,length(targ)),rep(i,length(targ)), targ, dist,t_type,opt))
}

df<-data.frame(
  x=sort(rep(xvals,length(targ))),
  Error=abs(Error),
  Condition=c(t(mapply(function(x) rep(x,length(xvals)),label)))
)

# ggplot(df, aes(x = x, y = Error)) +
#   geom_point(size=3,aes(shape=Condition,color=Error),show.legend=FALSE) + geom_line(aes(color=Error)) +
#   scale_color_gradientn(colours=c("#FF0000FF","#FFFF00","#000000")) +
#   xlim(0, 5) + #ylim(0.45, 1) +
#   xlab("Underlying Signal") + ylab("Error") +
#   ggtitle("Relative-only Model:") + 
#   facet_wrap(vars(Condition),nrow = 2) +
#   theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))

df<-subset(df, Condition=='all plus one move')

ggplot(df, aes(x = x, y = Error)) +
  geom_point(size=3,aes(shape=Condition,color=Error),show.legend=FALSE) + geom_line(aes(color=Error)) +
  scale_color_gradientn(colours=c("#FF0000FF","#FFFF00","#000000"),limits=c(0,8)) +
  xlim(0, 5) + #ylim(0.45, 1) +
  xlab("Relative Signal") + ylab("Error") +
  ggtitle("Relative-only Model:") +
  scale_y_reverse() + 
  coord_flip() + #scale_x_reverse() + 
  facet_wrap(vars(Condition),nrow = 2) +
  theme(text = element_text(size=24),plot.title = element_text(hjust = 0.5, face="bold"))
rm(list = ls())
library(model)
library(zoo)
library(ggplot2)
setwd("C:/Users/Lobster/Documents/GitHub/SimScripts/signal_detection_models/")

targ<-c(1,7,8,1)
dist<-c(7,1,0,7)
t_type<-c(FALSE,FALSE,FALSE,TRUE)
label<-c('one moves','all but one move','all move','all plus one move')

xvals<-seq(0,5,0.5)
dp<-NULL
B<-1.25
for (i in xvals){
  dp<-c(dp,mapply(full_model, rep(i,length(targ)),rep(0,length(targ)), targ, dist,t_type))
}

df<-data.frame(
  x=sort(rep(xvals,length(targ))),
  dp=dp,
  Condition=c(t(mapply(function(x) rep(x,length(xvals)),label)))
)

p1<-ggplot(df, aes(x = x, y = dp, color=Condition, shape=Condition)) +
  geom_point(size=3) + geom_line() +
  xlim(0, 5) + #ylim(0.45, 1) +
  xlab("Underlying Signal") + ylab("Pred. Performance") +
  ggtitle("Global-only Model:") + 
  theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"),legend.position = c(0.15, 0.8))
p1
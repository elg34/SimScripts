rm(list = ls())
library(model)
targ<-c(1,7,8,1)
dist<-c(7,1,0,7)
t_type<-c(FALSE,FALSE,FALSE,TRUE)
label<-mapply(function(x,y,z) paste("T:",x,"/","D:",y,'/T:B^2=',z,sep=''), targ, dist, t_type)

sim<-1000
xvals<-seq(0,6,0.5)
dp<-NULL
for (i in xvals){ # 'fp'=fp,'hit'=hit,'rej'=rej,'miss'=miss,'AUC'=AUC,'dp'=dp
  dp<-c(dp,mapply(gl_model, rep(i,length(targ)), targ, dist, sim,t_type))
}

df<-data.frame(
  x=sort(rep(xvals,length(targ))),
  dp=dp,
  group=c(t(mapply(function(x) rep(x,length(xvals)),label)))
)


p1<-ggplot(df, aes(x = x, y = dp, color=group, linetype=group)) +
  geom_point(size=3) + geom_line() +
  xlim(0, 6) + #ylim(0.45, 1) +
  xlab("Underlying Signal") + ylab("Pred. Performance") +
  ggtitle("Global-only Model:") + 
  theme(text = element_text(size=20),plot.title = element_text(hjust = 0.5, face="bold"))
p1
library(ggplot2)

x<-seq(0,1,0.01)
df<-data.frame(
  x=x,
  dv=c(unlist(lapply(x,function(x) x^2)),unlist(lapply(x,function(x) (x+x^2)))),
  dv2=c(unlist(lapply(x,function(x) x^2/x)),unlist(lapply(x,function(x) (x+x^2)/x))),
  label=c(rep('Old x^2',length(x)),rep('New 1+x^2',length(x)))
)

p1<-ggplot(df, aes(x = x, y = dv, color=label, linetype=label)) +
  geom_point(size=3) + geom_line()
p1

p2<-ggplot(df, aes(x = x, y = dv2, color=label, linetype=label)) +
  geom_point(size=3) + geom_line()
p2

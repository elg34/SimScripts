verghese2_pred<-function(gl,rel){
  sim<-10000
  targ<-c(1,7,8,1)
  dist<-c(7,1,0,7)
  t_type<-c(FALSE,FALSE,FALSE,TRUE)
  opt<-c(1.0255,1.8361,2.7298,1.6832)#c(0.5460,1.1775,0.9761,0.8691)
  res<-mapply(full_model,sig_gl=rep(gl,length(targ)),sig_rel=rep(rel,length(targ)),n_targ=targ,n_dist=dist,sim=rep(sim,length(targ)),t_type=t_type)
  list(res,res-opt)
}

opt<-c(1.0255,1.8361,2.7298,1.6832)
cond<-c('1B','B1','BB','BB^2')
pred <- verghese2_pred(1.4,1.6)

LGD<-data.frame(
  cond = cond,
  real = opt,
  pred = unlist(pred[1]),
  loss = unlist(pred[2])
)

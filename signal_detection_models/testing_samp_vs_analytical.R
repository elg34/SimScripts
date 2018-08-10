rm(list = ls())
library(model)

sig_gl=0.5
sig_rel=0.5
n_targ=1
n_dist=7
t_type = TRUE
opt=FALSE
sim=10000

print(gl_model(sig_gl,n_targ,n_dist, t_type,opt))
print(gl_model_samp(sig_gl,n_targ,n_dist,sim, t_type,opt))

print(gl_model(sig_rel,n_targ,n_dist, t_type,opt))
print(gl_model_samp(sig_rel,n_targ,n_dist,sim, t_type,opt))

print(full_model(sig_gl,sig_rel,n_targ,n_dist, t_type,opt))
print(full_model_samp(sig_gl,sig_rel,n_targ,n_dist,sim, t_type,opt))

sig_gl=0
sig_rel=0.5
n_targ=1
n_dist=7
t_type = TRUE
opt=FALSE
print(full_model(sig_gl,sig_rel,n_targ,n_dist, t_type,opt))
n_targ=8
n_dist=0
t_type = FALSE
print(full_model(sig_gl,sig_rel,n_targ,n_dist, t_type,opt))



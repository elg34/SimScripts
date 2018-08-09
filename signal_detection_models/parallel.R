library(foreach)
library(model)
library(zoo)
library(foreach)
library(doParallel)

cl<-makeCluster(no_cores)
registerDoParallel(cl)
n<-10000
start.time <- Sys.time()
x <- foreach(i=1:n, .combine='c', .packages = c("zoo", "model")) %dopar% full_model(2,1,1,8,100)
end.time <- Sys.time()
print(end.time - start.time)
stopImplicitCluster()
start.time <- Sys.time()
x <- mapply(full_model,rep(2,n),rep(1,n),rep(1,n),rep(8,n),rep(100,n))
end.time <- Sys.time()
print(end.time - start.time)


cl<-makeCluster(no_cores)
registerDoParallel(cl)
n<-10000
start.time <- Sys.time()
x <- foreach(i=1:n, .combine='c', .packages = c("zoo", "model")) %dopar% rnorm(1000,1,1)
end.time <- Sys.time()
print(end.time - start.time)
stopImplicitCluster()




#' Get d' from signal/nosignal samples
#'
#' Returns d prime calculated from the area under the ROC curve
#' @param sign Samples from a signal distribution for a condition
#' @param nosign Samples from a noise distribution for a condition
#' @export
#' @examples
#' get_dp()

library(zoo)
get_dp<-function(sign,nosign){
  cs<-sort(c(sign,nosign))
  sim<-length(cs)
  fp<-mapply(function(c,sim) length(nosign[nosign>c])/(sim/2),cs,sim)
  hit<-mapply(function(c,sim) length(sign[sign>c])/(sim/2),cs,sim)
  AUC <- abs(sum(diff(fp)*rollmean(hit,2)))
  
  qnorm(AUC)*sqrt(2)
}
#' Full version of the model
#'
#' Returns either the d-prime of a condition given by the inputs or the absolute error of the predicted d' relative to a prespecified value.
#' @param sig_gl Signal strength for detecting position change for a single sphere
#' @param sig_rel Signal strength for detecting relative position change for a single sphere
#' @param n_targ The number of targets, i.e. spheres that move on that trial
#' @param n_dist The number of static distractors (unless t_type=TRUE), in which case it is the number of spheres moving with lesser signal strength
#' @param t_type The type of the target/distractor relationship
#' @param opt An existing d' to compare predictions to if given
#' @keywords fullmodel
#' @export
#' @examples
#' full_model()

full_model<-function(sig_gl,sig_rel,n_targ,n_dist, t_type = FALSE,opt=FALSE){
  numit<-n_dist+n_targ
  if (n_targ==0){print('Warning! Not checked for 0 targets!')}
  
  nosig<-0
  if (t_type==TRUE){
    sigd<-sig_gl
    sigt<-2*sig_gl
  }else{
    sigd<-0
    sigt<-sig_gl
  }
  
  x <- seq(-10,20,0.1)
  
  fp<- 1-(pnorm(x, mean = nosig, sd = 1)^(numit*2))
  if (n_dist==0){
    hit<-1-(pnorm(x, mean = sigt, sd = 1)^n_targ * pnorm(x, mean = nosig, sd = 1)^numit)
  }else{
    hit<-1-(pnorm(x, mean = sigd, sd = 1)^n_dist * pnorm(x, mean = sigt, sd = 1)^n_targ * 
              pnorm(x, mean = sig_rel, sd = 1) * pnorm(x, mean = nosig, sd = 1)^(numit-1))
  }
  
  AUC <- abs(sum(diff(fp)*rollmean(hit,2)))
  dp<-qnorm(AUC)*sqrt(2)
  
  if (opt==FALSE){
    dp
  }else{
    dp-opt
  }
}
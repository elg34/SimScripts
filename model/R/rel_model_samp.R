#' Relative-only version of the model
#'
#' Returns either the d-prime of a condition given by the inputs or the absolute error of the predicted d' relative to a prespecified value.
#' @param sig_rel Signal strength for detecting relative position change for a single sphere
#' @param n_targ The number of targets, i.e. spheres that move on that trial
#' @param n_dist The number of static distractors (unless t_type=TRUE), in which case it is the number of spheres moving with lesser signal strength
#' @param sim The number of trials to simulate
#' @param t_type The type of the target/distractor relationship
#' @param opt An existing d' to compare predictions to if given
#' @keywords fullmodel
#' @export
#' @examples
#' rel_model_samp()

rel_model_samp<-function(sig_rel,n_targ,n_dist,sim, t_type = FALSE,opt=FALSE){
  numit<-n_dist+n_targ
  
  nosign<-apply(matrix(rnorm((sim/2)*numit,0,1), nrow = sim/2,ncol=numit, byrow = TRUE),1,max)
  
  if (n_dist==0){
    sign<-apply(matrix(rnorm((sim/2)*numit,0,1), nrow = sim/2,ncol=numit, byrow = TRUE),1,max)
  }else{
    sign<-apply(cbind(matrix(rnorm((sim/2)*(numit-1),0,1), nrow = sim/2,ncol=numit-1, byrow = TRUE),
                    matrix(rnorm((sim/2),sig_rel,1), nrow = sim/2,ncol=1, byrow = TRUE)),1,max)
  }
  dp<-get_dp(sign,nosign)
  
  if (opt==FALSE){
    dp
  }else{
    dp-opt
  }
}
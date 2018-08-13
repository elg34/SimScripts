#' Full version of the model
#'
#' Returns either the d-prime of a condition given by the inputs or the absolute error of the predicted d' relative to a prespecified value.
#' @param sig_gl Signal strength for detecting position change for a single sphere
#' @param sig_rel Signal strength for detecting relative position change for a single sphere
#' @param n_targ The number of targets, i.e. spheres that move on that trial
#' @param n_dist The number of static distractors (unless t_type=TRUE), in which case it is the number of spheres moving with lesser signal strength
#' @param sim The number of trials to simulate
#' @param t_type The type of the target/distractor relationship
#' @param opt An existing d' to compare predictions to if given
#' @keywords fullmodel
#' @export
#' @examples
#' full_model_samp()

full_model_samp<-function(sig_gl,sig_rel,n_targ,n_dist,sim, t_type = FALSE,opt=FALSE){
  numit<-n_dist+n_targ
  
  nosign_gl<-matrix(rnorm((sim/2)*numit,0,1), nrow = sim/2,ncol=numit, byrow = TRUE)
  nosign_rel<-matrix(rnorm((sim/2)*numit,0,1), nrow = sim/2,ncol=numit, byrow = TRUE)
  nosign<-apply(cbind(nosign_gl,nosign_rel),1,max)
  
  if (n_dist==0){
    sign_rel<-matrix(rnorm((sim/2)*numit,0,1), nrow = sim/2,ncol=numit, byrow = TRUE)
  }else{
    sign_rel<-cbind(matrix(rnorm((sim/2)*(numit-1),0,1), nrow = sim/2,ncol=numit-1, byrow = TRUE),
              matrix(rnorm((sim/2),sig_rel,1), nrow = sim/2,ncol=1, byrow = TRUE))
  }
  if (t_type==TRUE){
    sign_gl<-cbind(matrix(rnorm((sim/2)*n_dist,sig_gl,1), nrow = sim/2,ncol=n_dist, byrow = TRUE),
                   matrix(rnorm((sim/2)*n_targ,2*sig_gl,1), nrow = sim/2,ncol=n_targ, byrow = TRUE))
  }else{
    sign_gl<-cbind(matrix(rnorm((sim/2)*n_dist,0,1), nrow = sim/2,ncol=n_dist, byrow = TRUE),
                   matrix(rnorm((sim/2)*n_targ,sig_gl,1), nrow = sim/2,ncol=n_targ, byrow = TRUE))
  }
  sign<-apply(cbind(sign_gl,sign_rel),1,max)
  dp<-get_dp(sign,nosign)
  
  if (opt==FALSE){
    dp
  }else{
    dp-opt
  }
}
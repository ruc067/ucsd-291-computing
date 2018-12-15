dpareto <- function(x=2,alpha=1,beta=1,log=FALSE){
  dpa <- .dpareto(x,alpha,beta,logd = log)
  if ( any( is.nan( dpa)) ) warning("NaNs in x")
  return(dpa)
}

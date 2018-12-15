ppareto <- function(x=2, alpha=1, beta=1, lower.tail = TRUE,
                    log.p = FALSE){
  alpha <- ifelse(alpha <= 0, NA, alpha)
  
  beta <- ifelse(beta <= 0, NA, beta)
  
  if(!is.null(x)){
    options(warn=-1)
    x <-  ifelse(x < alpha, 0, x)
  }
  
  options(warn=0)
  
  p.pareto <- beta*(log(alpha)-log(x))
  
  if (!log.p) {
    p.pareto <- exp(p.pareto)
    if (lower.tail) {p.pareto <- 1 - p.pareto}
  } else {
    if (lower.tail) {
      p.pareto <- log(1 - exp(p.pareto))
    }
  }
  
  if ( any( is.nan( p.pareto)) ) warning("NaNs in x")
  
  return(p.pareto)
}

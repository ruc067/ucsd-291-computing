ppareto <- function(q=2, alpha=1, beta=1, lower.tail = TRUE,
                    log.p = FALSE){
  alpha <- ifelse(alpha <= 0, NA, alpha)
  
  beta <- ifelse(beta <= 0, NA, beta)
  
  if(!is.null(q)){
    options(warn=-1)
    q <-  ifelse(round(q,8) < alpha, 0, q)
  }
  
  options(warn=0)
  
  p.pareto <- beta*(log(alpha)-log(q))
  
  if (!log.p) {
    p.pareto <- exp(p.pareto)
    if (lower.tail) {p.pareto <- 1 - p.pareto}
  } else {
    if (lower.tail) {
      p.pareto <- log(1 - exp(p.pareto))
    }
  }
  
  if ( any( is.nan( p.pareto)) ) warning("NaNs in q")
  
  return(p.pareto)
}

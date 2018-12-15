qpareto <- function(p=0.5, alpha=1, beta=1, lower.tail = TRUE,
                    log.p = FALSE){
  alpha <- ifelse(alpha <= 0, NA, alpha)
  
  beta <- ifelse(beta <= 0, NA, beta)
  
  if (log.p) {p <- exp(p)}
  
  p <- ifelse((p < 0 | p > 1), NA, p)
  
  if (!lower.tail) {p <- 1-p}
  
  q.pareto <- exp((beta*log(alpha)-log(1-p))/beta)
  
  if ( any( is.nan( q.pareto)) ) warning("NaNs in x")
  
  return(q.pareto)
}

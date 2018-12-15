rpareto <- function(n = 1, alpha = 2, beta = 2){
  u <- runif(n)
  x <- qpareto(u, alpha, beta)
  return(x)
}

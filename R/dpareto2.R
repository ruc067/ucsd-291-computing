dpareto2 <- function(x=2,alpha=1,beta=1,log=FALSE){
  dpareto2.density <- log (beta) + beta * log(alpha) - (1 + beta ) * log (x)
  if ( log == FALSE ) {
    dpareto2.density <- exp(dpareto2.density)
  }
  dpareto2.density[alpha <= 0 | beta <= 0] <- NaN
  dpareto2.density[x<alpha&alpha>0&beta>0] <- 0
  return(dpareto2.density)
}

dpareto1 <- function(x=2,alpha=1,beta=1, log = FALSE){
  stopifnot(1L == length( x ),
            1L == length( alpha ),
            1L == length( beta ),
            1L == length( log ))
  if (alpha <= 0||beta <= 0){
    dpareto1.density <- NaN
  } else if (x < alpha){
    dpareto1.density <- 0
  } else if(log){
    dpareto1.density <- log (beta) + beta * log( alpha ) -
      (1 + beta ) * log (x)
  } else{
    dpareto1.density <- beta*alpha^beta/x^(beta+1)
  }
  return(dpareto1.density)
}

dparetoVec <- Vectorize( dpareto1, vectorize.args = c("x", "alpha", "beta"))

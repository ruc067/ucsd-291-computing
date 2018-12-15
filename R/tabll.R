tabll <- function(p.vec, eta.mat, X){
  M <- length(p.vec)
  
  stopifnot( M == dim(eta.mat)[1] )
  stopifnot( dim(eta.mat)[2] == dim(X)[2] )
  
  log.likelihood <- 0
  for(i in 1:dim(X)[1]){
    log.likelihood <- log.likelihood + 
    log( sum( p.vec * 
                apply(eta.mat,1,
                      function(p) dmultinom(x = X[i,], sum(X[i,]),prob = p) ) ))
  }
  
  return(log.likelihood)
}

ExZ.X <- function(p.vec, eta.mat, X){
  M <- length(p.vec)
  EZ <- matrix(NA,nrow = dim(X)[1], ncol = length(p.vec))
  for (i in 1:dim(X)[1]){
    EZ[i,] <- outer(X[i,1], 1:M, function(x, j) p.vec[j] * 
                      apply(eta.mat,1,
                            function(p) dmultinom(x = X[i,], 
                                                  sum(X[i,]),prob = p) )[j])
  }
  EZ <- sweep(EZ, 1, rowSums(EZ), "/")
  return(EZ)
}


Mparms <- function(expect.Z = ExZ.X(p.vec, eta.mat, X), X){
  eta.mat.p <- t(expect.Z) %*% X
  eta.mat <- eta.mat.p / rowSums(eta.mat.p)
  
  p.vec <-  colSums(expect.Z) / sum(colSums(expect.Z))
  
  theta <- list(p.vec,eta.mat)
  
  names(theta) <- c("p.vec","eta.mat")
  
  return(theta)
}


updatell <- function(p.vec, eta.mat, X, n.iter){
  M <- length(p.vec)
  
  stopifnot( M == dim(eta.mat)[1] )
  stopifnot( dim(eta.mat)[2] == dim(X)[2] )
  
  for(i in 1:n.iter){
    theta <- Mparms(expect.Z = ExZ.X(p.vec, eta.mat, X), X)
    
    loglikelihood <- tabll(theta$p.vec, theta$eta.mat, X)
    
    p.vec <- theta$p.vec
    eta.mat <- theta$eta.mat
  }
  outcome.list <- list(theta$p.vec, theta$eta.mat, loglikelihood)
  names(outcome.list) <- c("p.vec", "eta.mat", "loglikelihood")
  
  return(outcome.list)
}

#Problem 6
#p.vec <- c(0.5,0.5)
#eta.mat <- matrix(c(0.99,0.01,0.01,0.99), byrow = T, nrow = 2)
#X <-matrix(c(10,0,0,10,5,5), byrow = T, nrow = 3)
#ExZ.X(p.vec, eta.mat, X)

#Problem 7
#tabll(p.vec, eta.mat, X)

#p.vec.update <- (Mparms(expect.Z = ExZ.X(p.vec, eta.mat, X), X))$p.vec
#eta.mat.update <- (Mparms(expect.Z = ExZ.X(p.vec, eta.mat, X), X))$eta.mat

#tabll(p.vec.update, eta.mat.update, X)

#Problem 9
#X105 <- read.csv("X105.csv",header = T)
#p.vec <- c(0.3,0.3,0.4)
#eta.mat <- matrix(c(0.3,0.4,0.3,0.1,0.1,0.8,0.5,0.3,0.2), byrow = T, nrow = 3)

#tabll(p.vec, eta.mat, X105)

#p.vec.update <- (Mparms(expect.Z = ExZ.X(p.vec, eta.mat, data.matrix(X105)), 
#    data.matrix(X105)))$p.vec
#eta.mat.update <- (Mparms(expect.Z = ExZ.X(p.vec, eta.mat, data.matrix(X105)),
#    data.matrix(X105)))$eta.mat

#tabll(p.vec.update, eta.mat.update, X)

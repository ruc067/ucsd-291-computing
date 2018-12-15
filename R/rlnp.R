dlnp <-
  function(x,y ,log = FALSE) {
    val <-
      dnorm( x, log = TRUE ) +
      dpois( y, exp( x ), log = TRUE)
    if (log) val else exp( val )
  }

rlnp <- function(n=1, y=1, scale=1){
  if(y==0){
    dnp.opt <- (optimize(function(x) -dlnp(x, y), interval = 
                           c(-10, 10)))
    x0 <- dnp.opt$minimum
  } else{
    dnp.opt <- (optimize(function(x) -dlnp(x, y, log = T), interval = 
                           c(log(y)-2, (log(y)+2))))
    x0 <- dnp.opt$minimum
  }
  
  M <- (-(optimize(function(x) -(dlnp(x, y)/dcauchy(x, location = x0, scale = scale)), 
                   interval = 
                     c(x0-10, x0)))$objective)
  
  #stopifnot(M <= x0)
  
  n.accept  <- 0
  x <- yunif <- keep <- dratio <-  list()
  i <- 0
  
  while( n.accept < n )
  {
    i <- i + 1
    n.more <- n - n.accept
    x[[ i ]] <- rcauchy(n.more, location = x0, scale)
    yunif[[ i ]]  <- runif( n.more )
    dratio[[ i ]] <-
      dlnp(x[[ i ]], y=y) / (M * dcauchy(x[[ i ]],
                                         dnp.opt$minimum, scale))
    keep[[ i ]] <-
      yunif[[ i ]] <= dlnp(x[[ i ]], y=y) /
      (M * dcauchy(x[[ i ]], x0, scale))
    n.accept <- n.accept + sum( keep[[ i ]] )
  } 
  
  stopifnot(round(dlnp(unlist(x),y)/(M*dcauchy(unlist(x), location = x0, scale = scale)),6)<=1)
  
  dlnp.value <- list(unlist(x), unlist(yunif), unlist(dratio), unlist(keep), x0, M, y, scale)
  names(dlnp.value) <- c("x", "yunif", "dratio", "keep", "x0", "M", "y", "scale")
  
  return(dlnp.value)
}

#debugonce(rlnp); rlnp(100,0,1)
#debugonce(rlnp); rlnp(100,10,1)
#debugonce(rlnp); rlnp(100,100,1)

#sum(rlnp(100,100,1)$keep)/length((rlnp(100,100,1)$keep))
#sum(rlnp(100,100,2)$keep)/length((rlnp(100,100,2)$keep))
#sum(rlnp(100,100,5)$keep)/length((rlnp(100,100,5)$keep))
#sum(rlnp(100,100,10)$keep)/length((rlnp(100,100,10)$keep))
# I tried 4 different scales 1,2,5,10 respectively for y equal to 100, and found that
# smaller scale corresponds to larger acceptance rate.


rlnpPlot <- function(dlnp.value = rlnp(n, y, scale)){
  curve(dlnp(x,dlnp.value$y), dlnp.value$x0-5, dlnp.value$x0+5, col = "red", ylab = "density")
  
  curve((dcauchy(x, dlnp.value$x0, dlnp.value$scale) * dlnp.value$M),
        dlnp.value$x0-5, dlnp.value$x0+5, 
        add = T)
  
  points(dlnp.value$x, dlnp.value$yunif*
           dcauchy(dlnp.value$x, location = dlnp.value$x0, 
                   scale = dlnp.value$scale)*dlnp.value$M, col = "red", pch = 16)
  
  points(dlnp.value$x[dlnp.value$keep==0], dlnp.value$yunif[dlnp.value$keep==0]*
           dcauchy(dlnp.value$x[dlnp.value$keep==0], location = dlnp.value$x0, 
                   scale = dlnp.value$scale)*dlnp.value$M, pch = 16)
  
  frac <- round(sum(dlnp.value$keep==0)/length(dlnp.value$keep),2)
  
  legend(dlnp.value$x0-5, dlnp(dlnp.value$x0,dlnp.value$y), 
         legend=c(paste("y","=",dlnp.value$y), 
                  paste("scale","=",dlnp.value$scale), paste("fraction","=",frac)))
}

#rlnpPlot(rlnp(100,0,1))
#rlnpPlot(rlnp(100,10,1))
#rlnpPlot(rlnp(100,100,1))


rlnpx <-  function(n=1, y=1, scale=1){
  input <- rlnp(n,y,scale)
  return(input$x[input$keep == TRUE])
}

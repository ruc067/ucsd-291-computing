dlnp <-
  function(x,y ,log = FALSE) {
    val <-
      dnorm( x, log = TRUE ) +
      dpois( y, exp( x ), log = TRUE)
    if (log) val else exp( val )
  }
curve(dlnp(x,2),-5,5)
grid()

#locator()

#By function locator, I guess the mode of the distribution is around 0.44

(M <- dlnp(0.44,2)/dcauchy(0.44,location = 0.44, scale = 1))

curve((dcauchy(x,0.44,1)*M),-5,5, add = T, col = "red")



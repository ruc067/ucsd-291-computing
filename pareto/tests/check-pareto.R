## check pareto functions
library( pareto )

# training points
d.pareto.train  <- dpareto( 3, 2 ,2)

p.pareto.train  <- ppareto( 3, 2 ,2)

q.pareto.train  <- qpareto(0.5, 2 ,2)

#test points calculated from density function
d.pareto.test  <- exp(log (2) + 2 * log(2) - (1 + 2 ) * log (3))

p.pareto.test  <- 1 - 2^2*3^(-2)

q.pareto.test  <- exp(log(2^2/(1-0.5))/2)

# test by function all.equal
stopifnot( all.equal( d.pareto.test, d.pareto.train ) )

stopifnot( all.equal( p.pareto.train, p.pareto.test ) )

stopifnot( all.equal( q.pareto.train, q.pareto.test ) )

#check whether we can get the x back using both ppareto and qpareto functions
stopifnot(all.equal( qpareto(ppareto(6:8, 1:4, 2:3), 1:4, 2:3), c(6:8,6)) )

#check whether log.p works or not, if so, we can get x back
stopifnot(all.equal( qpareto(ppareto(6, 1, 2, log.p = T), 1, 2, log.p = T), 6))

#check whether lower.tail works or not, if so, we can get x back
stopifnot(all.equal( qpareto(ppareto(6, 1, 2, lower.tail = FALSE), 1, 2, 
                             lower.tail = FALSE), 6))

#verify that rpareto approximates the quantiles of the Pareto distribution
stopifnot(all.equal( as.vector(round(quantile(rpareto(10000,2,2)))[1:4]), round(qpareto(c(0,0.25,0.5,0.75,1),2,2))[1:4]))






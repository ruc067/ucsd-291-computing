# generate alpha beta and x values
alpha = rep(3, 10)
beta = rep(4, 10)
set.seed(1)
x = sample(4:20,10, replace = F)

#get density values
num = beta*alpha^beta
den = x^(beta+1)
frac = paste(num,"/",den, sep = "")

Pareto = data.frame(alpha, beta, x, frac)
print(Pareto)
Pareto$frac = as.character(Pareto$frac)

dpareto1 = function(x=2,alpha=1,beta=1){
  if (length(x)!=1) {
    print("ERROR! Length of x is not 1")
  } else if(length(alpha)!=1){
    print("ERROR! Length of a is not 1")
  } else if(length(beta)!=1){
    print("ERROR! Length of  bis not 1")
  } else if (x <= alpha){
    dpareto1.density = 0
  } else{
    dpareto1.density = beta*alpha^beta/x^(beta+1)
  }
  return(dpareto1.density)
}
dpareto <-
  Vectorize( dpareto1, vectorize.args = c("x", "alpha", "beta"))

curve( dpareto( x, 1, 2 ), from = 0, to = 10 )

result = paste("all.equal function comparing two densities:",all.equal(num/den, dpareto(x,alpha,beta)))
print(result)

\name{rpareto}
\alias{rpareto}
\title{
Random number generator of pareto distrition
}
\description{
  it will generate random variable of pareto distrition. 
}
\usage{
rpareto(n = 1, alpha = 2, beta = 2)
}
\arguments{
  \item{n}{
   int n
}
  \item{alpha}{
   vector alpha
}
  \item{beta}{
   vector beta
}
}
\details{
  The function will generate random variable of pareto distrition. The RNG method used is Inverse CDF Method.
}
\references{
  \href{https://en.wikipedia.org/wiki/Pareto_distribution}{Wikipedia pareto distribution}
}
\examples{
  rpareto(2, alpha = 2, beta = 2)
  
  rpareto(1, alpha = -2, beta = 2)
}
\author{
 Ruifeng Chen
}
\keyword{ rpareto }% use one of  RShowDoc("KEYWORDS")

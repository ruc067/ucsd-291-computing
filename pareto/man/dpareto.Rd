\name{dpareto}
\alias{dpareto}
\title{
call rcpp file to calculate density of pareto distribution 
}
\description{
  it will call rcpp file dpareto.cpp to calculate density of pareto distribution 
}
\usage{
dpareto(x = 2, alpha = 1, beta = 1, log = FALSE)
}
\arguments{
  \item{x}{
   vector x
}
  \item{alpha}{
   vector alpha
}
  \item{beta}{
   vector beta
}
  \item{log}{
   whether take the log the the density or not
}
}
\references{
  \href{https://en.wikipedia.org/wiki/Pareto_distribution}{Wikipedia pareto distribution}
}
\author{
 Ruifeng Chen
}
\keyword{ dpareto }% use one of  RShowDoc("KEYWORDS")

\name{sfdat}
\alias{sfdat}
\docType{data}
\title{
This is simulated functional data set
}
\description{
This data set contains observations from 50 random curves, each evaluated at 10 random locations with noise. This data set was simulated using the package \code{sfdasim}. 
}
\usage{data(sfdat)}
\format{
  A data frame with 500 observations on the following 5 variables.
  \describe{
    \item{\code{ID}}{a numeric vector identifying observations from the same curve}
    \item{\code{locs.Var1}}{a numeric vector}
    \item{\code{locs.Var2}}{a numeric vector}
    \item{\code{Time}}{a numeric vector containing points on the 'time' axis where curves are evaluated}
    \item{\code{X}}{a numeric vector of observed values}
  }
}
\keyword{datasets}

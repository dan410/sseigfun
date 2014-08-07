\name{estimate_cov_function}
\alias{estimate_cov_function}
\title{Nonparametric estimate of the covariance function}
\usage{
  estimate_cov_function(dat, marginal.knots = NULL,
    n.marginal.knots = NULL)
}
\arguments{
  \item{dat}{data frame which must have columns \code{ID,
  Time, X}. The \code{ID} column is an integer indicator
  variable identifying a specific curve, \code{Time}
  contains the points on the "time" axis where the curve is
  observed, and \code{X} contains the observed values.}

  \item{marginal.knots}{vector of locations which will be
  used to create a knots locations for the covariance
  function. See details for how knot locations for the
  product space are constructed. If this is specified, then
  \code{n.marginal.knots} should not be specified.}

  \item{n.marginal.knots}{integer specifying the number of
  locations which will be used to create a knots locations
  for the covariance function. See details for how knot
  locations for the product space are constructed. If this
  is specified, then \code{marginal.knots} should not be
  specified.}
}
\description{
  Nonparametric estimate of the covariance function
}
\examples{
data(sfdat)
cov.fit <- estimate_cov_function(sfdat, n.marginal.knots=5)
}


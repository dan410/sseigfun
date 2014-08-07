\name{my.samp.cov2d}
\alias{my.samp.cov2d}
\title{compute sample covariance for functional data}
\usage{
  my.samp.cov2d(time, x, subject, wt, marginal.knots,
    n.marginal.knots, centered = TRUE, noDiag = TRUE)
}
\arguments{
  \item{time}{vector of points on the 'time' axis}

  \item{x}{vector of functional observations}

  \item{subject}{vector of integer IDs for each curve}

  \item{wt}{vector of weights. These weights are not used
  to calculate anything in this function. The wt argument
  here is necessary for 'bookeeping' purposes for
  covariance function estimation methods that utilize these
  weights.}

  \item{marginal.knots}{vector of knot locations on the
  'time' (i.e. marginal) axis.}

  \item{n.marginal.knots}{integer specifying the number of
  knot locations to use on the marginal domain}

  \item{centered}{logical. If FALSE a smoothing spline fit
  will be computed to estimate the mean functions, and this
  mean funciton will be subtracted from each curve.}

  \item{noDiag}{logical. If TRUE the diagonal elements of
  th estimated covariance matrix will be removed.}
}
\value{
  A data frame with the sample covariances. The knot
  locations are included at the end of the data frame.
}
\description{
  compute sample covariance for functional data
}
\details{
  These weights argument \code{wt} is not used to calculate
  anything in this function. The wt argument here is
  necessary for 'bookeeping' purposes for covariance
  function estimation methods that utilize these weights.
}


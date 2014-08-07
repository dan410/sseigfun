#' compute sample covariance for functional data
#' 
#' compute sample covariance for functional data
#'
#' @param time vector of points on the 'time' axis
#' @param x vector of functional observations
#' @param subject vector of integer IDs for each curve
#' @param wt vector of weights. These weights are not used to calculate anything in this function. The wt argument here is necessary for 'bookeeping' purposes for covariance function estimation methods that utilize these weights. 
#' @param marginal.knots vector of knot locations on the 'time' (i.e. marginal) axis.
#' @param n.marginal.knots integer specifying the number of knot locations to use on the marginal domain
#' @param centered logical. If FALSE a smoothing spline fit will be computed to estimate the mean functions, and this mean funciton will be subtracted from each curve.
#' @param noDiag logical. If TRUE the diagonal elements of th estimated covariance matrix will be removed. 
#' @return A data frame with the sample covariances. The knot locations are included at the end of the data frame. 
#' @details These weights argument \code{wt} is not used to calculate anything in this function. The wt argument here is necessary for 'bookeeping' purposes for covariance function estimation methods that utilize these weights. 
#' @export

my.samp.cov2d <-
function (time, x, subject, wt, marginal.knots, n.marginal.knots, centered = TRUE, noDiag = TRUE)
{
    if (!centered) {
        fit <- smooth.spline(time, x)
        x <- x - fitted(fit)
    }
    gg <- NULL
    for (zz in unique(subject)) {
        if (sum(subject == zz) > 1) {
            tt <- time[subject == zz]
            xx <- x[subject == zz]
            g <- expand.grid(time1 = tt, time2 = tt)
            scov <- xx %*% t(xx)
            if (noDiag)
                scov <- scov + diag(rep(Inf, length(xx)))
            g$z <- matrix(scov, ncol = 1)
            g$id <- zz # creates a column recording the subject id
            g$wt <- wt[subject == zz] # CAUTION: this only works if ID and wt are aligned properly
            gg <- rbind(gg, g[g$z < Inf, ])
        }
    }
    nobs <- nrow(gg)
    if(!is.null(marginal.knots) & !is.null(n.marginal.knots)){
    	stop('Both marginal.knots and n.marginal.knots are specified. Only one is allowed to be specified.')
    }
    if( !is.null(n.marginal.knots)){
      tt <- min(time) + (max(time) - min(time)) * (1:n.marginal.knots)/(n.marginal.knots +
        1)
      g <- expand.grid(time1 = tt, time2 = tt)
    # g <- gg[sample(dim(gg)[1], n.marginal.knots*n.marginal.knots),] # use for random knot locations
      g$z <- 0
      g$id <- 0
      g$wt <- 0
      gg <- rbind(gg, g)
    }
    if( !is.null(marginal.knots)){
      tt <- marginal.knots
      g <- expand.grid(time1 = tt, time2 = tt)
    # g <- gg[sample(dim(gg)[1], n.marginal.knots*n.marginal.knots),] # use for random knot locations
      g$z <- 0
      g$id <- 0
      g$wt <- 0
      gg <- rbind(gg, g)
    }
    return(gg)
}

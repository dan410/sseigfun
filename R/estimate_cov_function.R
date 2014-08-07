#' Nonparametric estimate of the covariance function
#'
#' 
#'
#' @param dat data frame which must have columns \code{ID, Time, X}. The \code{ID} column is an integer indicator variable identifying a specific curve, \code{Time} contains the points on the "time" axis where the curve is observed, and \code{X} contains the observed values. 
#' @param marginal.knots vector of locations which will be used to create a knots locations for the covariance function. See details for how knot locations for the product space are constructed. If this is specified, then \code{n.marginal.knots} should not be specified. 
#' @param n.marginal.knots integer specifying the number of locations which will be used to create a knots locations for the covariance function. See details for how knot locations for the product space are constructed. If this is specified, then \code{marginal.knots} should not be specified. 
#' @export 
#' @examples
#' data(sfdat)
#' cov.fit <- estimate_cov_function(sfdat, n.marginal.knots=5)

estimate_cov_function <- function(dat, marginal.knots = NULL, n.marginal.knots = NULL){ 
	
 if(is.null(dat$wt)){
  	dat$wt <- 1 #assign equal weights for each location
  } 
  
# compute sample covariance
samp.cov <- my.samp.cov2d(time = dat$Time,x = dat$X, subject = dat$ID, wt = dat$wt, marginal.knots = marginal.knots, n.marginal.knots = n.marginal.knots)  
  
  
  # define objects for observed locations, observed data, and knot locations
  if(is.null(n.marginal.knots)){n.marginal.knots <- length(marginal.knots) }
  nb <- n.marginal.knots*n.marginal.knots # number of knots on the product space
  nobs <- dim(samp.cov)[1] - nb
  knots <- samp.cov[(nobs+1):(nobs+nb),c(1,2)]  # plot(knots) to see knots locations
  points <- samp.cov[1:nobs,c("time1","time2")]
  y <- samp.cov[1:nobs,"z"]
  wt <- samp.cov[1:nobs, "wt"]
  
  s <- unpenalized.terms(points$time1, points$time2)
  r <- penalized.terms(points, knots)
  q <- penalized.matrix(knots)
  
  fit <- gss:::sspreg1( s = s, r = r, q = q, y = y, method = "v", alpha =1.4, varht=1, random = NULL, wt = wt)
  
  return(list(fit=fit, knots=knots, nbasis=n.marginal.knots, samp.cov=samp.cov))
}

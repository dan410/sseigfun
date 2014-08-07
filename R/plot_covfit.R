#' Plot fitted covariance function
#'
#' @param fit fitted covariance object
#' @param grid.resolution integer specifying the resolution grid points where the funciton will be evaluated
#' @param onlyPen if TRUE will plot only the portion of the covariance function estimated from penalized funcitons
#' @param onlyUnpen if TRUE will plot only the portion of the covariance function estimated from unpenalized funcitons 
#' @param image if TRUE will produce an image plot. By default the plost is a wireframe plot
#' ... additional arguments sent to plotting functions. See \code{image.plot} and \code{wireframe} for details on additional arguments
#' @export
#' @examples
#' data(sfdat)
#' covfit <- estimate_cov_function(sfdat, n.marginal.knots=10)
#' plot_covfit(covfit, image=TRUE)

plot_covfit <-
function(fit, grid.resolution = 40, onlyPen = FALSE, onlyUnpen = FALSE, image=FALSE, ...){
	
  if(onlyPen){fit$fit$d <- rep(0, length(fit$fit$d))} # plots only penalized portion 
  if(onlyUnpen){fit$fit$c <- rep(0, length(fit$fit$c))} # plots only the unpenalized portion
  tt <- seq(0,1, length =  grid.resolution)
  grid <- expand.grid(t1 = tt, t2 = tt)
  surface <- mapply(cov.fn, x = grid[,1], y=grid[,2], MoreArgs = list(knots=fit$knots, fit.obj=fit))
  
  if(image == TRUE){
  	z <- matrix(surface, nrow=length(tt), byrow=TRUE)
  	x <- tt
  	y <- tt
  	image.plot(x=x, y=y, z=z, ...)
  }
  if(image == FALSE){
  	wireframe(surface ~ grid[,1]*grid[,2], drape=TRUE, pretty=TRUE, scales=list(arrows=FALSE), xlab='', ylab='',zlab='', ...)
  }
}

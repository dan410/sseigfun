#' extract principal component function from a fitted object
#' 
#' extract principal principal component funciton from a fitted object.
#' 
#' @param nharm integer corresponding to the principal component function. nharm = 1 corresponds to the leadng principal component function.
#' @param fit.obj fitted object from output of estimate_eigenfunctions() from which PCFs will be derived. 
#' @return the principal component function corresponding to nharm
extract_pcf <- function(nharm, fit.obj){
    pcf <- fit.obj$fns[[order(fit.obj$values, decreasing=TRUE)[nharm] ]]
  return(pcf)
}
penalized.terms <-
function(points, knots) {
  ### create matrix of basis functions evaluated at data points ###
  nobs <- dim(points)[1]
  nb <- dim(knots)[1]
  index1 <- rep(1:nobs,  nb)
  index2 <- rep(1:nb, each=nobs)
  vals <- cbind(points[index1, ], knots[index2,])
  R <- mapply(tprk, s1 = vals[,1], t1 = vals[,2], s2 = vals[,3], t2 = vals[,4])
  #R <- mapply(tprk,  vals[,1], vals[,2],  vals[,3],  vals[,4])
  R <- matrix(R, nrow = nobs, byrow=FALSE)
  return(R)
}

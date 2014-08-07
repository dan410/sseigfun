penalized.matrix <-
function(knots) {
  ### create matrix of basis functions evaluated at data points ###
  nb <- dim(knots)[1]
  index1 <- rep(1:nb, each= nb)
  index2 <- rep(1:nb, nb)
  vals <- cbind(knots[index1, ], knots[index2,])
  R <- mapply(tprk,  s1 = vals[,1], t1 = vals[,2], s2 =  vals[,3],  t2 = vals[,4])
  R <- matrix(R, nrow = nb, byrow=TRUE)
  return(R)
}

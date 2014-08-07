cov.fn <-
function(x,y, knots, fit.obj){
  c <- fit.obj$fit$c
  d <- fit.obj$fit$d
  theta <- fit.obj$fit$theta
  nb <- dim(knots)[1]
  R <- mapply(tprk, s1 = rep(x, nb), s2 = knots[,1], t1 = rep(y,nb), t2 = knots[,2])
  R <- 10^theta*R     #multiplying by 10^theta is needed because of how 'sspreg1' computes fitted values
  res1 <- as.vector(c)%*%as.vector(R)              # contribution from the penalized terms
  res2 <- as.vector(d)%*%t(unpenalized.terms(x,y)) # contribution from the unpenalized terms
  res <- res1 + res2
  return(res)
}

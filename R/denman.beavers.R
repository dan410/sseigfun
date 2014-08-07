denman.beavers <-
function(mat,maxit=50) {
  stopifnot(nrow(mat) == ncol(mat))
  niter <- 0
  y <- mat
  z <- diag(rep(1,nrow(mat)))
  for (niter in 1:maxit) {
    y.temp <- 0.5*(y+solve(z))
    z <- 0.5*(z+solve(y))
    y <- y.temp
  }
  return(list(sqrt=y,sqrt.inv=z))
}

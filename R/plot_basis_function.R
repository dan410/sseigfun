
plot_basis_function <-
function( loc , grid.resolution=40, zlim=c(-1,1)){
 tt <- seq(0,1, length =  grid.resolution)
 grid <- expand.grid(t1 = tt, t2 = tt)
 loc <- matrix(loc, nrow = grid.resolution^2, ncol = 2, byrow=TRUE)
 surface <- mapply(tprk, s1 = grid[,1], s2=loc[,1], t1 = grid[,2], t2 = loc[,2])
 #surface <- 10^fit$theta*surface
 wireframe(surface ~ grid[,1]*grid[,2], zlim=zlim, drape=TRUE, pretty=TRUE,scales=list(arrows=FALSE), xlab='', ylab='',zlab='')
}

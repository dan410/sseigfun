#' estimates eigenfunctions using fitted covariance function
#'
#'
#'
#' @param cov.fit fitted covariance function. The structure should match the output of \code{estimate_cov_function}
#' @return list of the eigenfunctions and a vector of the corresponding eigenvalues.
#' @export
#' @examples
#' data(sfdat)
#' cov.fit <- estimate_cov_function(sfdat, n.marginal.knots=5)
#' eig.fit <- estimate_eigenfunctions(cov.fit)
#' eig.fns <- eig.fit$fns
#' ef1 <- eig.fns[[1]]
#' curve(ef1)

estimate_eigenfunctions <- function(cov.fit){

  fit <- cov.fit$fit
  tp.knots <- cov.fit$knots
  num.marginal.knots <- sqrt(nrow(tp.knots))

  # here we want the knot location used on the univariate space
  knots <- tp.knots[1:num.marginal.knots,1]
  
  # create the vector-valued funciton g = g(b1(x), b2(x), R1(x,t1),...,R1(x,tk))
  g <- create.g(knots)
  
  # create the "A" matrix. This will be done in 4 parts:
  # A.ul is the upper left portion of the partitioned matrix
  # A.ur is the upper right portion
  # A.ll is the lower left portion
  # A.lr is the lower right portion
  
  d.coef <- fit$d       # coefficients for the basis terms in the unpenalized space
  d.mat  <- matrix(d.coef, nrow=2, byrow=TRUE)
  c.coef <- fit$c       # coefficients for the basis terms in the penalized space
  c.mat  <- matrix(c.coef, nrow=length(knots), byrow=TRUE)
  
  A.ul <- d.mat
  A.ul <- (1/2)*(d.mat + t(d.mat))
  A.lr <- c.mat
  A.lr <- (1/2)*(A.lr+t(A.lr))
  
  A.ll <- matrix(0, nrow=length(knots), ncol=2)
  for(i in 1:length(knots)){
    for(j in 1:2){
      A.ll[i,j] <- c.mat[,i]%*%g[[j]](knots)
    }
  }
  
  A.ur <- matrix(0, ncol=length(knots), nrow=2)
  for(i in 1:2){
    for(j in 1:length(knots)){
      A.ur[i,j] <- c.mat[j,]%*%g[[i]](knots)
    }
  }
  A.ur <- t(A.ll)
  
  A <- cbind(rbind(A.ul, 10^fit$theta*A.ll), rbind(10^fit$theta*A.ur, 10^fit$theta*A.lr))
  
  
  #Create the "Q" matrix used in the eigen decomposition. To do this
  #you need to integrate products of the univariate basis functions.
  
  # first  compute the upper left portion of the matrix which does not involve the R1 function
  nrow <- length(knots)+2              # the "+2" is for the 2 basis funcitons on the unpenalized space
  ncol <- length(knots)+2
  s <- c(1,2,rep(3,length(knots)))     # both "s" and "ts" help with indexing
  ts <- c(1,1, knots)                  # the first two numbers in "ts" are just fillers
  Q <- matrix(0, nrow=nrow, ncol=ncol)
  for(i in 1:nrow){
    for(j in 1:ncol){
      #f <- function(x){basis.fns[[s[i]]](x, s2=ts[i])*basis.fns[[s[j]]](x, s2=ts[j])}
      f <- function(x){g[[i]](x)*g[[j]](x)}
      Q[i,j] <- integrate(f, 0,1)$value
    }
    
  }
  
  # compute the principle component functions
  # QQ <- denman.beavers(Q)    #computes square root matrix and inverse square root matrix
  
  # Q.sqrt <- QQ$sqrt
  # Q.sqrt <- (1/2)*(Q.sqrt + t(Q.sqrt))
  # Q.sqrt.inv <- QQ$sqrt.inv
  
  Q.eig <- eigen(Q)
  evals <- Q.eig$values
  evals[evals < 0] <- 0
  Q.sqrt <- Q.eig$vectors %*% diag(sqrt(evals)) %*%solve(Q.eig$vectors)
  Q.sqrt.inv <- solve(Q.sqrt)

  
  M <- Q.sqrt%*%A%*%Q.sqrt
  eM <- eigen(M)
  vals <- eM$values
  V <- eM$vectors
  U <- Q.sqrt.inv%*%V
  
  # create list of eigenfunctions
  nfuns <- ncol(U) 	# number of eigenfunctions
  b <- list()       # convert column of U into a list to use in llply
  for(i in 1:nfuns){b[[i]] <- U[,i]}
  
  funs <- llply(b, function(bb){force(bb); function(s, b=bb){
  		X <- ldply(g, function(f){f(s)}) # evaluate the vector valued g at s
    	X$.id <- NULL # delete the .id column  created by ldply
    	as.numeric(t(b)%*%as.matrix(X))
  }})
  
 
  return(list(values = vals, fns=funs))
}

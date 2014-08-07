create.g <-
function(knots=knots)   {
  k <- length(knots)
  b1 <- function(s1, s2=NULL){ rep(1, length(s1))}
  b2 <- function(s1, s2=NULL){ k1(s1) }
  unpenalized.basis.fns <- list(b1 = b1, b2 = b2) 
  
  R1 <- function(s1, s2){
    res <- k2(s1)*k2(s2) - k4(abs(s1-s2))
  }

  penalized.basis.fns <- llply(as.list(knots), function(x){force(x); function(s,t=x){R1(s,t)}})
  g <- c(unpenalized.basis.fns, penalized.basis.fns)

  return(g)
}

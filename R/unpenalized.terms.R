unpenalized.terms <-
function(x,y){
  # minx <- -0.04397622
  # maxx <- 1.048224
  # x <- (x - minx)/(maxx - minx)
  # y <- (y - minx)/(maxx - minx)
  
   n <- length(x)
   S <- matrix(0, nrow = n, ncol = 4)
   S[,1] <- 1
   S[,2] <- k1(x)
   S[,3] <- k1(y)
   S[,4] <- S[,2]*S[,3]
   return(S)
}

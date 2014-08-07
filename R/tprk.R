tprk <-
function(s1, s2, t1, t2){  # reproducing kernal on the penalized space
#===============================================================================
# ARGUMENTS:
# inputs should be given to compute K(s1,s2)*K(t1,t2)
# VALUES:
# the function K(s1,s2)*K(t1,t2) evaluated at the inputs
#===============================================================================

# the following could be used if values are not on [0,1]
# minx <- -0.04397622
# maxx <- 1.048224
#  s1 <- (s1 - minx)/(maxx - minx)
#  t1 <- (t1 - minx)/(maxx - minx)
#  s2 <- (s2 - minx)/(maxx - minx)
# t2 <- (t2 - minx)/(maxx - minx)

  k2(t1)*k2(t2) - k4(abs(t1-t2))+
  k1(s1)*k1(s2)*(k2(t1)*k2(t2)-k4(abs(t1-t2)))+
  k2(s1)*k2(s2) - k4(abs(s1 - s2))+
  (k2(s1)*k2(s2) - k4(abs(s1 - s2)))*(k1(t1)*k1(t2))+
  (k2(s1)*k2(s2) - k4(abs(s1 - s2)))*(k2(t1)*k2(t2) - k4(abs(t1 - t2)))
}

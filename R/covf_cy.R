


# this is the covariance function used in Cai and Yuan 2010.
covf_cy <- function(s,t, alpha=2){
	res <- mapply(eval_covf_cy, s=s, t=t, alpha=alpha)
}

eval_covf_cy <- function(s,t, alpha){
	k <- 1:50
		res <- sum(k^(-1*2*alpha)*cos(k*pi*s)*cos(k*pi*t))	
	return(res)
}


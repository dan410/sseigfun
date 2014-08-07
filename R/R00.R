R00 <-
function(s1, s2){
    res <- rep(1, max(length(s1), length(s2)))    #this allows for handling of vector arguments
    return(res)
}

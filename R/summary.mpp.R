summary.mpp <- function(object, ...){
    object$data <- NULL
    if (class(object$gif)=="function") object$gif <- NULL
    return(object)
}


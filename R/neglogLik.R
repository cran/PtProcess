neglogLik <- function(params, object, pmap){
    #   have an arg to subset parameters
    object <- pmap(object, params)
    x <- -logLik(object)
    if (is.infinite(x) | is.na(x)) return(1e15)
    else return(x)
}


neglogLik <- function(params, object, updatep){
    #   have an arg to subset parameters
    object <- updatep(object, params)
    x <- -logLik(object)
    if (is.infinite(x) | is.na(x)) return(1e15)
    else return(x)
}


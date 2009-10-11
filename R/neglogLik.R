neglogLik <- function(params, object, pmap, SNOWcluster=NULL){
    #   have an arg to subset parameters
    object <- pmap(object, params)
    x <- -logLik(object, SNOWcluster)
    if (is.infinite(x) | is.na(x)) return(1e15)
    else return(x)
}


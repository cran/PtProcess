logLik.mpp <- function(object, ...){
    params <- object$params
    # pp.LL(object$data, object$gif, eval(object$gmap), object$TT)
    data <- object$data
    cif <- object$gif
    gparams <- eval(object$gmap)
    mparams <- eval(object$mmap)
    TT <- object$TT
    use <- (data[, "time"] <= TT[2]) & (data[, "time"] >= TT[1])
    L1 <- sum(log(cif(data, data[use, ], gparams)))
    L2 <- cif(data, NULL, gparams, TT = TT)
    dmark <- object$mark[[1]]
    if (is.null(dmark))
        L3 <- 0
    else
        L3 <- sum(dmark(data, data, mparams))
    LL <- L1 - sum(L2) + L3
    return(LL)
}


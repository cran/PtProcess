simulate.mpp <-
function (object, nsim = 1, seed = NULL, max.rate = NA, stop.condition = NULL, 
    ...) 
{
    data <- object$data
    params <- object$params
    gparams <- eval(object$gmap)
    mparams <- eval(object$mmap)
    gif <- object$gif
    TT <- object$TT
    if (!is.null(data)) {
        use <- (data[, "time"] <= TT[1])
        if (sum(use) == 0) 
            data <- NULL
        else data <- data[use, ]
    }
    if (!is.null(seed)) 
        set.seed(seed)
    ti <- TT[1]
    repeat {
        if (is.null(attr(gif, "rate"))) 
            stop("rate attribute is not specified on gif")
        else if (attr(gif, "rate") == "decreasing") {
            Rmax <- gif(data = data, evalpts = ti, params = gparams, 
                tplus = TRUE)
            tau <- rexp(1, rate = Rmax)
            rate <- gif(data = data, evalpts = ti + tau, params = gparams)
            if (rate > Rmax) 
                stop("gif is not decreasing")
        }
        else if (attr(gif, "rate") == "bounded") {
            if (is.na(max.rate)) 
                stop("Argument max.rate must be specified when gif has rate \"bounded\"")
            Rmax <- max.rate
            tau <- rexp(1, rate = Rmax)
            rate <- gif(data = data, evalpts = ti + tau, params = gparams)
            if (rate > Rmax) 
                stop("gif is not bounded by max.rate")
        }
        else if (attr(gif, "rate") == "increasing") {
            Rmax <- 0
            rate <- gif(data = data, evalpts = ti, params = gparams, 
                tplus = TRUE)
            tmax <- ti
            while (rate > Rmax) {
                ti <- tmax
                tmax <- ti + qexp(0.7, rate = rate)
                Rmax <- gif(data = data, evalpts = tmax, params = gparams)
                tau <- rexp(1, rate = Rmax)
                rate <- gif(data = data, evalpts = ti + tau, 
                  params = gparams)
            }
        }
        else stop("unknown value of rate attribute on gif")
        ti <- ti + tau
        if (ti > TT[2]) 
            break
        if (runif(1, 0, 1) <= rate/Rmax) {
            newevent <- list()
            newevent$time <- ti
            if (!is.null(object$marks[[2]])) newevent <- c(newevent,
                object$marks[[2]](ti, data, mparams))
            else newevent <- c(newevent, NA)
            newevent <- as.data.frame(newevent)
            data <- rbind(data[, names(newevent)], newevent)
            if (!is.null(stop.condition)) 
                if (stop.condition(data)) 
                  break
        }
    }
    if (is.null(object$marks[[2]])) data[,"NA."] <- NULL
    object$data <- data
    return(object)
}



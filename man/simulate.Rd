\name{simulate}
\alias{simulate}
\alias{simulate.mpp}
\alias{simulate.linksrm}

\title{Simulate a Point Process}

\description{
Provides methods for the generic function \code{\link[stats]{simulate}}.
}

\usage{
\method{simulate}{mpp}(object, nsim=1, seed=NULL, max.rate=NA,
         stop.condition=NULL, ...)
\method{simulate}{linksrm}(object, nsim=1, seed=NULL, max.rate=NA,
         stop.condition=NULL, ...)
}

\arguments{
  \item{object}{an object with class \code{"\link{mpp}"} or \code{"\link{linksrm}"}.}
  \item{nsim}{has no effect, and is only included for compatibility with the generic function \code{\link[stats]{simulate}}. See section \dQuote{Length of Simulated Series} below for control information.}
  \item{seed}{seed for the random number generator.}
  \item{max.rate}{maximum rate, only used if the attribute of \code{object$gif} is \code{"bounded"}. It is the maximum value of \code{object$gif} on the simulation interval \code{object$TT}.}
  \item{stop.condition}{a function returning a logical value. It is called after the addition of each simulated event. The simulation continues until either \code{object$TT[2]} is exceeded or \code{stopping.condition} returns \code{TRUE}. See section \dQuote{Length of Simulated Series} below for further information.}
  \item{\dots}{other arguments.}
}

\details{
The \emph{thinning method} (Ogata, 1981; Lewis & Shedler, 1979) is used to simulate a point process with specified ground intensity function. The method involves calculating an upper bound for the intensity function, simulating a value for the time to the next \emph{possible} event using a rate equal to this upper bound, and then calculating the intensity at this simulated point; hence these \dQuote{events} are simulated too frequently. The ratio of this rate with the upper bound is compared with a uniform random number to randomly determine whether the simulated time is retained or not (i.e. thinned).

The functions need to calculate an upper bound for the intensity function. The ground intensity functions will usually be discontinuous at event times, but may be monotonically increasing or decreasing at other times. The ground intensity functions have an attribute called \code{rate} with values of \code{"bounded"}, \code{"increasing"} or \code{"decreasing"}. This information is used to determine the required upper bounded.

The function \code{simulate.linksrm} is currently only used in conjunction with \code{\link{linksrm_gif}}, or a variation of that function. It expects the \code{gif} function to have an attribute called \code{regions}, which may be an expression, being the number of regions. The method used by the function \code{simulate.linksrm} also assumes that the function is \dQuote{increasing} (i.e. rate, summed over all regions, apart from discontinuous jumps), hence a positive tectonic input over the whole system.
}

\section{Length of Simulated Series}{
The interval of time over which events are simulated is determined by \code{object$TT}. Simulation starts at \code{object$TT[1]} and stops at \code{object$TT[2]}. The \dQuote{current} dataset will consist of all events prior to \code{object$TT[1]} in \code{object}, plus subsequently simulated events. A more complicated stopping condition can be formulated by using the argument \code{stop.condition}.

The argument \code{stop.condition} can be assigned a function that returns a logical value. The assigned function is a function of the \dQuote{current} dataset. It is executed near the bottom of \code{simulate.mpp} (check by printing the function). Simulation will then continue until either the stopping condition has been met or the current time exceeds \code{object$TT[2]}.

For example, we may want to simulate until the first earthquake with a magnitude of 8. Assume that the current dataset contains a variable with name \code{"magnitude"} (untransformed). We would then assign \code{Inf} to \code{object$TT[2]}, and write this condition as a function:
\preformatted{
    stop.cond <- function(data){
        n <- nrow(data)
        #   most recent event is the nth
        return(data$magnitude[n] >= 8)
    }
}
}

\value{
The returned value is an object of the same class as \code{object}. It will contain all events prior to \code{object$TT[1]} in \code{object$data} and all subsequently simulated events. Variables (columns) in \code{object$data} will be restricted to \code{"time"} and those for which a mark is simulated.
}

\references{
Cited references are listed on the \link{PtProcess} manual page.
}

\examples{
TT <- c(0, 1000)
bvalue <- 1
params <- c(-2.5, 0.01, 0.8, bvalue*log(10))

x <- mpp(data=NULL,
         gif=srm_gif,
         marks=list(NULL, rexp_mark),
         params=params,
         gmap=expression(params[1:3]),
         mmap=expression(params[4]),
         TT=TT)
x <- simulate(x, seed=5)

y <- hist(x$data$magnitude, xlab="Magnitude", main="")

#   overlay with an exponential density
magn <- seq(0, 3, length.out=100)
points(magn, nrow(x$data)*(y$breaks[2]-y$breaks[1])*
             dexp(magn, rate=1/mean(x$data$magnitude)),
       col="red", type="l")
}

\keyword{methods}
\keyword{datagen}

\name{simulate}
\alias{simulate}
\alias{simulate.mpp}
\alias{simulate.linksrm}

\title{Simulate a Point Process}

\description{
Provides methods for the generic function \code{\link[stats]{simulate}}.
}

\usage{
\method{simulate}{mpp}(object, nsim = 1, seed = NULL, max.rate = NA, ...)
\method{simulate}{linksrm}(object, nsim = 1, seed = NULL, max.rate = NA, ...)
}

\arguments{
  \item{object}{an object with class \code{"\link{mpp}"} or \code{"\link{linksrm}"}.}
  \item{nsim}{number of events to simulate.}
  \item{seed}{seed for the random number generator.}
  \item{max.rate}{max rate.}
  \item{\dots}{other arguments.}
}

\details{
The \emph{thinning method} (Ogata, 1981) is used to simulate a point process with specified ground intensity function. The method involves calculating an upper bound for the intensity function, simulating a value for the time to the next \emph{possible} event using a rate equal to this upper bound, and then calculating the intensity at this simulated point; hence these \dQuote{events} are simulated too frequently. The ratio of this rate with the upper bound is compared with a uniform random number to randomly determine whether the simulated time is retained or not (i.e. thinned).

The functions need to calculate an upper bound for the intensity function. The ground intensity functions will usually be discontinuous at event times, but may be monotonically increasing or decreasing at other times. The ground intensity functions have an attribute called \code{rate} with values of \code{"bounded"}, \code{"increasing"} or \code{"decreasing"}. This information is used to determine the required upper bounded.

The function \code{simulate.linksrm} is currently only used in conjunction with \code{\link{linksrm_gif}}, or a variation of that function. It expects the \code{gif} function to have an attribute called \code{regions}, which may be an expression, being the number of regions. The method used by the function \code{simulate.linksrm} also assumes that the function is \dQuote{increasing} (i.e. rate, summed over all regions, apart from discontinuous jumps), hence a positive tectonic input over the whole system.
}

\examples{
TT <- c(0, 1000)
bvalue <- 1
params <- c(-2.5, 0.01, 0.8, bvalue*log(10))

x <- mpp(data=NULL,
         gif=srm_gif,
         mark=list(NULL, rexp_mark),
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

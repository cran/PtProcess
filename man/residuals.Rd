\name{residuals}
\alias{residuals}
\alias{residuals.mpp}
\alias{residuals.linksrm}

\title{Residuals of a Point Process Model}

\description{
Provides methods for the generic function \code{\link[stats]{residuals}}.
}

\usage{
\method{residuals}{mpp}(object, ...)
\method{residuals}{linksrm}(object, ...)
}

\arguments{
  \item{object}{an object with class \code{\link{mpp}} or \code{\link{linksrm}}.}
  \item{...}{other arguments.}
}

\value{
Returns a time series object with class "\code{\link[stats]{ts}}" in the case of \code{\link{mpp}}. In the case of \code{\link{linksrm}} a list is returned with the number of components being equal to the number of regions, and with each component being a time series object.
}

\details{
Let \eqn{t_i}{ti} be the times of the observed events. Then the transformed times are defined as
\deqn{
\tau_i = \int_0^{t_i} \lambda_g(t|{\cal H}_t) dt.
}{
tau_i = integral_0^{ti} of {lambda_g(t|Ht) dt}.
}
If the proposed point process model is correct, then the transformed time points will form a stationary Poisson process with rate parameter one. A plot of transformed time points versus the cumulative number of events should then roughly follow the straight line \eqn{y = x}. Significant departures from this line indicate a weakness in the model. Further details can be found in Ogata (1988) and Aalen & Hoem (1978).

See Baddeley et al (2005) and Zhuang (2006) for extensions of these methodologies.
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

tau <- residuals(x)

plot(tau, ylab="Transformed Time", xlab="Event Number")
abline(a=0, b=1, lty=2, col="red")

#   represent as a cusum
plot(tau - 1:length(tau), ylab="Cusum of Transformed Time", xlab="Event Number")
abline(h=0, lty=2, col="red")
}

\keyword{methods}


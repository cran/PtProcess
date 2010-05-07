\name{plot}
\alias{plot}
\alias{plot.mpp}
\alias{plot.linksrm}

\title{Plot Point Process Ground Intensity Function}
\description{
Provides methods for the generic function \code{\link[graphics]{plot}}.
}

\usage{
\method{plot}{mpp}(x, log=FALSE, ...)
\method{plot}{linksrm}(x, region, log=FALSE, ...)
}

\arguments{
  \item{x}{an object with class \code{"\link{mpp}"} or \code{"\link{linksrm}"}.}
  \item{region}{scalar, specifies the required region.}
  \item{log}{plot \eqn{\log \lambda_g(t|{\cal H}_t)}{log lambda_g(t|Ht)}, default is \code{FALSE}.}
  \item{\dots}{other arguments.}
}

\examples{
data(Ogata)

p <- c(0.02, 70.77, 0.47, 0.002, 1.25)
TT <- c(0, 800)
bvalue <- 1

#   Note that the plot function does not utilise the
#   information about mark distributions, hence these
#   arguments can be NULL

x <- mpp(data=Ogata,
         gif=etas_gif,
         marks=list(NULL, NULL),
         params=p,
         gmap=expression(params[1:5]),
         mmap=NULL,
         TT=TT)

plot(x, log=TRUE)
}

\keyword{methods}


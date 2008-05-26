\name{summary}
\alias{summary}
\alias{summary.mpp}
\alias{summary.linksrm}

\title{Summary of a Point Process Model}

\description{
Provides methods for the generic function \code{\link[base]{summary}}.
}

\usage{
\method{summary}{mpp}(object, ...)
\method{summary}{linksrm}(object, ...)
}

\arguments{
  \item{object}{an object with class \code{"\link{mpp}"} or \code{"\link{linksrm}"}.}
  \item{...}{other arguments.}
}

\value{
A list object with a reduced number of components, mainly the parameter values.
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

print(summary(x))
}

\keyword{methods}


\name{makeSOCKcluster}
\alias{makeSOCKcluster}

\title{Parallel Processing: Transition Functions}

\description{
Package \pkg{snow} has become deprecated and replaced by \pkg{parallel}. Some functions in \pkg{snow} used by package \pkg{PtProcess} do not appear in \pkg{parallel} under the same name. Below are transition functions to map some functions in \pkg{snow} to the most comparable functions in \pkg{parallel}. These transition functions will ultimately be deprecated.
}

\usage{
makeSOCKcluster(names, ...)
}

\arguments{
  \item{names}{character vector of node names.}
  \item{...}{cluster option specifications.}
}

\details{
\code{\link{makeSOCKcluster}} calls \code{\link[parallel]{makePSOCKcluster}}.
}

\keyword{programming}
\keyword{iteration}


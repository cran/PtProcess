\name{marks}
\alias{marks}
\alias{dexp_mark}
\alias{rexp_mark}

\title{Mark Distributions}
\description{
Contains densities and random number generators for some example mark distributions. The mark distributions can be multi-dimensional. Users can write their own functions, and general rules are given under \dQuote{Details}.
}

\usage{
dexp_mark(x, data, params)
rexp_mark(ti, data, params)
}

\arguments{
  \item{ti}{scalar, time of an event.}
  \item{x}{a \code{\link{data.frame}} of mark values at given times, often a subset of the history.}
  \item{data}{a \code{\link{data.frame}} containing the history of the process, denoted below as \eqn{ {\cal H}_t}{Ht}.}
  \item{params}{numeric vector of parameters.}
}

\details{
The functions listed under \dQuote{Usage} calculate the (mark) density and simulate earthquake magnitudes assuming an exponential distribution that is independent of the history of the process. This corresponds to the Gutenberg-Richter law.

All mark densities and random number generators have three arguments as shown in the examples above. Multi-parameter distributions have their parameters specified as a vector in the \code{params} argument. Other ancillary data or information can be passed into the function non formally, though one needs to be careful with names of other objects.

In the \dQuote{Examples} below, the function \code{dmagn_mark} is an example where the density of the magnitude distribution is dependent on the value of the ground intensity function (\code{etas_gif}), and in this case, the history of the process. This function assumes that after large events, there is a deficit of smaller magnitude events with more larger magnitude events. It has seven parameters with parameters \eqn{p_1, \cdots, p_5}{p_1, ..., p_5} relating to \code{etas_gif}. It assumes that the magnitude distribution is gamma (\code{\link{GammaDist}}), with a shape parameter given by
\deqn{
\mbox{shape} = 1 + \sqrt{\lambda_g(t|{\cal H}_t)} \ p_7 \,,
}{
shape = 1 + \sqrt{lambda_g(t|Ht)} * p_7 ,
}
where \eqn{p_7} (\eqn{p_7 > 0}) is a free estimable parameter, and parameter \eqn{p_6} is the scale parameter. Hence when \eqn{\lambda_g(t|{\cal H}_t)}{lambda_g(t|Ht)} is small, the magnitude distribution returns to the exponential distribution with an approximate rate of \eqn{p_6} (i.e. Gutenberg Richter law).
}

\value{
Mark density functions must return a vector with length being equal to the number of rows in \code{x}. Each element contains the joint density of the marks corresponding to each time (row) in \code{x}.

The random number generator simulates each mark for a \emph{single value} of \code{ti}. It must return a \code{\link{list}} of simulated marks corresponding to the specified time \code{ti}. Further, the list must have its elements named, e.g. \code{"magnitude"}, \code{"longitude"}, \code{"latitude"}, etc. Note that each component in the list will be of length one. A list is used (rather than a vector) because it allows marks to be character as well as numeric. For example, one mark might be the region name. See function \code{rexample_mark} in \dQuote{Examples} below.
}

\examples{
#   Below is an example of a 3-D mark distribution. Each component
#   is independent of each other and history, hence the arguments
#   ti and data do not occur in the function.

rexample_mark <- function(ti, data, params)
    return(list(magnitude=rexp(n=1, params[1]),
                longitude=rnorm(1, mean=params[2]),
                latitude=rnorm(1, mean=params[3])))

#-----------------------------------------------------------------
#   Below is an example of a density for magnitudes of events
#   in and etas model but where the magnitude density is
#   dependent on the value of the ground intensity function
#   and hence the history of the process (See Details above for
#   more explanation).

dmagn_mark <- function(x, data, params){
    lambda <- etas_gif(data, x[,"time"], params=params[1:5])
    y <- dgamma(x[,"magnitude"], shape=1+sqrt(lambda)*params[7],
                rate=params[6], log=TRUE)
    return(y)
}
}

\keyword{distribution}

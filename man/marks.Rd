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
The example functions listed under \dQuote{Usage} calculate the \emph{logarithm} of the (mark) density and simulate earthquake magnitudes assuming an exponential distribution that is independent of the history of the process. This corresponds to the Gutenberg-Richter law. They assume that the history contains a variable named \code{"magnitude"}.

All mark densities and random number generators must have the three arguments as shown in the examples above. Multi-parameter distributions have their parameters specified as a vector in the \code{params} argument. Other ancillary data or information can be passed into the function non formally, though one needs to be careful about possible conflict with names of other objects.
}

\value{
Mark density functions must return a vector with length being equal to the number of rows in \code{x}. Each element contains the \emph{logarithm} of the joint density of the marks corresponding to each time (row) in \code{x}.

The random number generator simulates each mark for a \emph{single value} of \code{ti}. It must return a \code{\link{list}} of simulated marks corresponding to the specified time \code{ti}. Further, the list must have its elements named the same as those in the history. Note that each component in the list will be of length one. A list is used (rather than a vector) because it allows marks to be character as well as numeric.
}

\section{Example 1}{
This is an example where the density of the magnitude distribution is dependent on the value of the ground intensity function (assumed to be \code{etas_gif}), and in this case, the history of the process. The history is assumed to contain a variable named \code{"magnitude"}. In this mark distribution, it is assumed that after large events, there is a deficit of smaller magnitude events with more larger magnitude events. It has seven parameters with parameters \eqn{p_1, \cdots, p_5}{p_1, ..., p_5} relating to \code{etas_gif}. It assumes that the magnitude distribution is gamma (\code{\link{GammaDist}}), with a shape parameter given by
\deqn{
\mbox{shape} = 1 + \sqrt{\lambda_g(t|{\cal H}_t)} \ p_7 \,,
}{
shape = 1 + \sqrt{lambda_g(t|Ht)} * p_7 ,
}
where \eqn{p_7} (\eqn{p_7 > 0}) is a free estimable parameter, and parameter \eqn{p_6} is the scale parameter. Hence when \eqn{\lambda_g(t|{\cal H}_t)}{lambda_g(t|Ht)} is small, the magnitude distribution returns to an approximate exponential distribution with an approximate rate of \eqn{p_6} (i.e. Gutenberg Richter law).
\preformatted{
    dexample1_mark <- function(x, data, params){
        lambda <- etas_gif(data, x[,"time"], params=params[1:5])
        y <- dgamma(x[,"magnitude"], rate=params[6], 
                    shape=1+sqrt(lambda)*params[7], log=TRUE)
        return(y)
    }

    rexample1_mark <- function(ti, data, params){
        #  Gamma distribution
        #  exponential density when params[7]=0
        lambda <- etas_gif(data, ti, params=params[1:5])
        y <- rgamma(1, shape=1+sqrt(lambda)*params[7],
                    rate=params[6])
        return(list(magnitude=y))
    }
}
}

\section{Example 2}{
This an example of a 3-D mark distribution. Each component is independent of each other and the history, hence the arguments \code{ti} and \code{data} are not utilised in the functions. The history is assumed to contain the three variables \code{"magnitude"},  \code{"longitude"} and \code{"latitude"}. The event magnitudes are assumed to have an exponential distribution with rate \code{params[1]}, and the longitudes and latitudes to have normal distributions with means \code{params[2]} and \code{params[3]}, respectively.
\preformatted{
    dexample2_mark <- function(x, data, params)
        return(dexp(x[,"magnitude"], rate=params[1], log=TRUE) +
               dnorm(x[,"longitude"], mean=params[2], log=TRUE) +
               dnorm(x[,"latitude"], mean=params[3], log=TRUE))

    rexample2_mark <- function(ti, data, params)
        return(list(magnitude=rexp(1, rate=params[1]),
                    longitude=rnorm(1, mean=params[2]),
                    latitude=rnorm(1, mean=params[3])))
}
}

\keyword{distribution}

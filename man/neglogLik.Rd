\name{neglogLik}
\alias{neglogLik}

\title{Negative Log-Likelihood}
\description{
Calculates the log-likelihood multiplied by negative one. It is in a format that can be used with the functions \code{\link[stats]{nlm}} and \code{\link[stats]{optim}}.
}

\usage{
neglogLik(params, object, pmap = NULL, SNOWcluster=NULL)
}

\arguments{
  \item{params}{a vector of revised parameter values.}
  \item{object}{an object of class \code{"\link{mpp}"}.}
  \item{pmap}{a user provided function mapping the revised parameter values \code{params} into the appropriate locations in \code{object}. If \code{NULL} (default), an untransformed one to one mapping is used.}
  \item{SNOWcluster}{an object of class \code{"cluster"} created by the package \pkg{snow}; default is \code{NULL}. Enables parallel processing if not \code{NULL}. See \code{\link{logLik}} for further details.}
}

\value{
Value of the log-likelihood times negative one.
}

\details{
This function can be used with the two functions \code{\link{nlm}} and \code{\link{optim}} (see \dQuote{Examples} below) to maximise the likelihood function of a model specified in \code{object}. Both \code{\link{nlm}} and \code{\link{optim}} are \emph{minimisers}, hence the \dQuote{negative} log-likelihood. The topic \code{\link{distribution}} gives examples of their use in the relatively easy situation of fitting standard probability distributions to data assuming independence.

The maximisation of the model likelihood function can be restricted to be over a subset of the model parameters. Other parameters will then be fixed at the values stored in the model \code{object}. Let \eqn{\Theta_0}{Theta_0} denote the full model parameter space, and let \eqn{\Theta}{Theta} denote the parameter sub-space (\eqn{\Theta \subseteq \Theta_0}{Theta subseteq Theta_0}) over which the likelihood function is to be maximised. The argument \code{params} contains values in \eqn{\Theta}{Theta}, and \code{pmap} is assigned a function that maps these values into the full model parameter space \eqn{\Theta_0}{Theta_0}. See \dQuote{Examples} below.

The mapping function assigned to \code{pmap} can also be made to impose restrictions on the domain of the parameter space \eqn{\Theta}{Theta} so that the minimiser cannot jump to values such that \eqn{\Theta \not\subseteq \Theta_0}{Theta not subseteq Theta_0}. For example, if a particular parameter must be positive, one can work with a transformed parameter that can take any value on the real line, with the model parameter being the exponential of this transformed parameter. Similarly a modified logit like transform can be used to ensure that parameter values remain within a fixed interval with finite boundaries. Examples of these situations can be found in the topic \code{\link{distribution}} and the \dQuote{Examples} below.
}

\seealso{\code{\link[stats]{nlm}}, \code{\link[stats]{optim}}
}

\examples{
#    SRM: magnitude is iid exponential with bvalue=1
#    maximise exponential mark density too

TT <- c(0, 1000)
bvalue <- 1
params <- c(-2.5, 0.01, 0.8, bvalue*log(10))

x <- mpp(data=NULL,
         gif=srm_gif,
         mark=list(dexp_mark, rexp_mark),
         params=params,
         gmap=expression(params[1:3]),
         mmap=expression(params[4]),
         TT=TT)
x <- simulate(x, seed=5)

allmap <- function(y, p){
    #    map all parameters into model object
    #    transform exponential param so it is positive
    y$params[1:3] <- p[1:3]
    y$params[4] <- exp(p[4])
    return(y)
}

params <- c(-2.5, 0.01, 0.8, log(bvalue*log(10)))

z <- nlm(neglogLik, params, object=x, pmap=allmap,
         print.level=2, iterlim=500, typsize=abs(params))
print(z$estimate)

#   these should be the same:
print(exp(z$estimate[4]))
print(1/mean(x$data$magnitude))
}

\keyword{optimize}

\name{linksrm}
\alias{linksrm}

\title{Linked Stress Release Model Object}

\description{
Creates a point process model object with class \code{"linksrm"}.
}

\usage{
linksrm(data, gif, marks, params, gmap, mmap, TT)
}

\arguments{
  \item{data}{a \code{\link{data.frame}} containing the history of the process, denoted below as \eqn{ {\cal H}_t}{Ht}. It should contain all variables that are required to evaluate the \code{gif} function and the mark distribution, though can contain others too. No history is represented as \code{NULL}.}
  \item{gif}{ground intensity function. At this stage, this can only be \code{\link{linksrm_gif}} or modifications of that function; see \dQuote{Details} below.}
  \item{marks}{mark distribution. See topic \code{\link{marks}} for further details.}
  \item{params}{numeric vector of \emph{all} model parameters.}
  \item{gmap}{\code{\link{expression}}, maps the model parameters (\code{params}) into the parameter sub-space of the ground intensity function; see \dQuote{Details} below.}
  \item{mmap}{\code{\link{expression}}, maps the model parameters (\code{params}) into the parameter sub-space of the mark distribution; see \dQuote{Details} below.}
  \item{TT}{vector of length 2, being the time interval over which the integral of the ground intensity function is to be evaluated.}
}

\details{
The linked stress release model has a slightly peculiar structure which makes it difficult to fit into the \code{\link{mpp}} class. While the region should be thought of as a mark, it is completely defined by the function \code{\link{linksrm_gif}}, and hence from the programming perspective the \code{region} mark is really tied in with the \code{gif} function. Hence at the moment, the linked stress release model is treated as a special case. There may be other models that could be grouped into this class.
}

\examples{
p <- c(-1.5, -1.5, 0.01, 0.03, 2, -0.5, 0.2, 1, 1*log(10), 3)
TT <- c(0, 1000)

rexptrunc_mark <- function(ti, data, params){
    x <- rexp(n=1, params[1])
    x[x > params[2]] <- params[2]
    names(x) <- "magnitude"
    return(x)
}

x <- linksrm(data=NULL,
             gif=linksrm_gif,
             mark=list(NULL, rexptrunc_mark),
             params=p,
             gmap=expression(params[1:8]),
             mmap=expression(params[9:10]),
             TT=TT)

x <- simulate(x, seed=5)
print(logLik(x))

#   estimate parameters
temp_map <- function(y, p){
    #    map only gif parameters into model object
    y$params[1:8] <- p
    return(y)
}

#   replace linksrm_gif with linksrm1_gif (faster)
if (exists('St1')) rm(St1)
if (exists('St2')) rm(St2)
x$gif <- linksrm1_gif

weight <- c(0.1, 0.1, 0.005, 0.005, 0.1, 0.1, 0.1, 0.1)
z <- nlm(neglogLik, p[1:8], object=x, updatep=temp_map,
         hessian=TRUE, gradtol=1e-08, steptol=1e-10,
         print.level=2, iterlim=500, typsize=weight)

param.names <- c("a1", "a2", "b1", "b2", "c11", "c12", "c21", "c22")
param.est <- cbind(p[1:8], z$estimate, sqrt(diag(solve(z$hessian))))
dimnames(param.est) <- list(param.names,
                            c("Actual", "Estimate", "StdErr"))
print(param.est)

#   place parameter estimates into model object
x <- temp_map(x, z$estimate)

#   plot ground intensity function
par.default <- par(mfrow=c(2,1), mar=c(4.1, 4.1, 0.5, 1))
x$gif <- linksrm_gif
plot(x, 1, xlab="")
plot(x, 2)
par(par.default)

#   plot "residuals" for each region
tau <- residuals(x)
par(mfrow=c(2,1))
plot(tau[[1]], ylab="Transformed Time",
     xlab="Event Number", main="Region 1")
abline(a=0, b=1, lty=2, col="red")
plot(tau[[2]], ylab="Transformed Time",
     xlab="Event Number", main="Region 2")
abline(a=0, b=1, lty=2, col="red")
par(mfrow=c(1,1))
}

\keyword{classes}

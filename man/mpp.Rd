\name{mpp}
\alias{mpp}

\title{Marked Point Process Object}

\description{
Creates a marked point process model object with class \code{"mpp"}.
}

\usage{
mpp(data, gif, marks, params, gmap, mmap, TT)
}

\arguments{
  \item{data}{a \code{\link{data.frame}} containing the history of the process, denoted below as \eqn{ {\cal H}_t}{Ht}. It should contain all variables that are required to evaluate the \code{gif} function and the mark distribution, though can contain others too. No history is represented as \code{NULL}.}
  \item{gif}{ground intensity function. See topic \code{\link{gif}} for further details.}
  \item{marks}{mark distribution. See topic \code{\link{marks}} for further details.}
  \item{params}{numeric vector of \emph{all} model parameters.}
  \item{gmap}{\code{\link{expression}}, maps the model parameters (\code{params}) into the parameter sub-space of the ground intensity function; see \dQuote{Details} below.}
  \item{mmap}{\code{\link{expression}}, maps the model parameters (\code{params}) into the parameter sub-space of the mark distribution; see \dQuote{Details} below.}
  \item{TT}{vector of length 2, being the time interval over which the integral of the ground intensity function is to be evaluated.}
}

\details{
Let \eqn{\lambda_g(t|{\cal H}_t)}{lambda_g(t|Ht)} denote the ground intensity function and \eqn{f(y|{\cal H}_t)}{f(y|Ht)} denote the joint mark densities, where \eqn{y \in {\cal Y}}{y in Y}. The log-likelihood of a marked point process is given by
\deqn{
\log L = \sum_i \log \lambda_g(t_i|{\cal H}_{t_i}) + \sum_i \log f(y_i|{\cal H}_{t_i}) - \int \lambda_g(t|{\cal H}_t) dt,
}{
log L = sum{log lambda_g(ti|Ht)} + sum{log f(yi|Ht)} - integral{lambda_g(t|Ht) dt},
}
where the summation is taken over those events contained in the interval \code{(TT[1], TT[2])}, and the integral is also taken over that interval. However, all events in the data frame \code{data} before \eqn{t}, even those before \code{TT[1]}, form the history of the process \eqn{ {\cal H}_t}{Ht}. This allows an initial period for the process to reach a \dQuote{steady state} or \dQuote{equilibrium}.

The parameter spaces of the ground intensity function and mark distribution are not necessarily disjoint, and can have common parameters. Hence, when the model parameters are estimated, these relationships must be known, and are specified by the arguments \code{gmap} and \code{mmap}. The mapping expressions can also contain arithmetic expressions. The \eqn{i}th element in the \code{params} argument is addressed in the expressions as \code{params[i]}. Here is an example of a five parameter model, where the \code{gif} has 4 parameters, and the mark distribution has 2, with mappings specified as:
\preformatted{
    gmap = expression(c(params[1:3], exp(params[4]+params[5])))

    mmap = expression(c(log(params[2]/3), params[5]))
}
Note the inclusion of the combine (\code{\link{c}}) function, because the \code{\link{expression}} must create a vector of parameters. Care must be taken specifying these expressions are they are embedded directly into the code of various functions.
}

\examples{
data(Tangshan)

#   increment magnitudes a fraction so none are zero
Tangshan[,"magnitude"] <- Tangshan[,"magnitude"] + 0.01

dmagn_mark <- function(x, data, params){
    #  Gamma distribution
    #  exponential density when params[7]=0
    #   See topic "marks" for further discussion
    lambda <- etas_gif(data, x[,"time"], params=params[1:5])
    y <- dgamma(x[,"magnitude"], shape=1+sqrt(lambda)*params[7],
                rate=params[6], log=TRUE)
    return(y)
}

TT <- c(0, 4018)
params <- c(0.007, 2.3, 0.98, 0.008, 0.94, 1.25, 0.2)

x <- mpp(data=Tangshan,
         gif=etas_gif,
         mark=list(dmagn_mark, NULL),
         params=params,
         gmap=expression(params[1:5]),
         mmap=expression(params[1:7]),
         TT=TT)

allmap <- function(y, p){
    #    one to one mapping, all p positive
    y$params <- exp(p)
    return(y)
}

#    Parameters must be positive. Transformed so that nlm
#    can use entire real line (no boundary problems, see
#    topic "neglogLik" for further explanation).
z <- nlm(neglogLik, log(params), object=x, pmap=allmap,
         print.level=2, iterlim=500, typsize=abs(params))

x1 <- allmap(x, z$estimate)

#    print parameter estimates
print(x1$params)

print(logLik(x))
print(logLik(x1))
plot(x1, log=TRUE)
}

\keyword{classes}

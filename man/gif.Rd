\name{gif}
\alias{gif}
\title{General Notes on Ground Intensity Functions}

\description{
This page contains general notes about the required structure of ground intensity functions (including those that are not conditional on their history) to be used with this package.
}

%  have used the section environment below to retain correct ordering
\section{Forms of Usage}{
The usage of a ground intensity function takes two forms, one to evaluate the \code{gif} at specified \code{evalpts}, or to evaluate the integral of the \code{gif} on the interval \code{TT}, each shown below, respectively.
\cr
\code{gif(data, evalpts, params, tplus=FALSE)}
\cr
\code{gif(data, NULL, params, TT)}
}


\section{Arguments}{
All ground intensity functions should be defined to contain the following arguments, in the order below, even though they may not be required (see Details below).
\describe{
  \item{\code{data}}{a data frame containing the history of the process, denoted below as \eqn{ {\cal H}_t}{Ht}. It should contain all variables that are required to evaluate the \code{gif} function, though can contain others too. No history is represented as \code{NULL}.}
  \item{\code{evalpts}}{a matrix containing the values at which the \code{gif} function is to be evaluated, consistent with what is required by the \code{gif} function.}
  \item{\code{params}}{vector containing values of the parameters required by the \code{gif} function.}
  \item{\code{TT}}{vector of length 2, being the time interval over which the integral of the ground intensity function is to be evaluated.}
\item{\code{tplus}}{logical, \eqn{\lambda_g(t|{\cal H}_t)}{lambda_g(t|Ht)} is evaluated as \eqn{\lambda_g(t^+|{\cal H}_t)}{lambda_g(t^+|Ht)} if \code{TRUE}, else \eqn{\lambda_g(t^-|{\cal H}_t)}{lambda_g(t^-|Ht)}. It is important if a \dQuote{jump} occurs at \eqn{t}.}
}}

\section{Details}{
Note that the \code{gif} functions not only evaluate values of \eqn{\lambda_g(t_i|{\cal H}_t)}{lambda_g(ti|Ht)}, but also the integral. The value of the ground intensity function is returned at each time point specified in \code{evalpts} when \code{TT==NA}. If \code{TT} is not missing, the integral between \code{TT[1]} and \code{TT[2]} of the ground intensity function is calculated. In this last situation, anything assigned to the argument \code{evalpts} will have no effect.

At the moment, we have the following types of processes: those jump processes that are conditional on their history (\code{\link{etas_gif}}, \code{\link{srm_gif}}, \code{\link{linksrm_gif}}), and non-homogeneous Poisson processes that are not conditional on their history (\code{\link{simple_gif}}). Another case is where we have a collection of point like \dQuote{regions} (or lattice nodes), each with their own ground intensity function, but where each is also dependent on what is happening in the other regions (\code{\link{linksrm_gif}}). The attribute \dQuote{type} determines the \dQuote{methods} used for simulation (\code{\link{simulate.mpp}}) and residual analyses (\code{\link{residuals.mpp}}).

Functions have been given an attribute \dQuote{rate}, taking the values of \dQuote{bounded}, \dQuote{decreasing} or \dQuote{increasing}. This is used within the simulation function \code{\link{simulate.mpp}} which uses the thinning method. This method requires a knowledge of the maximum of \eqn{\lambda_g(t|{\cal H}_t)}{lambda_g(t|Ht)} in a given interval. The argument \code{tplus} is also used by the simulation routine, where it is necessary to determine the value of the intensity immediately after a simulated event.
}

\section{Value}{
The returned value is either \eqn{\lambda_g(t_i|{\cal H}_t)}{lambda_g(ti|Ht)}, where the \eqn{t_i}{ti} are specified within \code{evalpts}; or
\deqn{\int \lambda_g(t|{\cal H}_t) dt}{integral{ lambda_g(t|Ht) dt }}
where the limits of the integral are specified by the function argument \code{TT}.
}

\section{Function Attributes}{
Each function should have some of the following attributes if it is to be used in conjunction with \code{\link{residuals.mpp}} or \code{\link{simulate.mpp}}:
\describe{
\item{\code{rate}}{must be specified if the default method for \code{\link{simulate.mpp}} is to be used. Takes the values \code{"bounded"}, \code{"decreasing"} or \code{"increasing"}; see Details.}

\item{\code{regions}}{an expression giving the number of regions; required with \code{\link{linksrm_gif}}.}

\item{\code{type}}{takes the values \code{"default"} or \code{"linksrm"}, and indicates the \dQuote{methods} to be used by \code{\link{residuals.mpp}} and \code{\link{simulate.mpp}}. If type is not specified, then the \code{"default"} method is used.}
}}

\seealso{
\code{\link{etas_gif}}, \code{\link{expfourier_gif}}, \code{\link{exppoly_gif}}, \code{\link{fourier_gif}}, \code{\link{linksrm_gif}}, \code{\link{poly_gif}}, \code{\link{simple_gif}}, \code{\link{srm_gif}}
}

\examples{
#  Ogata's Data: ground intensity function
#  evaluate lambda_g(t) at certain times

data(Ogata)

p <- c(0.02, 70.77, 0.47, 0.002, 1.25)
times <- sort(c(seq(0, 800, 0.5), Ogata$time))
TT <- c(0, 800)

plot(times, log(etas_gif(Ogata, times, params=p)), type="l",
     ylab=expression(paste(log, " ", lambda[g](t))),
     xlab=expression(t), xlim=TT)

#  Evaluate the integral
#   The first form below is where the arguments are in their
#   default positions, the 2nd is where they are not, hence
#   their names must be specified

print(etas_gif(Ogata, NULL, p, TT))
#  or
print(etas_gif(Ogata, params=p, TT=TT))
}

\keyword{documentation}


\name{srm_gif}
\alias{srm_gif}
\title{
Conditional Intensity for Stress Release Model
}
\description{
This function calculates the value of the conditional intensity of a Stress Release Model (SRM). Spatial coordinates of the events are not taken into account.
}

\usage{
srm_gif(data, evalpts, params, TT=NA, tplus=FALSE)
}

\arguments{
\item{data}{
a data frame containing the event history, where each row represents one event. There must be columns named \dQuote{time}, usually the number of days from some origin; and \dQuote{magnitude} which is the event magnitude less the magnitude threshold, i.e. \eqn{M_i - M_0}{Mi - M0}.
}
\item{evalpts}{
a \code{\link{vector}}, \code{\link{matrix}} or \code{\link{data.frame}}. If a vector, the elements will be assumed to represent the required evaluation times. Other objects must include a column named \code{"time"} that can be referred to as \code{evalpts[,"time"]}, at which the intensity function will be evaluated.
}
\item{params}{
vector of parameters for the proposed SRM model in the order \eqn{(a, b, c)}.
}
\item{TT}{
vector of length 2, being the time interval over which the integral of the conditional intensity function is to be evaluated.
}
\item{tplus}{logical, \eqn{\lambda_g(t|{\cal H}_t)}{lambda_g(t|Ht)} is evaluated as \eqn{\lambda_g(t^+|{\cal H}_t)}{lambda_g(t^+|Ht)} if \code{TRUE}, else \eqn{\lambda_g(t^-|{\cal H}_t)}{lambda_g(t^-|Ht)}.
}
}
\value{
Two usages are as follows.
\preformatted{
srm_gif(data, evalpts, params, tplus=FALSE)
srm_gif(data, evalpts=NULL, params, TT)
}
The first usage returns a vector containing the values of \eqn{\lambda_g(t)}{lambda_g(t)} evaluated at the specified points. In the second usage, it returns the value of the integral.
}
\details{
Vere-Jones (1978) proposed the stress release model, being a stochastic version of elastic rebound theory (Reid, 1910). The SRM assumes a deterministic increase in stress over time, and a stochastic release through earthquake events. The conditional intensity function is
\deqn{
\lambda_g(t) = \exp\{a + b[t - cS(t)]\},
}{
lambda_g(t) = exp{a + b[t - cS(t)]},
}
where
\deqn{
S(t) = \sum_i 10^{0.75(M_i-M_0)}
}{
S(t) = sum{10^[0.75(M_i-M_0)]}
}
and the summation is taken over those \eqn{i} such that \eqn{t_i < t}{ti < t}, where \eqn{t_i}{ti} denotes the event times.
}

\section{Function Attributes}{
\describe{
\item{\code{rate}}{is \code{"increasing"}.}
}}

\seealso{
General details about the structure of conditional intensity functions are given in the topic \code{\link{gif}}.
}

\section{Problems and Inconsistencies}{
Runs much slower than \code{\link{linksrm_gif}}. Should set up matrices \code{St1} and \code{St2} as in \code{\link{linksrm_gif}}.
}

\examples{
#  Treating North China as one region

data(NthChina)
p <- c(-2.46, 0.0113, 0.851)
times <- seq(0, 517, 0.5)

par.default <- par(mfrow=c(2,1), mar=c(4.1, 4.1, 0.5, 1))
plot(times+1480, srm_gif(NthChina, times, params=p), type="l",
     ylab=expression(lambda[g](t)),
     xlab="", xlim=c(1480, 2000))
plot(NthChina$time+1480, NthChina$magnitude+6, type="h",
     xlim=c(1480, 2000), ylim=c(5.8, 8.6),
     xlab="Year", ylab="Magnitude")

par(par.default)
}

\keyword{models}


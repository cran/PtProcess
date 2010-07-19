\name{etas_gif}
\alias{etas_gif}
\title{
Ground Intensity for ETAS Model
}
\description{
This function calculates the value of the ground intensity of a time-magnitude Epidemic Type Aftershock Sequence (ETAS) model. Spatial coordinates of the events are not taken into account.
}

\usage{
etas_gif(data, evalpts, params, TT=NA, tplus=FALSE)
}

\arguments{
\item{data}{
a data frame containing the event history, where each row represents one event. There must be columns named \code{"time"}, usually the number of days from some origin; and \code{"magnitude"} which is the event magnitude less the magnitude threshold, i.e. \eqn{M_i - M_0}{Mi - M0}.
}
\item{evalpts}{
a \code{\link{vector}}, \code{\link{matrix}} or \code{\link{data.frame}}. If a vector, the elements will be assumed to represent the required evaluation times. Other objects must include a column named \code{"time"} that can be referred to as \code{evalpts[,"time"]}, at which the intensity function will be evaluated.
}
\item{params}{
vector of parameter values in the following order: \eqn{(\mu, A, \alpha, c, p)}{(mu, A, alpha, c, p)}.
}
\item{TT}{
vector of length 2, being the time interval over which the integral of the ground intensity function is to be evaluated.
}
\item{tplus}{logical, \eqn{\lambda_g(t|{\cal H}_t)}{lambda_g(t|Ht)} is evaluated as \eqn{\lambda_g(t^+|{\cal H}_t)}{lambda_g(t^+|Ht)} if \code{TRUE}, else \eqn{\lambda_g(t^-|{\cal H}_t)}{lambda_g(t^-|Ht)}.
}}

\value{
Two usages are as follows.
\preformatted{
etas_gif(data, evalpts, params, tplus=FALSE)
etas_gif(data, evalpts=NULL, params, TT)
}
The first usage returns a vector containing the values of \eqn{\lambda_g(t)}{lambda_g(t)} evaluated at the specified points. In the second usage, it returns the value of the integral.
}

\details{
The ETAS model was proposed by Ogata (1988, 1998, 1999) for the modelling of earthquake mainshock-aftershock sequences. The form of the ground intensity function used here is given by
\deqn{
\lambda_g(t|{\cal H}_t) = \mu + A \sum_{i : t_i < t} e^{\alpha(M_i - M_0)} \left( 1 + \frac{t-t_i}{c} \right)^{-p},
}{
lambda_g(t|Ht) = mu + A sum{exp[alpha(Mi - M0)](1 + (t-ti)/c)^(-p)},
}
where \eqn{t_i}{ti} denotes the event times and the summation is taken over those \eqn{i} such that \eqn{t_i < t}{ti < t}.
}

\section{Function Attributes}{
\describe{
\item{\code{rate}}{is \code{"decreasing"}.}
}}

\seealso{
General details about the structure of ground intensity functions are given in the topic \code{\link{gif}}.
}

\references{
Cited references are listed on the \link{PtProcess} manual page.
}

\examples{
#  Tangshan: ground intensity and magnitude time plots

data(Tangshan)
p <- c(0.007, 2.3, 0.98, 0.008, 0.94)
bvalue <- 1
TT <- c(0, 4018)

x <- mpp(data=Tangshan,
         gif=etas_gif,
         marks=list(rexp_mark, rexp_mark),
         params=p,
         gmap=expression(params),
         mmap=expression(bvalue*log(10)),
         TT=TT)

par.default <- par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(2,1), mar=c(4.1, 4.1, 0.5, 1))

plot(x, log=TRUE, xlab="")

plot(Tangshan$time, Tangshan$magnitude+4, type="h",
     xlim=c(0, 4018),
     xlab="Days Since 1 January 1974", ylab="Magnitude")

par(par.default)
}

\keyword{models}

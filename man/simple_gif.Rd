\name{simple_gif}
\alias{expfourier_gif}
\alias{exppoly_gif}
\alias{fourier_gif}
\alias{poly_gif}
\alias{simple_gif}
\title{
Non-Homogeneous Poisson Processes
}
\description{
The functions listed here are intensity functions that are not conditional on the history of the process. Each has exactly the same usage and synopsis as the function \code{simple_gif}. They are: \code{expfourier_gif}, \code{exppoly_gif}, \code{fourier_gif}, \code{poly_gif}, and \code{simple_gif}.
}
\synopsis{
simple_gif(data, evalpts, params, TT=NA, tplus=FALSE)
}

\usage{
simple_gif(data, evalpts, params, tplus=FALSE)
simple_gif(data, evalpts=NULL, params, TT=NA)
}
\arguments{
\item{data}{
\code{NULL} or a data frame. The contents of this object are not used by these functions, though they retain this argument for consistency with other \code{\link{gif}} functions.}
\item{evalpts}{
a \code{\link{vector}}, \code{\link{matrix}} or \code{\link{data.frame}}. If a vector, the elements will be assumed to represent the required evaluation times. Other objects must include a column named \code{"time"} that can be referred to as \code{evalpts[,"time"]}, at which the intensity function will be evaluated.
}
\item{params}{
vector of parameter values as required by the particular intensity function, see Details below.
}
\item{TT}{
vector of length 2, being the time interval over which the integral of the intensity function is to be evaluated.
}
\item{tplus}{logical, \eqn{\lambda_g(t|{\cal H}_t)}{lambda_g(t|Ht)} is evaluated as \eqn{\lambda_g(t^+|{\cal H}_t)}{lambda_g(t^+|Ht)} if \code{TRUE}, else \eqn{\lambda_g(t^-|{\cal H}_t)}{lambda_g(t^-|Ht)}. Included for compatibility with others conditional intensity functions.
}
}

\value{
The first usage returns a vector containing the values of \eqn{\lambda_g(t)}{lambda_g(t)} evaluated at the specified points. In the second usage, it returns the value of the integral.
}

\details{
The models are parameterised as follows.
\describe{
\item{\code{expfourier_gif}}{
The vector of parameters is
\deqn{
\code{c}(p, a_0, a_1, a_2, \cdots, a_n, b_1, b_2, \cdots, b_n)
}{
\code{c}(p, a0, a1, a2, \dots, an, b1, b2, \dots, bn)
}
and the intensity function is
\deqn{
\lambda_g(t) = \exp\left\{a_0 + \sum_{j=1}^n a_j\cos\left(\frac{2j\pi t}{p}\right) + \sum_{j=1}^n b_j\sin\left(\frac{2j\pi t}{p}\right)\right\}.
}{
lambda_g(t) = exp{a0 + a1*cos(2*pi*t/p) + a2*cos(4*pi*t/p) + \dots + an*cos(2*n*pi*t/p) + b1*sin(2*pi*t/p) + b2*sin(4*pi*t/p) + \dots + bn*sin(2*n*pi*t/p)}.
}
The length of \code{params} is \eqn{2n + 2}, and determines the order of the fitted Fourier series. The numbers of specified sine and cosine coefficients must be the same. The integral is evaluated using numerical integration, using the \R function \code{\link{integrate}}.
}
\item{\code{exppoly_gif}}{
The vector of parameters is
\eqn{
\code{c}(b_0, b_1, b_2, \cdots, b_n)
}{
\code{c}(b0, b1, b2, \dots, bn)
}
and the intensity function is
\deqn{
\lambda_g(t) = \exp\left\{b_0 + \sum_{j=1}^n b_j t^j \right\}.
}{
lambda_g(t) = exp{b0 + b1*t + b2*t^2 \dots bn*t^n}.
}
The length of \code{params} determines the order of the fitted polynomial. The integral is evaluated using numerical integration, using the \R function \code{\link{integrate}}.
}
\item{\code{fourier_gif}}{
The Fourier intensity function is the same as \code{expfourier_gif}, except the intensity function omits the exponential, and the integration is performed explicitly.
}
\item{\code{poly_gif}}{
The polynomial intensity function is the same as \code{exppoly_gif}, except the intensity function omits the exponential, and the integration is performed explicitly.
}
\item{\code{simple_gif}}{
The intensity function is \eqn{\lambda_g(t) = a + b t^g}{lambda_g(t) = a + b t^g} and the vector of parameters is \eqn{\code{c}(a, b, g)}.
}
}}

\section{Function Attributes}{
\describe{
\item{\code{rate}}{is \code{"bounded"}.}
}}

\seealso{
General details about the structure of conditional intensity functions are given in the topic \code{\link{gif}}.
}

\examples{
expfourier_gif(NULL, c(1.1,1.2,1.3), c(2,3,1,2,3,4), TT=NA)
#  Evaluates:  lambda_g(t) = exp(3 + 1*cos(2*pi*t/2) + 2*cos(4*pi*t/2) +
#                                3*sin(2*pi*t/2) + 4*sin(4*pi*t/2))
#  lambda_g(1.1) = 162.56331
#  lambda_g(1.2) = 127.72599
#  lambda_g(1.3) =  23.83979

expfourier_gif(NULL, NULL, c(2,3,1,2,3,4), TT=c(3,4))
#  Let:  lambda_g(t) = exp(3 + 1*cos(2*pi*t/2) + 2*cos(4*pi*t/2) +
#                              3*sin(2*pi*t/2) + 4*sin(4*pi*t/2))
#  Evaluates: integral_3^4 lambda_g(t) dt = 46.21920
}

\keyword{models}

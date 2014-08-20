\name{dpareto}
\alias{dpareto}
\alias{ppareto}
\alias{qpareto}
\alias{rpareto}
\alias{dtappareto}
\alias{ltappareto}
\alias{ptappareto}
\alias{qtappareto}
\alias{rtappareto}

\title{Pareto and Tapered Pareto Distributions}

\description{
Density, cumulative probability, quantiles and random number generation for the Pareto and tapered Pareto distributions with shape parameter \eqn{\lambda}{lambda}, tapering parameter \eqn{\theta}{theta} and range \eqn{a \le x < \infty}{a <= x < infty}; and log-likelihood of the tapered Pareto distribution.
}

\usage{
dpareto(x, lambda, a, log=FALSE)
ppareto(q, lambda, a, lower.tail=TRUE, log.p=FALSE)
qpareto(p, lambda, a, lower.tail=TRUE, log.p=FALSE)
rpareto(n, lambda, a)

dtappareto(x, lambda, theta, a, log=FALSE)
ltappareto(data, lambda, theta, a)
ptappareto(q, lambda, theta, a, lower.tail=TRUE, log.p=FALSE)
qtappareto(p, lambda, theta, a, lower.tail=TRUE, log.p=FALSE,
           tol=1e-8)
rtappareto(n, lambda, theta, a)

ltappareto(data, lambda, theta, a)
}

\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{data}{vector of sample data.}
  \item{n}{number of observations to simulate.}
  \item{lambda}{shape parameter, see Details below.}
  \item{theta}{tapering parameter, see Details below..}
  \item{a}{the random variable takes values on the interval \eqn{a \le x < \infty}{a <= x < infty}. This is a scalar and is assumed to be a constant for all values in a given function call.}
  \item{log, log.p}{logical; if \code{TRUE}, probabilities \code{p} are given as \code{log(p)}.}
  \item{lower.tail}{logical; if \code{TRUE} (default), probabilities are \eqn{\Pr\{X \le x\}}{Pr{X <= x}}, otherwise, \eqn{\Pr\{X > x\}}{Pr{X > x}}.}
  \item{tol}{convergence criteria for the Newton Raphson algorithm for solving the quantiles of the tapered Pareto distribution.}
}

\details{
For all functions except \code{ltappareto}, arguments \code{lambda} and \code{theta} can either be scalars or vectors of the same length as \code{x}, \code{p}, or \code{q}. If a scalar, then this value is assumed to hold over all cases. If a vector, then the values are assumed to have a one to one relationship with the values in \code{x}, \code{p}, or \code{q}. The argument \code{a} is a scalar.

In the case of \code{ltappareto}, all \code{data} are assumed to be drawn from the same distribution and hence \code{lambda}, \code{theta} and \code{a} are all scalars.

Let \eqn{Y} be an exponential random variable with parameter \eqn{\lambda > 0}{lambda > 0}. Then the distribution function of \eqn{Y} is
\deqn{
F_Y(y) = \Pr\{Y < y \} = 1 - \exp(-\lambda y),
}{
F_Y(y) = Pr{Y < y} = 1 - exp(-lambda*y),
}
and the density function is
\deqn{
f_Y(y) = \lambda \exp(-\lambda y).
}{
f_Y(y) = lambda exp(-lambda*y).
}
Further, the mean and variance of the distribution of \eqn{Y} is \eqn{1/\lambda}{1/lambda} and \eqn{1/\lambda^2}{1/lambda^2}, respectively.

Now transform \eqn{Y} as
\deqn{
X = a \exp(Y),
}{
X = a exp(Y),
}
where \eqn{a>0}. Then \eqn{X} is a Pareto random variable with shape parameter \eqn{\lambda}{lambda} and distribution function
\deqn{
F_X(x) = \Pr\{X < x \} = 1 - \left( \frac{a}{x} \right)^\lambda,
}{
F_X(x) = Pr{X < x} = 1 - (a/x)^lambda,
}
where \eqn{a \le x < \infty}{a <= x < infty}, and density function
\deqn{
f_X(x) = \frac{\lambda}{a} \left( \frac{a}{x} \right)^{\lambda+1}.
}{
f_X(x) = (lambda/a) * (a/x)^(lambda+1).
}

We simulate the Pareto deviates by generating exponential deviates, and then transforming as described above.

As above, let \eqn{X} be Pareto with shape parameter \eqn{\lambda}, and define \eqn{W - a} to be exponential with parameter \eqn{1/\theta}, i.e.
\deqn{
\Pr\{X > x\} = \left( \frac{a}{x} \right)^\lambda
}{
Pr{X > x} = (a/x)^lambda
}
and
\deqn{
\Pr\{W > w\} = \exp\left( \frac{a - w}{\theta} \right),
}{
Pr{W > w} = exp[(a - w)/theta],
}
where \eqn{a \le w < \infty}{a <= w < infty}. Say we sample one independent value from each of the distributions \eqn{X} and \eqn{W}, then
\deqn{
\Pr\{X > z\ \&\ W > z\} = \Pr\{X > z\} \Pr\{ W > z\} = \left( \frac{a}{z} \right)^\lambda \exp\left( \frac{a - z}{\theta} \right).
}{
Pr{X > z & W > z} = Pr{X > z} Pr{ W > z} = (a/z)^lambda * exp[(a - z)/theta].
}
We say that \eqn{Z} has a tapered Pareto distribution if it has the above distribution, i.e.
\deqn{
F_Z(z) = \Pr\{Z < z\} = 1- \left( \frac{a}{z} \right)^\lambda \exp\left( \frac{a - z}{\theta} \right).
}{
F_Z(z) = Pr{Z < z} = 1 - (a/z)^lambda * exp[(a - z)theta].
}
The above relationship shows that a tapered Pareto deviate can be simulated by generating independent values of \eqn{X} and \eqn{W}, and then letting \eqn{Z = \min(X, W)}{Z = min(X, W)}. This minimum has the effect of \dQuote{tapering} the tail of the Pareto distribution.

The tapered Pareto variable \eqn{Z} has density
\deqn{
f_Z(z) = \left( \frac{\lambda}{z} + \frac{1}{\theta} \right) \left( \frac{a}{z} \right)^\lambda \exp\left( \frac{a - z}{\theta} \right).
}{
f_Z(z) = (lambda/z + 1/theta) (a/z)^lambda * exp[(a - z)/theta].
}
Given a sample of data \eqn{z_1, z_2, \cdots, z_n}{z_1, z_2, ..., z_n}, we write the log-likelihood as
\deqn{
\log L = \sum_{i=1}^n \log f_Z(z_i).
}{
log L = SUM_{i=1}^n log f_Z(z_i).
}
Hence the gradients are calculated as
\deqn{
\frac{\partial \log L}{\partial \lambda} = \theta \sum_{i=1}^n \frac{1}{\lambda \theta + z_i} - \sum_{i=1}^n \log(z_i/a)
}{
(partial log L)/(partial lambda) = theta * SUM_i{1/(lambda*theta + z_i)} - SUM_i{log(z_i/a)}
}
and
\deqn{
\frac{\partial \log L}{\partial \theta} = \frac{-1}{\theta} \sum_{i=1}^n \frac{z_i}{\lambda \theta + z_i} - \frac{1}{\theta^2} \sum_{i=1}^n (a - z_i).
}{
(partial log L/partial theta) = -1/theta * SUM_i{z_i/(lambda*theta + z_i)} - 1/theta^2 * SUM_i{a - z_i}.
}
Further, the Hessian is calculated using
\deqn{
\frac{\partial^2 \log L}{\partial \lambda^2} = -\theta^2 \sum_{i=1}^n \frac{1}{(\lambda \theta + z_i)^2},
}{
(partial^2 log L)/(partial lambda^2) = -theta^2 * SUM_i{1/(lambda*theta + z_i)^2},
}
\deqn{
\frac{\partial^2 \log L}{\partial \theta^2} = \frac{1}{\theta^2} \sum_{i=1}^n \frac{z_i(2\lambda\theta + z_i)}{(\lambda \theta + z_i)^2} - \frac{2}{\theta^3} \sum_{i=1}^n (a - z_i),
}{
(partial^2 log L)/(partial theta^2) = 1/theta^2 * SUM_i{z_i*(2*lambda*theta + z_i)/(lambda*theta + z_i)^2} + 2/theta^3 * SUM_i{a - z_i},
}
and
\deqn{
\frac{\partial^2 \log L}{\partial \theta \, \partial \lambda} = \frac{\partial^2 \log L}{\partial \lambda \, \partial \theta} = \sum_{i=1}^n \frac{z_i}{(\lambda \theta + z_i)^2}.
}{
(partial^2 log L)/(partial theta partial lambda) = (partial^2 log L)/(partial lambda partial theta) = SUM_i{z_i/(lambda*theta + z_i)^2}.
}


See the section \dQuote{Seismological Context} (below), which outlines its application in Seismology.
}

\section{Seismological Context}{
The Gutenberg-Richter (GR) Law says that if we plot the base 10 logarithm of the number of events with magnitude greater than \eqn{M} (vertical axis) against \eqn{M} (horizontal axis), there should be a straight line. This is equivalent to magnitudes having an exponential distribution.

Assume that the magnitude cutoff is \eqn{M_0}, and let \eqn{Y = M - M_0}. Given that \eqn{Y} has an exponential distribution with parameter \eqn{\lambda}, it follows that
\deqn{
\log_{10} \left( 1 - F_Y(y) \right) = \frac{-\lambda y}{\log_e 10}.
}{
log_10 (1 - F_Y(y)) = lambda/(log_e 10) * y.
}
The coefficient \eqn{\lambda/(\log_e 10)}{lambda/(log_e 10)} is often referred to as the \eqn{b}-value, and its negative value is the slope of the line in the GR plot.

Now define \eqn{S} as
\deqn{
S = 10^{\gamma (M - M_0)} = 10^{\gamma Y}.
}{
S = 10^{gamma (M - M_0)} = 10^{gamma * Y}.
}
When \eqn{\gamma = 0.75}{gamma=0.75}, \eqn{S} is the \dQuote{stress}; and when \eqn{\gamma = 1.5}{gamma=1.5}, \eqn{S} is the \dQuote{seismic moment}. Still assuming that \eqn{Y} is exponential with parameter \eqn{\lambda}{lambda}, then \eqn{Y \gamma \log_e 10}{Y * gamma  * log_e 10} is also exponential with parameter \eqn{\lambda/(\gamma \log_e 10)}{lambda/(gamma * log_e 10}. Hence, by noting that \eqn{S} can be rewritten as
\deqn{
S = \exp\{ Y \gamma \log_e 10 \},
}{
S = exp[ Y * gamma * log_e 10 ],
}
it is seen that \eqn{S} is Pareto with parameter \eqn{\lambda/(\gamma \log_e 10)}{lambda/(gamma * log_e 10)}, and \eqn{1 \le S < \infty}{1 <= S < infty}.

While the empirical distribution of magnitudes appears to follow an exponential distribution for smaller events, it provides a poor approximation for larger events. This is because it is not physically possible to have events with magnitudes much greater than about 9.5. Consequently, the tail of the Pareto distribution will also be too long. Hence the tapered Pareto distribution provides a more realistic description.
}

\value{
\code{dpareto} and \code{dtappareto} give the densities; \code{ppareto} and \code{ptappareto} give the distribution functions; \code{qpareto} and \code{qtappareto} give the quantile functions; and \code{rpareto} and \code{rtappareto} generate random deviates.

\code{ltappareto} returns the log-likelihood of a sample using the tapered Pareto distribution. It also calculates, using analytic expressions (see \dQuote{Details}), the derivatives and Hessian which are attached to the log-likelihood value as the attributes \code{"gradient"} and \code{"hessian"}, respectively.
}

\seealso{
See \code{\link{dexp}} for the exponential distribution. Generalisations of the exponential distribution are the gamma distribution \code{\link{dgamma}} and the Weibull distribution \code{\link{dweibull}}.

See the topic \code{\link{distribution}} for examples of estimating parameters.
}

\examples{
#    Simulate and plot histogram with density for Pareto Distribution

a0 <- 2
lambda0 <- 2
x <- rpareto(1000, lambda=lambda0, a=a0)
x0 <- seq(a0, max(x)+0.1, length=100)
hist(x, freq=FALSE, breaks=x0, xlim=range(x0),
     main="Pareto Distribution")
points(x0, dpareto(x0, lambda0, a0), type="l", col="red")

#-----------------------------------------------
#    Calculate probabilities and quantiles for Pareto Distribution

a0 <- 2
lambda0 <- 2
prob <- ppareto(seq(a0, 8), lambda0, a0)
quan <- qpareto(prob, lambda0, a0)
print(quan)

#-----------------------------------------------
#    Simulate and plot histogram with density for tapered Pareto Distribution

a0 <- 2
lambda0 <- 2
theta0 <- 3
x <- rtappareto(1000, lambda=lambda0, theta=theta0, a=a0)
x0 <- seq(a0, max(x)+0.1, length=100)
hist(x, freq=FALSE, breaks=x0, xlim=range(x0),
     main="Tapered Pareto Distribution")
points(x0, dtappareto(x0, lambda0, theta0, a0), type="l", col="red")

#-----------------------------------------------
#    Calculate probabilities and quantiles for tapered Pareto Distribution

a0 <- 2
lambda0 <- 2
theta0 <- 3
prob <- ptappareto(seq(a0, 8), lambda0, theta0, a0)
quan <- qtappareto(prob, lambda0, theta0, a0)
print(quan)

#-----------------------------------------------
#    Calculate log-likelihood for tapered Pareto Distribution
#    note the Hessian and gradient attributes

a0 <- 2
lambda0 <- 2
theta0 <- 3
x <- rtappareto(1000, lambda=lambda0, theta=theta0, a=a0)
LL <- ltappareto(x, lambda=lambda0, theta=theta0, a=a0)
print(LL)

}

\keyword{distribution}


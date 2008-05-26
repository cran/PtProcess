\name{distribution}
\alias{distribution}
\title{General Notes on Distribution Fitting}
\description{
This page contains general notes about fitting probability distributions to datasets.
}

\details{
We give examples of how the maximum likelihood parameters can be estimated using standard optimisation routines provided in the \R\ software (\code{\link{nlm}} and \code{\link{optim}}). We simply numerically maximise the sum of the logarithms of the density evaluated at each of the data points, i.e. log-likelihood function. In fact, by default, the two mentioned optimizers find the \emph{minimum}, and hence we minimise the negative log-likelihood function.

Both optimization routines require initial starting values. The optimisation function \code{\link{optim}} uses a grid search technique, and is therefore more robust to poor starting values. The function \code{\link{nlm}} uses derivatives and the Hessian to determine the size and direction of the next step, which is generally more sensitive to poor initial values, but faster in the neighbourhood of the solution. One possible strategy is to start with \code{\link{optim}} and then use its solution as a starting value for \code{\link{nlm}}. This is done below in the example for the tapered Pareto distribution.

The function \code{\link{nlm}} numerically calculates the Hessian and derivatives, by default. If the surface is very flat, the numerical error involved may be larger in size than the actual gradient. In this case the process will work better if analytic derivatives are supplied. This is done in the tapered Pareto example below. Alternatively, one could simply use the Newton-Raphson algorithm (again, see the tapered Pareto example below).

We also show that parameters can be constrained to be positive (or negative) by transforming the parameters with the exponential function during the maximisation procedure. Similarly, parameters can be restricted to a finite interval by using a modified logit transform during the maximisation procedure. The advantage of using these transformations is that the entire real line is mapped onto the positive real line or the required finite interval, respectively; and further, they are differentiable and monotonic. This eliminates the \dQuote{hard} boundaries which are sometimes enforced by using a penalty function when the estimation procedure strays into the forbidden region. The addition of such penalty functions causes the function that is being optimised to be non-differentiable at the boundaries, which can cause considerable problems with the optimisation routines.
}

\examples{
#    Random number generation method
RNGkind("Mersenne-Twister", "Inversion")
set.seed(5)

#--------------------------------------------
#    Exponential Distribution

#    simulate a sample
p <- 1
x <- rexp(n=1000, rate=p)

#    Transform to a log scale so that -infty < log(p) < infty.
#    Hence no hard boundary, and p > 0.
#    If LL is beyond machine precision, LL <- 1e20.

neg.LL <- function(logp, data){
    x <- -sum(log(dexp(data, rate=exp(logp))))
    if (is.infinite(x)) x <- 1e20
    return(x)
}

p0 <- 5
logp0 <- log(p0)
z <- nlm(neg.LL, logp0, print.level=0, data=x)
print(exp(z$estimate))

#    Compare to closed form solution
print(exp(z$estimate)-1/mean(x))

#--------------------------------------------
#    Normal Distribution

#    simulate a sample
x <- rnorm(n=1000, mean=0, sd=1)

neg.LL <- function(p, data){
    x <- -sum(log(dnorm(data, mean=p[1], sd=exp(p[2]))))
    if (is.infinite(x)) x <- 1e20
    return(x)
}

p0 <- c(2, log(2))
z <- nlm(neg.LL, p0, print.level=0, data=x)
p1 <- c(z$estimate[1], exp(z$estimate[2]))
print(p1)

#    Compare to closed form solution
print(p1 - c(mean(x), sd(x)))

#--------------------------------------------
#    Gamma Distribution
#    shape > 0 and rate > 0
#    use exponential function to ensure above constraints

#    simulate a sample
x <- rgamma(n=2000, shape=1, rate=5)

neg.LL <- function(p, data){
    #   give unreasonable values a very high neg LL, i.e. low LL
    if (any(exp(p) > 1e15)) x <- 1e15
    else{
        x <- -sum(log(dgamma(data, shape=exp(p[1]), rate=exp(p[2]))))
        if (is.infinite(x)) x <- 1e15
    }
    return(x)
}

p0 <- c(2, 2)
z <- optim(p0, neg.LL, data=x)
print(exp(z$par))

z <- nlm(neg.LL, p0, print.level=0, data=x)
print(exp(z$estimate))

#--------------------------------------------
#    Beta Distribution
#    shape1 > 0 and shape2 > 0
#    use exponential function to ensure above constraints

#    simulate a sample
x <- rbeta(n=5000, shape1=0.5, shape2=0.2)

#    exclude those where x=0
x <- x[x!=1]

neg.LL <- function(p, data)
    -sum(log(dbeta(data, shape1=exp(p[1]), shape2=exp(p[2]))))

p0 <- log(c(0.1, 0.1))

z <- optim(p0, neg.LL, data=x)
print(exp(z$par))

z <- nlm(neg.LL, p0, typsize=c(0.01, 0.01), print.level=0, data=x)
print(exp(z$estimate))

#--------------------------------------------
#    Weibull Distribution
#    shape > 0 and scale > 0
#    use exponential function to ensure above constraints

#    simulate a sample
x <- rweibull(n=2000, shape=2, scale=1)

neg.LL <- function(p, data)
    -sum(log(dweibull(data, shape=exp(p[1]), scale=exp(p[2]))))

p0 <- log(c(0.1, 0.1))
z <- optim(p0, neg.LL, data=x)
print(exp(z$par))

#--------------------------------------------
#    Pareto Distribution
#    lambda > 0
#    Use exponential function to enforce constraint

#    simulate a sample
x <- rpareto(n=2000, lambda=2, a=1)

neg.LL <- function(p, data){
    #   give unreasonable values a very high neg LL, i.e. low LL
    if (exp(p) > 1e15) x <- 1e15
    else x <- -sum(log(dpareto(data, lambda=exp(p), a=1)))
    if (is.infinite(x)) x <- 1e15
    return(x)
}

p0 <- log(0.1)
z <- nlm(neg.LL, p0, print.level=0, data=x)
print(exp(z$estimate))

#--------------------------------------------
#    Tapered Pareto Distribution
#    lambda > 0  and  theta > 0

# simulate a sample
x <- rtappareto(n=2000, lambda=2, theta=4, a=1) 

neg.LL <- function(p, data){
    x <- -ltappareto(data, lambda=p[1], theta=p[2], a=1)
    attr(x, "gradient") <- -attr(x, "gradient")
    attr(x, "hessian") <- -attr(x, "hessian")
    return(x)
}

#   use optim to get approx initial value 
p0 <- c(3, 5)
z1 <- optim(p0, neg.LL, data=x) 
p1 <- z1$par
print(p1)
print(neg.LL(p1, x))

#   nlm with analytic gradient and hessian
z2 <- nlm(neg.LL, p1, data=x, hessian=TRUE) 
p2 <- z2$estimate
print(z2)

#    Newton Raphson Method
p3 <- p1
iter <- 0
repeat{
    LL <- ltappareto(data=x, lambda=p3[1], theta=p3[2], a=1)
    p3 <- p3 - as.numeric(solve(attr(LL,"hessian")) \%*\% 
                 matrix(attr(LL,"gradient"), ncol=1))
    iter <- iter + 1
    if ((max(abs(attr(LL,"gradient"))) < 1e-8) |
        (iter > 100)) break
}
print(iter)
print(LL)
print(p3)
}

\keyword{documentation}



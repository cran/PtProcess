\name{Changes}
\alias{Changes}
\title{Changes Made to the Package}
\description{
This page contains a listing of recent changes made to functions, and known general problems.
}

\section{Recent Changes}{
\enumerate{
\item Version 3 contains major changes, and code that worked in Version 2 will no longer work in Version 3. The models included in Version 2 are also contained in Version 3, but the framework has been extended so that the original models can now contain a variety of mark distributions. This has been achieved by giving a more general structure and utilising the object orientated aspects of the \R\ language. \emph{Examples are given below that show how models were defined in Version 2 and how the corresponding models are now defined in Version 3.} (28 Apr 2008)

\item Naming changes to the \code{*.cif} functions. In Version 2, these were referred to as \dQuote{conditional intensity functions}, which is really a slightly more general class. In keeping with Daley \& Vere-Jones (2003) we now call them ground intensity functions, with a suffix of \dQuote{gif}. Further, the dot has been replaced by an underscore, e.g. \code{etas.cif} to \code{etas_gif}. This is to lessen the possibility of future conflicts with object orientated naming conventions in the \R\ language. (28 Apr 2008)

\item Arguments \code{eval.pts} and \code{t.plus} in the ground intensity functions have been renamed to \code{evalpts} and \code{tplus}, respectively. This is to lessen the possibility of future conflicts with object orientated naming conventions in the \R\ language. (28 Apr 2008)

\item \code{\link{logLik}}: The log-likelihood calculated in package Versions before Version 3 did not have the sum over the mark density term (see topic \code{\link{logLik}}, under \dQuote{Details}). This term can also be excluded in this Version of the package by placing \code{NULL} for the mark density in the \code{\link{mpp}} object, see example below. (28 Apr 2008)

\item Version 2 had a framework to assign prior densities to the estimated parameters. This has not been retained in Version 3. However, some of the features like holding a parameter at a fixed value, and restricting it to an open or closed interval can be achieved in Version 3; see \code{\link{neglogLik}} for further details. (28 Apr 2008)

}}


%\section{Required Changes or Checking}{
%\enumerate{
%\item 
%}}


\section{Future Development}{
\enumerate{
\item In the model object, allow one to alternatively specify the \code{name} of the gif function.

\item The methods for \code{simulate} do not have the stopping condition that was in Version 2. This means that events are simulated on the specified interval \code{TT}, and cannot be stopped if an unreasonably too many or too few events are generated, etc.

\item Function \code{\link{linksrm_gif}}: Use of \code{St1} and \code{St2}. Is there a tidier way? Also utilise this feature in \code{\link{srm_gif}}.

\item Need models to deal with space. How far can one go using the present \code{\link{mpp}} framework? Is it necessary to define another more general model class? Is there another class of models similar in nature to the class defined for \code{\link{linksrm}}?

\item Want a generic function, possibly called \code{forecast}, to produce probability forecasts. This would be based on simulating empirical probability distributions.

\item Want a function like \code{\link{linksrm_convert}} to map between the two main parameterisations of the ETAS model.

\item Add general forms of the truncated exponential and gamma distributions as marks for the magnitude of the event.

\item Check whether the mark functions can simply refer to \code{gif} (if they require its value). It (\code{gif}) has been defined in the method functions.
}}

\examples{
#    SRM: magnitude is iid exponential with bvalue=1
#    simulate and calculate the log-likelihood

TT <- c(0, 1000)
bvalue <- 1
params <- c(-1.5, 0.01, 0.8, bvalue*log(10))

#   --- Old Method ---
# x <- pp.sim(NULL, params[1:3], srm.cif, TT, seed=5, magn.sim=1)
# print(pp.LL(x, srm.cif, params[1:3], TT))
# [1] -601.3941

#   --- New Method, no mark density ---
x1 <- mpp(data=NULL,
          gif=srm_gif,
          mark=list(NULL, rexp_mark),
          params=params,
          gmap=expression(params[1:3]),
          mmap=expression(params[4]),
          TT=TT)
x1 <- simulate(x1, seed=5)
print(logLik(x1))

#  An advantage of the object orientated format is that it
#  simplifies further analysis, e.g. plot intensity function:
plot(x1)
#  plot the residual process:
plot(residuals(x1))

#---------------------------------------------------
#    SRM: magnitude is iid exponential with bvalue=1
#    simulate then estimate parameters from data

#   --- Old Method ---
# TT <- c(0, 1000)
# bvalue <- 1
# params <- c(-2.5, 0.01, 0.8)
#
# x <- pp.sim(NULL, params, srm.cif, TT, seed=5, magn.sim=1)
#
# posterior <- make.posterior(x, srm.cif, TT)
#
# neg.posterior <- function(params){
#     x <- -posterior(params)
#     if (is.infinite(x) | is.na(x)) return(1e15)
#     else return(x)
# }
#
# z <- nlm(neg.posterior, params, typsize=abs(params),
#          iterlim=1000, print.level=2)
#
# print(z$estimate)
# [1] -2.83900091  0.01242595  0.78880647

#   --- New Method, no mark density ---
#   maximise only SRM parameters (like old method)

TT <- c(0, 1000)
bvalue <- 1
params <- c(-2.5, 0.01, 0.8, bvalue*log(10))

x1 <- mpp(data=NULL,
          gif=srm_gif,
          mark=list(dexp_mark, rexp_mark),
          params=params,
          gmap=expression(params[1:3]),
          mmap=expression(params[4]),
          TT=TT)
#  note that dexp_mark above is not used below
#  and could alternatively be replaced by NULL

x1 <- simulate(x1, seed=5)

#  maximise only SRM parameters
onlysrm <- function(y, p){
    #  maps srm parameters into model object
    #  the exp rate for magnitudes is unchanged
    y$params[1:3] <- p
    return(y)
}

params <- c(-2.5, 0.01, 0.8)

z1 <- nlm(neglogLik, params, object=x1, updatep=onlysrm,
          print.level=2, iterlim=500, typsize=abs(params))
print(z1$estimate)
}

\keyword{documentation}


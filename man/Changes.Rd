\name{Change Log}
\alias{Changes}
\title{Changes Made to the Package}
\description{
This page contains a listing of recent changes made to functions, and known general problems.
}

\section{Recent Changes}{
\enumerate{
\item Version 3 contains major changes, and code that worked in Version 2 will no longer work in Version 3. The models included in Version 2 are also contained in Version 3, but the framework has been extended so that the original models can now contain a variety of mark distributions. This has been achieved by giving a more general structure and utilising the object orientated aspects of the \R language. \emph{Examples are given below that show how models were defined in Version 2 and how the corresponding models are now defined in Version 3.} (28 Apr 2008)

\item Naming changes to the \code{*.cif} functions. In Version 2, these were referred to as \dQuote{conditional intensity functions}, which is really a slightly more general class. In keeping with Daley & Vere-Jones (2003) we now call them ground intensity functions, with a suffix of \dQuote{gif}. Further, the dot has been replaced by an underscore, e.g. \code{etas.cif} to \code{etas_gif}. This is to lessen the possibility of future conflicts with object orientated naming conventions in the \R language. (28 Apr 2008)

\item Arguments \code{eval.pts} and \code{t.plus} in the ground intensity functions have been renamed to \code{evalpts} and \code{tplus}, respectively. This is to lessen the possibility of future conflicts with object orientated naming conventions in the \R language. (28 Apr 2008)

\item \code{\link{logLik}}: the log-likelihood calculated in package Versions before Version 3 did not have the sum over the mark density term (see topic \code{\link{logLik}}, under \dQuote{Details}). This term can also be excluded in this Version of the package by placing \code{NULL} for the mark density in the \code{\link{mpp}} object, see example below. (28 Apr 2008)

\item Version 2 had a framework to assign prior densities to the estimated parameters. This has not been retained in Version 3. However, some of the features like holding a parameter at a fixed value, and restricting it to an open or closed interval can be achieved in Version 3; see \code{\link{neglogLik}} for further details. (28 Apr 2008)

\item \code{\link{neglogLik}}: the format of this function has been changed to be consistent with that in package \pkg{HiddenMarkov}. Argument \code{updatep} renamed as \code{pmap}. (07 Aug 2008)

\item \code{\link{simulate}}: manual page revised to include more information about controlling the length of the simulated series. (18 Nov 2008)

\item \code{\link{mpp}}: example modified due to warning messages caused by negative \eqn{\lambda_g(t|{\cal H}_t)}{lambda_g(t|Ht)}. (18 Nov 2008)

\item \code{\link{marks}}: manual page revised to include more information. (18 Nov 2008)

\item \code{\link{mpp}}: fuller description to argument \code{marks} on manual page. (19 Nov 2008)

\item \code{\link{Phuket}}: new dataset added. (4 Dec 2008)

\item \code{\link{linksrm_gif}}, \code{\link{marks}}: remove some LaTeX specific formatting to be compatible with \R 2.9.0. (26 Jan 2009)

\item \code{\link{Phuket}}: clarify magnitude scale used in the dataset. (11 Jul 2009)

\item Attribute \code{type} is no longer required on the \code{\link{gif}} functions, removed. (7 Oct 2009)

\item \code{\link{logLik}}, \code{\link{neglogLik}}: Parallel processing support, using package \pkg{snow}, has been added. (8 Oct 2009)

\item \code{\link{plot}}: Correct hyperlink to generic plot function. (10 Oct 2009)

\item \code{etas_normal0}: New function. Test version of a spatial ETAS conditional intensity function. (12 Oct 2009)

\item \code{\link{logLik}}: Fixed bug when using parallel processing on only two nodes. (22 Oct 2009)

\item Tidied HTML representation of equations in manual pages. Removal of \dQuote{synopsis} on manual pages of functions with multiple forms of usage. (26 Jan 2010)

\item \code{\link{logLik.mpp}}, \code{\link{summary.mpp}}: Changed to \code{\link{inherits}} to determine class. (27 Jan 2010)

\item \code{\link{Phuket}}: Additional data, until the beginning of 2009, have been added. The magnitude is now the maximum of the body wave and surface wave magnitudes, \eqn{m_b} and \eqn{M_s}, respectively. Earlier it was simply \eqn{m_b}. (01 Feb 2010)

\item \code{\link{simulate.linksrm}}, \code{\link{simulate.mpp}}, \code{\link{logLik.mpp}}: Inconsistency in nomenclature between \dQuote{mark} and \dQuote{marks}, will standardise on the plural. (07 May 2010)

\item \code{\link{simulate.mpp}}: Two bugs: \cr
\code{use <- (data[, "time"] < TT[1])} changed to \code{use <- (data[, "time"] <= TT[1])}, \cr
and \code{else data <- data[use, c("time", "magnitude")]} changed to \cr
\code{else data <- data[use, ]}. (18 Jun 2010)

\item \code{etas_normal0}: Errors in some terms involving \code{beta}. (18 Jun 2010)

\item Minor citation and reference inclusion changes to manual pages. (19 Jul 2010)

\item \code{\link{simulate.mpp}}: Bug fix on 18 June 2010 induced another bug;\cr
\code{data <- rbind(data, newevent)} changed to\cr
\code{data <- rbind(data[, names(newevent)], newevent)}. (11 Dec 2010)

\item Implement very basic NAMESPACE. (5 Nov 2011)

\item List functions explicitly in NAMESPACE; ``\code{LazyData: no}'' and ``\code{ZipData: no}'' in DESCRIPTION file. (9 Dec 2011)

\item \code{\link{logLik.mpp}}: Enable one to specify the relative CPU speeds of the nodes when parallel processing. (9 Dec 2011)

\item \code{\link{mpp}} and \code{etas_normal0}: Restrict the number of iterations in examples on manual pages to minimise time during package checks. (13 Dec 2011)

\item \code{\link{residuals}} and \code{\link{linksrm}}: Include example using cusum of residuals on manual page. (15 Dec 2011)

\item \code{\link{dpareto}}, \code{\link{dtappareto}}, \code{\link{ltappareto}} (etc): Include parameter consistency checks. (6 Jan 2014)

\item \code{\link{etas_gif}}: Documentation example error: \code{marks=list(rmagn_mark, rmagn_mark)} should be \code{marks=list(dmagn_mark, NULL)}. (23 Jan 2014)

\item \code{linksrm1_gif}: Function deleted, alternative discussed on manual page of \code{\link{linksrm_gif}}. (19 Mar 2014)

\item Correct html problem in \file{inst/doc/index.html}. (14 Aug 2014)

\item \code{\link{logLik.mpp}}: Call to \code{clusterApply} changed to \code{snow::clusterApply}. (20 Aug 2014)

\item \code{\link{logLik.mpp}}: The package \pkg{snow} has been superseded by \pkg{parallel}. Change \code{snow} to \code{parallel}, also in file \file{DESCRIPTION}. (15 Oct 2014)

\item \code{\link{makeSOCKcluster}}: This function is in \pkg{snow} but not in \pkg{parallel}. This function points to the closest eqivalent in \pkg{parallel}, \code{\link[parallel]{makePSOCKcluster}}. \code{\link{makeSOCKcluster}} will eventually become deprecated. Was added to the export list in file \file{NAMESPACE} too. (15 Oct 2014)

\item \code{\link{logLik.mpp}}, \code{\link{neglogLik}}: Update consistent with changes from \pkg{snow} to \pkg{parallel}. (17 Oct 2014)

\item \code{\link{logLik.mpp}}: Change \code{require(parallel)} to \code{requireNamespace("parallel")}. (21 Jan 2015)

\item Added to NAMESPACE: \cr
\code{importFrom(graphics, plot)} \cr
\code{importFrom(stats, dexp, integrate, logLik, pnorm,} \cr
\code{           qexp, rexp, runif, simulate, ts)} \cr
(03 Jul 2015)

\item \code{\link{PtProcess}}: Add DOI to some references, rename topic to appear first in table of contents. (16 Oct 2015)

\item \code{\link{plot.mpp}}: Activate argument \code{ylim}. (17 Aug 2016)

\item \code{etas_normal0}: This has been removed. Adding a spatial dimension requires more generality in other package functions like \code{\link{logLik.mpp}}. For a reasonable amount of generality, it requires the addition of new model class, currently under development. (01 Sep 2016)

\item \code{\link{simulate.mpp}}: Did not allow argument \code{marks = list(NULL, NULL)} in \code{\link{mpp}} object. \cr \code{simulate.mpp} now tests to see if \code{NULL} marks. (17 Nov 2017)

\item \code{\link{fourier_gif}}: Example added on manual page with \code{NULL} marks. (17 Nov 2017)

\item \code{\link{Phuket}}: Hyperlink to data source updated, others updated to https where possible. (24 Apr 2021)

\item Updated \file{DESCRIPTION} file. (03 Jun 2025)
}}


%\section{Required Changes or Checking}{
%\enumerate{
%\item
%}}


\section{Future Development}{
\enumerate{
\item Currently spatial versions of the ETAS model are being written and tested.

\item In the model object, allow one to alternatively specify the \code{name} of the gif function.

\item Function \code{\link{linksrm_gif}}: Use of \code{St1} and \code{St2}. Is there a tidier way? Also utilise this feature in \code{\link{srm_gif}}.

\item Want a generic function, possibly called \code{forecast}, to produce probability forecasts. This would be based on simulating empirical probability distributions.

\item Want a function like \code{\link{linksrm_convert}} to map between the two main parametrisations of the ETAS model.

\item Add general forms of the truncated exponential and gamma distributions as marks for the magnitude of the event.

\item A tidy way to pass the values of the \code{gif} function into the mark distributions, if required.
}}

\references{
Cited references are listed on the \link{PtProcess} manual page.
}

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
          marks=list(NULL, rexp_mark),
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
          marks=list(dexp_mark, rexp_mark),
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

z1 <- nlm(neglogLik, params, object=x1, pmap=onlysrm,
          print.level=2, iterlim=500, typsize=abs(params))
print(z1$estimate)
}

\keyword{documentation}



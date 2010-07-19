\name{etas_spatial}
\Rdversion{1.1}
\alias{etas_spatial}
\alias{etas_normal0}

\title{
Template Function for Spatial ETAS
}

\description{
This is a template function for a spatial ETAS model. The spatial component is based on the shape of the bivariate normal density function. \emph{\bold{This function is currently in development and may change, see \dQuote{Warnings} below.}}
}

\usage{
etas_normal0(data, evalpts, params, fixedparams, TT=NA,
             tplus=FALSE)
}

\arguments{
\item{data}{
a data frame containing the event history, where each row represents one event. There must be columns named \code{"time"}, usually the number of days from some origin; \code{"magnitude"} which is the event magnitude less the magnitude threshold, i.e. \eqn{M_i - M_0}{Mi - Mo}; \code{"latitude"}, denoted below as \eqn{y}{y}; and \code{"longitude"}, denoted below as \eqn{x}{x}.
}
\item{evalpts}{
a \code{\link{matrix}} or \code{\link{data.frame}} containing columns named \code{"time"}, \code{"magnitude"}, \code{"latitude"}, and \code{"longitude"}.
}
\item{params}{
vector of parameter values in the following order: \eqn{(\mu, A, \alpha, c, p, d_x, d_y, \beta)}{(mu, A, alpha, c, p, dx, dy, beta)}.
}
\item{fixedparams}{
vector of values defining the spatial boundaries (degrees) of the region, assumed to be rectangular: \eqn{(X_1, X_2, Y_1, Y_2)}{(X1, X2, Y1, Y2)}.
}
\item{TT}{
vector of length 2, being the time interval over which the integral of the ground intensity function is to be evaluated.
}
\item{tplus}{logical, \eqn{\lambda(t,x,y|{\cal H}_t)}{lambda(t,x,y|Ht)} is evaluated as \eqn{\lambda(t^+,x,y|{\cal H}_t)}{lambda(t^+,x,y|Ht)} if \code{TRUE}, else \eqn{\lambda(t^-,x,y|{\cal H}_t)}{lambda(t^-,x,y|Ht)}.
}
}

\details{
Let
\deqn{
f(t) = (1+t/c)^{-p}
}
and
\deqn{
g(x,y) = \exp\left(-\frac{x^2}{2\sigma_x^2} - \frac{y^2}{2\sigma_y^2} \right) \,,
}{
g(x,y) = exp(-x^2/(2*sigma_x^2) - y^2/(2*sigma_y^2) ) ,
}
where \eqn{\sigma_x^2 = d_x e^{\alpha(M_i-M_0)}}{sigma_x^2 = d_x exp{alpha(Mi-Mo)}} and \eqn{\sigma_y^2 = d_y e^{\alpha(M_i-M_0)}}{sigma_y^2 = d_y exp{alpha(Mi-Mo)}}. Then the conditional intensity function is
\deqn{
\lambda(t,x,y|{\cal H}_t) = \mu + A \sum_{i: t_i < t} e^{\beta(M_i-M_0)} g(x-x_i, y-y_i) f(t-t_i) \,.
}{
lambda(t,x,y|Ht) = mu + A SUM_{i: ti < t} exp{beta(Mi-Mo)} g(x-xi, y-yi) f(t-ti) .
}
Note that the parameters \eqn{\mu}{mu} and \eqn{A} are not the same as those in \code{\link{etas_gif}}. That function is the spatial integral of the above, and so, for example, \eqn{\widetilde{\mu} = (X_2-X_1)(Y_2-Y_1)\mu}{tilde{mu} = (X2-X1)(Y2-Y1)mu} where \eqn{\widetilde{\mu}}{tilde{mu}} is the \eqn{\mu}{mu} in \code{\link{etas_gif}}. Ogata & Zhuang (2006) have discussed this and other formulations of the spatial ETAS models.

This can be thought of as a template function, i.e. a function that can be used to define the required spatial intensity function. It is probably most efficient to embed the boundaries of the region under analysis into the function. Say we want the scaling for latitude and longitude to be the same (i.e. \eqn{d=d_x=d_y}), \eqn{\beta=0}{beta=0}, and the analysed region is contained within \eqn{89^\circ}{89deg}E--\eqn{105^\circ}{105deg}E, and \eqn{5^\circ}{5deg}S--\eqn{16^\circ}{16deg}N. This is then the same as case (5) in Ogata & Zhuang (2006) with their \eqn{S_i}{Si} being the identity matrix. The conditional intensity function would then be defined as follows.
\preformatted{
etas_tmp <- function(data, evalpts, params, TT=NA, tplus=FALSE)
    etas_normal0(data, evalpts, params=c(params, params[6], 0),
                 fixedparams=c(89, 105, -5, 16),
                 TT=TT, tplus=tplus)
}
The vector \code{params} of the new function \code{etas_tmp} is \eqn{(\mu, A, \alpha, c, p, d)}{(mu, A, alpha, c, p, d)}.
}

\section{Warnings}{
This is function is still in development and may often change. The code may well be inefficient, though the current purpose is to keep it transparent and hence easier to change.

When this function is used to build a model object (\code{\link{mpp}}), the model object will work correctly with the generic functions \code{\link{summary}}, \code{\link{residuals}}, \code{\link{neglogLik}} and \code{\link{logLik}}, but will not yet work with \code{\link{simulate}} and \code{\link{plot}}.
}

\value{
The function has two forms of usage as described in \code{\link{gif}}. The first usage returns a vector containing the values of \eqn{\lambda(t,x,y)}{lambda(t,x,y)} evaluated at the specified points. In the second usage, it returns the value of the space-time integral.
}

\seealso{
General details about the structure of ground intensity functions are given in the topic \code{\link{gif}}.
}

\references{
Cited references are listed on the \link{PtProcess} manual page.
}

\examples{
data(Phuket)
#    magnitudes are rounded to 1 dp
Phuket$magnitude <- Phuket$magnitude - 4.95

TT <- c(0, 1827)
params <- c(0.0001, 73, 1.3, 0.01, 1.0, 0.01)

etas_tmp <- function(data, evalpts, params, TT=NA, tplus=FALSE){
    # etas_normal0 params = c(mu, A, alpha, c, p, dx, dy, beta)
    # etas_tmp params = c(mu, A, alpha, c, p, d)
    # want: d=dx=dy and alpha=beta
    etas_normal0(data, evalpts, params=c(params, params[6], 0),
                 fixedparams=c(89, 105, -5, 16),
                 TT=TT, tplus=tplus)
}

x <- mpp(data=Phuket,
         gif=etas_tmp,
         marks=list(NULL, NULL),
         params=params,
         gmap=expression(params),
         mmap=NULL,
         TT=TT)

allmap <- function(y, p){
    y$params <- exp(p)
    return(y)
}

#    Note: the "Not run" blocks below are not run during package checks
#    as the makeSOCKcluster definition is specific to my network,
#    modify accordingly if you want parallel processing.

cl <- NULL
\dontrun{
if (require(snow)){
    cl <- makeSOCKcluster(c("horoeka.localdomain", "horoeka.localdomain",
                            "kowhai.localdomain", "kowhai.localdomain",
                            "localhost", "localhost"))
    clusterExport(cl, c("etas_normal0"))
}
}

initial <- log(params)
#  set iterlim larger to converge satisfactorily
#  needs about 47 iterations
z <- nlm(neglogLik, initial, object=x, pmap=allmap,
         print.level=2, iterlim=5, SNOWcluster=cl)

\dontrun{
if (!is.null(cl)){
    stopCluster(cl)
    rm(cl)
}
}

x <- allmap(x, z$estimate)
print(z$code)

print(x$params)
#  MLE = (1.139316e-04 7.258019e+01 1.304519e+00 8.839539e-03
#         9.999858e-01 9.299687e-03)

print(logLik(x))
# -2954.28

#   The integral term should be equal to the number
#   of events in TT (1248)
print(sum((x$data$time > TT[1]) & (x$data$time < TT[2])))
print(etas_tmp(x$data, NULL, x$params, TT=TT))
}

\keyword{models}


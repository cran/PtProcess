\name{linksrm_gif}
\alias{linksrm_gif}
\alias{linksrm1_gif}
\title{
Ground Intensity for Linked Stress Release Model
}
\description{
Calculates the value of the ground intensity of a Linked Stress Release Model (LSRM). This model allows for multiple linked regions, where the stress can be transferred between the regions.
}

\synopsis{
linksrm_gif(data, evalpts, params, TT=NA, tplus=FALSE, eta=0.75)
}

\usage{
linksrm_gif(data, evalpts, params, tplus=FALSE, eta=0.75)
linksrm_gif(data, evalpts=NULL, params, TT, eta=0.75)

linksrm1_gif(data, evalpts, params, tplus=FALSE, eta=0.75)
linksrm1_gif(data, evalpts=NULL, params, TT, eta=0.75)
}

\arguments{
\item{data}{
a data frame containing the event history, where each row represents one event. There must be columns named \code{"time"}, usually the number of days from some origin; \code{"magnitude"} which is the event magnitude less the magnitude threshold, i.e. \eqn{M_k - M_0}{Mk - M0}; and \code{"region"} which are consecutively numbered starting at 1.
}
\item{evalpts}{
a matrix of evaluation points at which the ground intensity is to be calculated. The matrix must include two columns named \code{"time"} and \code{"region"} at which the intensity function will be evaluated.
}
\item{params}{
vector of parameters of length \eqn{n^2+2n}, where \eqn{n} is the number of regions, for the proposed LSRM in the following order:
\deqn{\code{c}(a_1, \cdots, a_n, b_1, \cdots, b_n, c_{11}, c_{12}, c_{13}, \cdots, c_{nn}).}{\code{c}(a_1, \dots, a_n, b_1, \dots, b_n, c_{11}, c_{12}, c_{13}, \dots, c_{nn}).}
}
\item{TT}{
vector of length 2, being the time interval over which the integral of the ground intensity function is to be evaluated.
}
\item{tplus}{logical, \eqn{\lambda_g(t,i|{\cal H}_t)}{lambda_g(t,i|Ht)} is evaluated as \eqn{\lambda_g(t^+,i|{\cal H}_t)}{lambda_g(t^+,i|Ht)} if \code{TRUE}, else \eqn{\lambda_g(t^-,i|{\cal H}_t)}{lambda_g(t^-,i|Ht)}.
}
\item{eta}{a scalar used in the stress calculations, see Details below.}
}
\value{
The first usage returns a vector containing the values of \eqn{\lambda_g(t,i)}{lambda_g(t,i)} evaluated at the specified \dQuote{time-region} points. In the second usage, it returns a vector containing the value of the integral for each region.
}
\details{
The ground intensity for the \eqn{i}th region is assumed to have the form
\deqn{
\lambda_g(t,i | {\cal H}_t) = \exp\left\{ a_i + b_i\left[t - \sum_{j=1}^n c_{ij} S_j(t)\right]\right\}
}{
lambda_h(t,i) = exp{ a_i + b_i*[t - sum_j c_{ij} S_j(t)]}
}
with \eqn{\code{params} = \code{c}(a_1, \cdots, a_n, b_1, \cdots, b_n, c_{11}, c_{12}, c_{13}, \cdots, c_{nn})}{\code{params} = \code{c}(a_1, \dots, a_n, b_1, \dots, b_n, c_{11}, c_{12}, c_{13}, \dots, c_{nn})}; and
\deqn{S_j(t) = \sum_k 10^{\eta(M_k-M_0)},}{S_j(t) = \sum_k 10^{\eta(M_k-M_0)},}
where the summation is taken over those events in region \eqn{j} with time \eqn{t_k < t}. This model has been discussed by Bebbington \& Harte (2001, 2003). The default value of \eqn{\eta = \code{eta} = 0.75}{\code{eta} = 0.75}.

The difference between \code{linksrm_gif} and \code{linksrm1_gif} is that the stress reduction matrices \code{St1} and \code{St2} (internal to both functions) are calculated every time that the function is called in the case of \code{linksrm_gif}. If the event \emph{history} is not changing between successive calls (e.g. parameter estimation), then this is unnecessary. However, in a simulation, the history changes with the addition of each new event. The function \code{linksrm1_gif} checks to see whether the matrices \code{St1} and \code{St2} exist. If so, these existing matrices are used, and new ones are not calculated. Therefore when using \code{linksrm1_gif} for parameter estimation, one \bold{must} check for the existence of such matrices, and delete upon starting to fit a new model, for example: \newline
\code{if (exists("St1")) rm(St1)}
\newline
\code{if (exists("St2")) rm(St2)}
}

\section{Function Attributes}{
\describe{
\item{\code{rate}}{is \code{"increasing"}.}
\item{\code{type}}{is \code{"linksrm"}.}
\item{\code{regions}}{is \code{expression(sqrt(length(params) + 1) - 1)}.}
}}

\section{Problems and Inconsistencies}{
It would be better if the objects \code{St1} and \code{St2} could be dealt with in a tidier manner. This is the only difference between \code{linksrm_gif} and \code{linksrm1_gif}.
}

\seealso{
General details about the structure of ground intensity functions are given in the topic \code{\link{gif}}.
}

\keyword{models}

\name{linksrm_gif}
\alias{linksrm_gif}
\title{
Ground Intensity for Linked Stress Release Model
}
\description{
Calculates the value of the ground intensity of a Linked Stress Release Model (LSRM). This model allows for multiple linked regions, where the stress can be transferred between the regions.
}

\usage{
linksrm_gif(data, evalpts, params, TT=NA, tplus=FALSE, eta=0.75)
}


\arguments{
\item{data}{
a data frame containing the event history, where each row represents one event. There must be columns named \code{"time"}, usually the number of days from some origin; \code{"magnitude"} which is the event magnitude less the magnitude threshold, i.e. \eqn{M_k - M_0}{Mk - M0}; and \code{"region"} which are consecutively numbered starting at 1.
}
\item{evalpts}{
a \code{\link{matrix}} or \code{\link{data.frame}}. It must include two columns named \code{"time"} and \code{"region"} that can be referred to as \code{evalpts[,"time"]} and \code{evalpts[,"region"]}, respectively. The function will be evaluated at these points.
}
\item{params}{
vector of parameters of length \eqn{n^2+2n}, where \eqn{n} is the number of regions, for the proposed LSRM in the following order:
\deqn{
(a_1, \cdots, a_n, b_1, \cdots, b_n, c_{11}, c_{12}, c_{13}, \cdots, c_{nn}).
}{
(a_1, ..., a_n, b_1, ..., b_n, c_{11}, c_{12}, c_{13}, ..., c_{nn}).}
}
\item{TT}{
vector of length 2, being the time interval over which the integral of the ground intensity function is to be evaluated.
}
\item{tplus}{logical, \eqn{\lambda_g(t,i|{\cal H}_t)}{lambda_g(t,i|Ht)} is evaluated as \eqn{\lambda_g(t^+,i|{\cal H}_t)}{lambda_g(t^+,i|Ht)} if \code{TRUE}, else \eqn{\lambda_g(t^-,i|{\cal H}_t)}{lambda_g(t^-,i|Ht)}.
}
\item{eta}{a scalar used in the stress calculations, see Details below.}
}
\value{
Two usages are as follows.
\preformatted{
linksrm_gif(data, evalpts, params, tplus=FALSE, eta=0.75)
linksrm_gif(data, evalpts=NULL, params, TT, eta=0.75)
}
The first usage returns a vector containing the values of \eqn{\lambda_g(t,i)}{lambda_g(t,i)} evaluated at the specified \dQuote{time-region} points. In the second usage, it returns a vector containing the value of the integral for each region.
}
\details{
The ground intensity for the \eqn{i}th region is assumed to have the form
\deqn{
\lambda_g(t,i | {\cal H}_t) = \exp\left\{ a_i + b_i\left[t - \sum_{j=1}^n c_{ij} S_j(t)\right]\right\}
}{
lambda_h(t,i) = exp{ a_i + b_i*[t - sum_j c_{ij} S_j(t)]}
}
with \code{params}\eqn{= \code{c}(a_1, \cdots, a_n, b_1, \cdots, b_n, c_{11}, c_{12}, c_{13}, \cdots, c_{nn})}{ = (a_1, ..., a_n, b_1, ..., b_n, c_{11}, c_{12}, c_{13}, ..., c_{nn})}; and
\deqn{
S_j(t) = \sum_k 10^{\eta(M_k-M_0)},
}{
S_j(t) = SUM_k 10^{eta(M_k-M_0)},
}
where the summation is taken over those events in region \eqn{j} with time \eqn{t_k < t}. This model has been discussed by Bebbington & Harte (2001, 2003). The default value of \eqn{\eta = \code{eta} = 0.75}{eta = 0.75}.
}

\section{Function Attributes}{
\describe{
\item{\code{rate}}{is \code{"increasing"}.}
\item{\code{regions}}{is \code{expression(sqrt(length(params) + 1) - 1)}.}
}}

\section{Modify Function to Decrease Calculation Time}{
The function \code{linksrm_gif} calculates the stress reduction matrices \code{St1} and \code{St2} every time that the function is called. Ideally, these should be calculated once and be included within the model object. Currently, the structure of the model object is not sufficiently flexible. However, the user can create a new function to calculate \code{St1} and \code{St2} once. This will only work if the event \emph{history} is not changing between successive calls (e.g. parameter estimation). However, in a simulation, the history changes with the addition of each new event, and in this situation \code{St1} and \code{St2} need to be calculated with every function call.

The modified function, as described below, will write the objects \code{St1} and \code{St2} to a temporary database (position 2 in the search path). Consequently, it cannot be defined within the package itself because this violates the CRAN rules. The function \code{linksrm_gif} contains markers indicating the beginning and ending of the parts where \code{St1} and \code{St2} are calculated. The modified function is made by editing the function \code{linksrm_gif}. We firstly \code{\link{deparse}} the function \code{linksrm_gif} (i.e. put the contents into a character vector). We initially create a temporary database called \code{PtProcess.tmp} in which to write \code{St1} and \code{St2}. We then search for the line numbers that mark the beginning and ending of the parts where \code{St1} and \code{St2} are calculated. We replace the beginning of each with a conditional statement so that the contents are only run if these two objects do not already exist. We then \code{\link{parse}} the lines of code in the character vector back into a function, and call this new function \code{linksrm1_gif}. The same thing can be achieved by dumping \code{linksrm_gif} to a text file and editing manually.
\preformatted{
#   define linksrm1_gif by modifying linksrm_gif

#   put function linksrm_gif into a character vector
tmp <- deparse(linksrm_gif)

#   remove "if (FALSE)" lines
linenum <- grep("if \\(FALSE\\)", tmp)
tmp <- tmp[-linenum]

#   attach new database at pos=2 in search path called PtProcess.tmp
linenum <- grep("attach new database to search path", tmp)
tmp[linenum] <- "if (!any(search()==\"PtProcess.tmp\")) attach(NULL,
                      pos=2L, name=\"PtProcess.tmp\", warn.conflicts=TRUE)"

#   calc St1 if St1 does not exist
linenum <- grep("this loop calculates St1", tmp)
tmp[linenum] <- "if (!exists(\"St1\", mode = \"numeric\")) {"
linenum <- grep("assign statement for St1", tmp)
tmp[linenum] <- "assign(\"St1\", St1, pos=\"PtProcess.tmp\")"
linenum <- grep("end loop St1", tmp)
tmp[linenum] <- "}"


#   calc St2 if St2 does not exist
linenum <- grep("this loop calculates St2", tmp)
tmp[linenum] <- "if (!exists(\"St2\", mode = \"numeric\")) {"
linenum <- grep("assign statement for St2", tmp)
tmp[linenum] <- "assign(\"St2\", St2, pos=\"PtProcess.tmp\")"
linenum <- grep("end loop St2", tmp)
tmp[linenum] <- "}"

linksrm1_gif <- eval(parse(text=tmp))
}
\emph{Warning}: The function \code{linksrm1_gif} checks to see whether the matrices \code{St1} and \code{St2} exist. If so, these existing matrices are used, and new ones are not calculated. Therefore when using \code{linksrm1_gif} for parameter estimation, one \bold{must} check for the existence of such matrices, and delete upon starting to fit a new model:
\preformatted{
if (exists("St1")) rm(St1)
if (exists("St2")) rm(St2)
}
or detach the database as \code{detach(2)}. The objects \code{St1} and \code{St2} will exist for the duration of the current R session, so should be deleted when no longer required.
}

\seealso{
General details about the structure of ground intensity functions are given in the topic \code{\link{gif}}.
}

\references{
Cited references are listed on the \link{PtProcess} manual page.
}

\keyword{models}

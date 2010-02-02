\name{linksrm_convert}
\alias{linksrm_convert}
\title{
Parameter Conversion for Linked Stress Release Model
}
\description{
Converts parameter values between two different parameterisations (described in Details below) of the linked stress release model.
}
\usage{
linksrm_convert(params, abc=TRUE)
}
\arguments{
\item{params}{
a vector of parameter values of length \eqn{n^2+2n}{n^2+2n}, where \eqn{n} is the number of regions in the model.
}
\item{abc}{
logical. If \code{TRUE} (default), then the input value of params is that of the \code{abc} parameterisation. See Details for further explanation.
}}
\value{
A list object with the following components is returned:

\item{params}{
vector as specified in the function call.
}
\item{a}{
vector of length \eqn{n} as in the \code{abc} parameterisation.
}
\item{b}{
vector of length \eqn{n} as in the \code{abc} parameterisation.
}
\item{c}{
n by \eqn{n} matrix as in the \code{abc} parameterisation.
}
\item{alpha}{
vector of length \eqn{n} as in the alternative parameterisation.
}
\item{nu}{
vector of length \eqn{n} as in the alternative parameterisation.
}
\item{rho}{
vector of length \eqn{n} as in the alternative parameterisation.
}
\item{theta}{
n by \eqn{n} matrix with ones on the diagonal as in the alternative parameterisation.
}}
\details{
If \code{abc == TRUE}, the conditional intensity for the \eqn{i}th region is assumed to have the form
\deqn{
\lambda_g(t,i | {\cal H}_t) = \exp\left\{ a_i + b_i\left[t - \sum_{j=1}^n c_{ij} S_j(t)\right]\right\}
}{
lambda_g(t,i) = exp{ a_i + b_i*[t - sum_j c_{ij} S_j(t)]}
}
with \code{params}\eqn{ = (a_1, \cdots, a_n, b_1, \cdots, b_n, c_{11}, c_{12}, c_{13}, \cdots, c_{nn})}{ = (a_1, ..., a_n, b_1, ..., b_n, c_{11}, c_{12}, c_{13}, ..., c_{nn})}.

If \code{abc == FALSE}, the conditional intensity for the \eqn{i}th region is assumed to have the form
\deqn{
\lambda_g(t,i | {\cal H}_t) = \exp\left\{ \alpha_i + \nu_i\left[\rho_i t - \sum_{j=1}^n \theta_{ij} S_j(t)\right]\right\}
}{
lambda_g(t,i) = exp{ alpha_i + nu_i*[rho_i*t - sum_j theta_{ij} S_j(t)]}
}
where \eqn{\theta_{ii}=1}{theta_{ii}=1} for all \eqn{i}, \eqn{n = \sqrt{\code{length(params)} + 1} - 1}{n = sqrt(length(params) + 1) - 1}, and 
\code{params}\deqn{ = (\alpha_1, \cdots, \alpha_n, \nu_1, \cdots, \nu_n, \rho_1, \cdots, \rho_n, \theta_{12}, \theta_{13}, \cdots, \theta_{1n}, \theta_{21}, \theta_{23}, \cdots, \theta_{n,n-1}).}{ = (alpha_1, ..., alpha_n, nu_1, ..., nu_n, rho_1, ..., rho_n, theta_{12}, theta_{13}, ..., theta_{1n}, \theta_{21}, theta_{23}, ..., theta_{n,n-1}).}
}

\seealso{
\code{\link{linksrm_gif}}
}

\keyword{models}

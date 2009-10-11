\name{Phuket}
\alias{Phuket}
\non_function{}
\title{Phuket Earthquake and Aftershock Sequence}
\usage{data(Phuket)}
\description{
The Phuket earthquake occurred on 26 December 2004 at 00:58:53.45 GMT. The \code{Phuket} data frame contains this event and its aftershock sequence.
}
\details{The \code{Phuket} data frame contains those events (1111) from the PDE Catalogue, within the spatial region \eqn{89^\circ}{89&#176;}E--\eqn{105^\circ}{105&#176;}E and \eqn{5^\circ}{5&#176;}S--\eqn{16^\circ}{16&#176;}N, with magnitude 5 or greater, occurring between midnight on 1 January 2004 and midnight on 1 January 2008 (1461 days later). The tabulated magnitudes are the body wave magnitudes. They are determined by the amplitude of the initial primary wave, and these magnitudes tend to saturate for higher values. Consequently, the tabulated magnitudes for the Phuket Boxing Day event (26 December 2004) and the Sumatra event (28 March 2005) are quite small compared to magnitudes derived on other scales.}

\format{
  This data frame contains the following columns:
  \describe{
    \item{latitude}{number of degrees north.}
    \item{longitude}{number of degrees east.}
    \item{depth}{depth of event in kilometres.}
    \item{magnitude}{event magnitude rounded to one decimal place.}
    \item{year}{year of event (numeric vector).}
    \item{month}{month of event, 1 \dots 12 (numeric vector).}
    \item{day}{day of event, 1 \dots 31 (numeric vector).}
    \item{hour}{hour of event, 0 \dots 23 (numeric vector).}
    \item{minute}{minute of event, 0 \dots 59 (numeric vector).}
    \item{second}{second of event, 0 \dots 59 (numeric vector).}
    \item{time}{number of days (and fractions) from midnight on 1 January 2004.}
  }
}

\source{
The data were extracted from the PDE (Preliminary Determination of Epicentres) catalogue provided by the US Geological Survey (\url{ftp://hazards.cr.usgs.gov/pde/}).
}

\examples{
data(Phuket)
print(Phuket[1:10,])
}

\keyword{datasets}

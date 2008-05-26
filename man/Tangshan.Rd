\name{Tangshan}
\alias{Tangshan}
\non_function{}
\title{Tangshan Earthquake and Aftershock Sequence}
\usage{data(Tangshan)}
\description{
The Tangshan earthquake occurred on 28 July 1976 at 03:42:53, with a magnitude of 7.9. The \code{Tangshan} data frame contains those events (455) from the Beijing Catalogue, within 100\,km of the epicentre and with magnitude 4 or greater, from the beginning of 1974 to the end of 1984.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{latitude}{number of degrees north.}
    \item{longitude}{number of degrees east.}
    \item{magnitude}{number of magnitude units \emph{above} 4.}
    \item{year}{year of event (numeric vector).}
    \item{month}{month of event, 1 \dots 12 (numeric vector).}
    \item{day}{day of event, 1 \dots 31 (numeric vector).}
    \item{hour}{hour of event, 0 \dots 23 (numeric vector).}
    \item{minute}{minute of event, 0 \dots 59 (numeric vector).}
    \item{second}{second of event, 0 \dots 59 (numeric vector).}
    \item{time}{number of days (and fractions) from the beginning of 1974.}
  }
}

\source{
These data originate from the Beijing Catalogue which is administered by the China Seismological Bureau, Beijing.
}

\examples{
data(Tangshan)
print(Tangshan[1:10,])
}

\keyword{datasets}

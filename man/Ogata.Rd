\name{Ogata}
\alias{Ogata}
\title{
Ogata's ETAS Test Data
}
\description{
A data frame containing the test data from Utsu and Ogata's (1997) software contained in the file testetas.dat. The first column is named \code{"time"}, and the second column is named \code{"magnitude"}.
}
\usage{data(Ogata)
}
\format{
A data frame with 100 rows (earthquake events) in the time interval (0, 800). It contains the following variables:
\describe{
\item{time}{number of time units since time zero.}
\item{magnitude}{number of magnitude units \emph{above} 3.5.}
}}

\references{
Cited references are listed on the \link{PtProcess} manual page.
}

\examples{
data(Ogata)
plot(Ogata$time, Ogata$magnitude + 3.5, type="h")
}
\keyword{datasets}

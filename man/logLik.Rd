\name{logLik}
\alias{logLik}
\alias{logLik.mpp}
\alias{logLik.linksrm}

\title{Log Likelihood of a Point Process Model}

\description{
Calculates the log-likelihood of a point process. Provides methods for the generic function \code{\link[stats]{logLik}}.
}

\usage{
\method{logLik}{mpp}(object, SNOWcluster=NULL, ...)
\method{logLik}{linksrm}(object, ...)
}

\arguments{
  \item{object}{an object with class \code{"\link{mpp}"} or \code{"\link{linksrm}"}.}
  \item{SNOWcluster}{an object of class \code{"cluster"} created by the package \pkg{parallel}; default is \code{NULL}. Enables parallel processing if not \code{NULL}. See \dQuote{Parallel Processing} below for further details.}
  \item{\dots}{other arguments.}
}

\value{
Value of the log-likelihood.
}

\section{Parallel Processing}{
Parallel processing can be enabled to calculate the term \eqn{\sum_i \log \lambda_g(t_i|{\cal H}_{t_i})}{SUM_i log lambda_g(ti|H_ti)}. Generally, the amount of computational work involved in calculating \eqn{\lambda_g(t|{\cal H}_t)}{lambda_g(t|H_t)} is much greater if there are more events in the process history prior to \eqn{t} than in the case where there are fewer events. Given \eqn{m} nodes, the required evaluation points are divided into \eqn{m} groups, taking into account the amount of \dQuote{history} prior to each event and the CPU speed of the node (see below).

We have assumed that communication between nodes is fairly slow, and hence it is best to allocate the work in large chunks and minimise communication. If the dataset is small, then the time taken to allocate the work to the various nodes may in fact take more time than simply using one processor to perform all of the calculations.

The required steps in initiating parallel processing are as follows.
\preformatted{
#   load the "parallel" package
library(parallel)

#   define the SNOW cluster object, e.g. a SOCK cluster
#   where each node has the same R installation.
cl <- makeSOCKcluster(c("localhost", "horoeka.localdomain", 
                        "horoeka.localdomain", "localhost"))

#   A more general setup: Totara is Fedora, Rimu is Debian:
#   Use 2 processors on Totara, 1 on Rimu:
totara  <- list(host="localhost",
                rscript="/usr/lib/R/bin/Rscript",
                snowlib="/usr/lib/R/library")
rimu    <- list(host="rimu.localdomain",
                rscript="/usr/lib/R/bin/Rscript",
                snowlib="/usr/local/lib/R/site-library")
cl <- makeCluster(list(totara, totara, rimu), type="SOCK")

#   NOTE: THE STATEMENTS ABOVE WERE APPROPRIATE FOR THE snow PACKAGE.
#   I HAVE NOT YET TESTED THEM USING THE parallel PACKAGE.

#   Relative CPU speeds of the nodes can be added as an attribute
#   Say rimu runs at half the speed of totara
#   (default assumes all run at same speed)
attr(cl, "cpu.spd") <- c(1, 1, 0.5)

#   then define the required model object, e.g. see topic "mpp"
#   say the model object is called x

#   then calculate the log-likelihood as
print(logLik(x, SNOWcluster=cl))

#   stop the R jobs on the slave machines
stopCluster(cl)
}
Note that the communication method does not need to be \code{SOCKS}; see the \pkg{parallel} package documentation, topic \code{\link[parallel]{makeCluster}}, for other options. Further, if some nodes are on other machines, the firewalls may need to be tweaked. The master machine initiates the \R jobs on the slave machines by communicating through port 22 (use of security keys are needed rather than passwords), and subsequent communications use random ports. This port can be fixed, see \code{\link[parallel]{makeCluster}}.
}

\examples{
#    SRM: magnitude iid exponential with bvalue=1

TT <- c(0, 1000)
bvalue <- 1
params <- c(-2.5, 0.01, 0.8, bvalue*log(10))

#   calculate log-likelihood excluding the mark density term
x1 <- mpp(data=NULL,
          gif=srm_gif,
          marks=list(NULL, rexp_mark),
          params=params,
          gmap=expression(params[1:3]),
          mmap=expression(params[4]),
          TT=TT)
x1 <- simulate(x1, seed=5)
print(logLik(x1))

#   calculate log-likelihood including the mark density term
x2 <- mpp(data=x1$data,
          gif=srm_gif,
          marks=list(dexp_mark, rexp_mark),
          params=params,
          gmap=expression(params[1:3]),
          mmap=expression(params[4]),
          TT=TT)
print(logLik(x2))

#  contribution from magnitude marks
print(sum(dexp(x1$data$magnitude, rate=bvalue*log(10), log=TRUE)))
}

\keyword{methods}

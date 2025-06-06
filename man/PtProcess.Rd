\name{PtProcess-package}
\alias{PtProcess-package}
\alias{PtProcess}
\docType{package}

\title{Overview of PtProcess Package}

\description{
This topic gives an introductory overview to the package \pkg{PtProcess}. Links are given to follow up topics where more detail can be found.
}

\section{Introduction}{
This package contains routines for the fitting of \emph{time dependent} point process models, particularly marked processes with \dQuote{jumps}. These models have particular application to earthquake data. A detailed theoretical background to these and other point process models can be found in Daley & Vere-Jones (2003, 2008). An overview of the package structure is given by Harte (2010).

The direction of the development of the package has been influenced by our research on the application of point process models to seismology. The package was originally written for S-PLUS, being part of the Statistical Seismology Library (Harte, 1998; Brownrigg & Harte, 2005). The package \pkg{ptproc} by Peng (2002, 2003) analyses multi-dimensional point process models, and the package \pkg{spatstat} by Baddeley et al (2005, 2005a, 2008) analyses spatial point processes.

\emph{The topic \code{\link{Changes}} lists recent changes made to the package. Version 3 of the package has some major changes from Version 2, and code for Version 2 will not work in Version 3 without modification. Some examples giving the old code and the required new code are given in the topic \code{\link{Changes}}. Changes made in Version 3 enable one to fit a more general class of model.}
}

\section{Classes of Point Process Models Analysed}{
The classes of models currently fitted by the package are listed below. Each are defined within an object that contains the data, current parameter values, and other model characteristics.

\describe{
\item{Marked Point Process Model:}{
is described under the topic \code{\link{mpp}}. This model can be simulated or fitted to data by defining the required model structure within an object of class \code{"\link{mpp}"}.}
\item{Linked Stress Release Model:}{
is described under the topic \code{\link{linksrm}}. This model is slightly peculiar, and doesn't fit naturally in the \code{mpp} framework.}
}
}

\section{Main Tasks Performed by the Package}{
The main tasks performed by the package are listed below. These can be achieved by calling the appropriate generic function.
\describe{
\item{Simulation:}{can be performed by the function \code{\link{simulate}}.}
\item{Parameter Estimation:}{can be achieved by using the function \code{\link{neglogLik}}.}
\item{Model Residuals:}{can be calculated with the function \code{\link{residuals}}.}
\item{Model Summary:}{can be extracted with the function \code{\link{summary}}.}
\item{Log-Likelihood:}{can be calculated with the function \code{\link{logLik}}.}
\item{Ground Intensity Plot:}{can be performed by the function \code{\link{plot}}.}
}
The method function conforms to the following naming convention, for example, the function \code{\link{logLik.mpp}} provides the method to calculate the log-likelihood for \code{\link{mpp}} objects. The function code can be viewed by entering \code{PtProcess:::logLik.mpp} on the \R command line.

If you want to modify such a function, \code{\link{dump}} the code to your local directory, modify in a text editor, then use \code{\link{source}} at the beginning of your program script, but after \code{library(PtProcess)}. Your modified version will then be used in preference to the version in the \pkg{PtProcess} package.
}

\section{Organisation of Topics in the Package}{
\describe{
\item{Cited References:}{anywhere in the manual are only listed within this topic.}
\item{General Documentation:}{topics summarising general structure are indexed under the keyword \dQuote{documentation} in the Index.}
}
}

\section{Acknowledgements}{
The package is based on an S-PLUS package which was commenced at Victoria University of Wellington in 1996. Contributions and suggestions have been made by many, including: Mark Bebbington, Ray Brownrigg, Edwin Choi, Robert Davies, Michael Eglinton, Dongfeng Li, Li Ma, Alistair Merrifield, Andrew Tokeley, David Vere-Jones, Wenzheng Yang, Leon Young, Irina Zhdanova and Jiancang Zhuang.
}

\references{
Aalen, O.O. & Hoem, J.M. (1978). Random time changes for multivariate counting processes. \emph{Scandinavian Journal of Statistics} \bold{5}, 81--101. \doi{10.1080/03461238.1978.10419480}

Baddeley, A. (2008). \emph{Open source software for spatial statistics.} URL: \url{http://spatstat.org/}.

Baddeley, A. & Turner, R. (2005). Spatstat: an R package for analyzing spatial point patterns. \emph{Journal of Statistical Software} \bold{12(6)}, 1--42. \doi{10.18637/jss.v012.i06}

Baddeley, A.; Turner, R.; Moller, J. & Hazelton, M. (2005a). Residual analysis for spatial point processes (with discussion). \emph{J. R. Statist. Soc. B} \bold{67(5)}, 617--666. \doi{10.1111/j.1467-9868.2005.00519.x}

Bebbington, M.S. & Harte, D.S. (2001). On the statistics of the linked stress release model. \emph{Journal of Applied Probability} \bold{38A}, 176--187. \doi{10.1239/jap/1085496600}

Bebbington, M.S. & Harte, D.S. (2003). The linked stress release model for spatio-temporal seismicity: formulations, procedures and applications. \emph{Geophysical Journal International} \bold{154}, 925--946. \doi{10.1046/j.1365-246X.2003.02015.x}

Brownrigg, R. & Harte, D.S. (2005). Using \R for statistical seismology. \emph{\R News} \bold{5(1)}, 31--35. URL: \url{https://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf}.

Daley, D.J. & Vere-Jones, D. (2003). \emph{An Introduction to the Theory of Point Processes. Volume I: Elementary Theory and Methods. Second Edition.} Springer-Verlag, New York. \doi{10.1007/b97277}

Daley, D.J. & Vere-Jones, D. (2008). \emph{An Introduction to the Theory of Point Processes. Volume II: General Theory and Structure. Second Edition.} Springer-Verlag, New York. \doi{10.1007/978-0-387-49835-5}

Harte, D. (1998). Documentation for the Statistical Seismology Library. School of Mathematical and Computing Sciences Research Report No. 98--10 (Updated Edition June 1999), Victoria University of Wellington. (ISSN 1174--4545)

Harte, D. (2010). PtProcess: An \R package for modelling marked point processes indexed by time. \emph{Journal of Statistical Software} \bold{35(8)}, 1--32. \doi{10.18637/jss.v035.i08}

Kagan, Y. & Schoenberg, F. (2001). Estimation of the upper cutoff parameter for the tapered Pareto distribution. \emph{Journal of Applied Probability} \bold{38A}, 158--175. \doi{10.1239/jap/1085496599}

Lewis, P.A.W. & Shedler, G.S. (1979). Simulation of nonhomogeneous Poisson processes by thinning. \emph{Naval Research Logistics Quarterly} \bold{26(3)}, 403--413. \doi{10.1002/nav.3800260304}

Ogata, Y. (1981). On Lewis' simulation method for point processes. \emph{IEEE Transactions on Information Theory} \bold{27(1)}, 23--31. \doi{10.1109/TIT.1981.1056305}

Ogata, Y. (1988). Statistical models for earthquake occurrences and residual analysis for point processes. \emph{J. Amer. Statist. Assoc.} \bold{83(401)}, 9--27. \doi{10.2307/2288914}

Ogata, Y. (1998). Space-time point-process models for earthquake occurrences. \emph{Ann. Instit. Statist. Math.} \bold{50(2)}, 379--402. \doi{10.1023/A:1003403601725}

Ogata, Y. (1999). Seismicity analysis through point-process modeling: a review. \emph{Pure and Applied Geophysics} \bold{155}, 471--507. \doi{10.1007/s000240050275}

Ogata, Y. & Zhuang, J.C. (2006). Space-time ETAS models and an improved extension. \emph{Tectonophysics} \bold{413(1-2)}, 13--23. \doi{10.1016/j.tecto.2005.10.016}

Peng, R. (2002). Multi-dimensional Point Process Models. Package \dQuote{ptproc}, URL: \url{https://rdpeng.org/}.

Peng, R. (2003). Multi-dimensional point process models in R. \emph{Journal of Statistical Software} \bold{8(16)}, 1--27. \doi{10.18637/jss.v008.i16}

Reid, H.F. (1910). The mechanism of the earthquake. In \emph{The California Earthquake of April 18, 1906, Report of the State Earthquake Investigation Commission} \bold{2}, 16--28. Carnegie Institute of Washington, Washington D.C.

Utsu, T. and Ogata, Y. (1997). Statistical analysis of seismicity. In: \emph{Algorithms for Earthquake Statistics and Prediction} (Edited by: J.H. Healy, V.I. Keilis-Borok and W.H.K. Lee), pp 13--94.  IASPEI, Menlo Park CA.

Vere-Jones, D. (1978). Earthquake prediction - a statistician's view. \emph{Journal of Physics of the Earth} \bold{26}, 129--146. \doi{10.4294/jpe1952.26.129}

Vere-Jones, D.; Robinson, R. & Yang, W. (2001). Remarks on the accelerated moment release model: problems of model formulation, simulation and estimation. \emph{Geophysical Journal International} \bold{144(3)}, 517--531. \doi{10.1046/j.1365-246x.2001.01348.x}

Zheng, X.-G. & Vere-Jones, D. (1991). Application of stress release models to historical earthquakes from North China. \emph{Pure and Applied Geophysics} \bold{135(4)}, 559--576. \doi{10.1007/BF01772406}

Zhuang, J.C. (2006). Second-order residual analysis of spatiotemporal point processes and applications in model evaluation. \emph{J. R. Statist. Soc. B} \bold{68(4)}, 635--653. \doi{10.1111/j.1467-9868.2006.00559.x}
}

\keyword{documentation}

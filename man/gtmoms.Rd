\encoding{utf8}
\name{gtmoms}
\alias{gtmoms}
\title{ Moments of Observations Above the Threshold }
\description{
Moments of observations above the threshold (\code{xsi}, \eqn{x_{si}}), which has been standardized to a zero mean and unit standard deviation. Define the standard normal hazard function as
\deqn{H(x) = \phi(x) / (1 - \Phi(x))\mbox{,}}
where \eqn{\phi(x)} is the standard normal density function and \eqn{\Phi(x)} is the standard normal distribution (cumulative) function. For a truncation index, \eqn{r}, define the recursion formula, \eqn{\Psi} for \code{gtmoms} as
\deqn{\Psi(x_{si}, r) = (r-1)\Psi(x_{si}, r-2) + x_{si}^{r-1}H(x_{si})\mbox{,}}
for which \eqn{\Psi(x_{si}, 0) = 1} and \eqn{\Psi(x_{si}, 1) = H(x_{si})}.
}
\usage{
gtmoms(xsi, r)
}
\arguments{
  \item{xsi}{The lower threshold; and}
  \item{r}{The number of truncated observations.}
}
\value{
  The moments.
}
\note{
\bold{AUTHOR TODO---}Note that is it not clear in TAC documentation that \eqn{X_{si}} is a scalar or vector quantity, and \code{gtmoms} is automatically vectoral in the \R idioms if \eqn{X_{si}} is. Also it is not immediately clear \eqn{X_{si}} is or is not one of the order statistics. Based on MGBT operation in USGS-PeakFQ output (USGS, 2014), the threshold is \dQuote{known} no better in accuracy than one of the sample order statistics, so \eqn{X_{si}} might be written \eqn{x_{[r:n]}}. But this answer could be only restricted to a implementation in software and perhaps not theory. Finally, although the computations involve the standard normal distribution, the standardization form of \eqn{X_{si}} is not yet confirmed during the WHA porting process.
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, \code{P3_089(R).txt}---Named \code{gtmoms}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.

U.S. Geological Survey (USGS), 2018, PeakFQ---Flood frequency analysis based on Bulletin 17B and recommendations of the Advisory Committee on Water Information (ACWI) Subcommittee on Hydrology (SOH) Hydrologic Frequency Analysis Work Group (HFAWG), version 7.2: Accessed November 29, 2018, at \url{https://water.usgs.gov/software/PeakFQ/}.
}
\author{ W.H. Asquith consulting T.A. Cohn sources }
\seealso{\code{\link{CondMomsChi2}}, \code{\link{gtmoms}}
}
\examples{
gtmoms(-3.561143, 2)  # Is this a meaningful example?
#[1] 0.9974952
}
\keyword{moments}
\keyword{moments (conditional)}
\keyword{utility functions}

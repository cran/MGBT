\encoding{utf8}
\name{CritK}
\alias{CritK}
\title{ Compute Critical Value of Grubbs--Beck statistic (eta) Given Probability }
\description{
Compute critical value for the Grubbs--Beck statistic (\code{eta} = \eqn{GB_r(p)}) given a probability (p-value), which is the \dQuote{pseudo-studentized} magnitude of \eqn{r}th smallest observation. The \code{CritK} function is the same as the \eqn{GB_r(p)} quantile function.  In distribution notation, this is equivalent to saying \eqn{GB_r(F)} for nonexceedance probability \eqn{F \in (0,1)}, and cumulative distribution function \eqn{F(GB_r)} is the value that comes from \code{\link{RthOrderPValueOrthoT}}.
}
\usage{
CritK(n, r, p)
}
\arguments{
  \item{n}{The number of observations;}
  \item{r}{The number of truncated observations; and}
  \item{p}{The probability value (p-value).}
}
\value{
  The critical value of the Grubbs--Beck statistic (\code{eta} = \eqn{GB_r(p)}).
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, not \code{P3_089(R).txt}---Named: \code{CritK}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.
}
\author{ W.H. Asquith  consulting T.A. Cohn sources }
\seealso{\code{\link{critK10}}
}
\examples{
\donttest{
CritK(58, 2, .001) # CPU heavy: -3.561143}
}
\keyword{critical values}
\keyword{MGBT}

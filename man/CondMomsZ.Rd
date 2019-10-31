\encoding{utf8}
\name{CondMomsZ}
\alias{CondMomsZ}
\title{ Conditional Moments: N.B. Moments employ only observations above Xsi }
\description{
Compute the \eqn{Z}-conditional moments (standard normal distributed moments) based on only those (\eqn{n} \eqn{-} \eqn{r}) observations above a threshold \eqn{X_{si}} for a sample size of \eqn{n} and \eqn{r} number of truncated observations. The first moment is \code{gtmoms(xsi,1)}, which is in the first returned column. The second moment is
\preformatted{
  (gtmoms(xsi,2) - gtmoms(xsi,1)^2)/(n-r)
} that is in the second returned column. Further mathematical details are available under \code{\link{gtmoms}}.
}
\usage{
CondMomsZ(n, r, xsi)
}
\arguments{
  \item{n}{The number of observations;}
  \item{r}{The number of truncated observations; and}
  \item{xsi}{The lower threshold (see \code{\link{gtmoms}}).}
}
\value{
  The value a two-column, one-row \R \code{matrix}.
}
\note{
The \code{CondMomsZ} function appears to not be used for any purpose. Only the \code{CondMomsChi2} function is needed for MGBT. The author WHA hypothesizes that TAC has the simple logic of this function constructed in long hand as needed within other functions---Rigorous inquiry of TAC's design purposes is not possible.
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, \code{P3_089(R).txt}---Named \code{CondMomsZ}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.
}
\author{ W.H. Asquith consulting T.A. Cohn sources }
\seealso{\code{\link{CondMomsChi2}}, \code{\link{gtmoms}}
}
\examples{
CondMomsZ(58, 2, -3.561143)
#             [,1]       [,2]
#[1,] 0.0007033727 0.01781241
}
\keyword{moments}
\keyword{moments (conditional)}
\keyword{utility functions}

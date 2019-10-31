\encoding{utf8}
\name{CondMomsChi2}
\alias{CondMomsChi2}
\title{ Conditional Moments: N.B. Moments employ only observations above Xsi }
\description{
Compute the \eqn{\chi^2}-conditional moments (Chi-squared distributed moments) based on only those (\eqn{n} \eqn{-} \eqn{r}) observations above a threshold \eqn{X_{si}} for a sample size of \eqn{n} and \eqn{r} number of truncated observations. The first moment is \code{(gtmoms(xsi,2) -} \code{gtmoms(xsi,1)^2)} that is in the first returned column. The second moment (variance of S-squared) is \code{V(n,r,pnorm(xsi))[2,2]} that is in the second returned column. Further mathematical details are available under functions \code{\link{gtmoms}} and \code{\link{V}}.
}
\usage{
CondMomsChi2(n, r, xsi)
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
TAC sources define a \code{\link{CondMomsZ}} function along with this function. However, the \code{\link{CondMomsZ}} function appears to not be used for any purpose. Only the \code{CondMomsChi2} is needed for the MGBT test.
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, \code{P3_089(R).txt}---Named \code{CondMomsChi2}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.
}
\author{ W.H. Asquith consulting T.A. Cohn sources }
\seealso{\code{\link{CondMomsZ}}, \code{\link{gtmoms}}
}
\examples{
CondMomsChi2(58, 2, -3.561143)
#          [,1]       [,2]
#[1,] 0.9974947 0.03574786

# Note that CondMomsChi2(58, 2, -3.561143)[2] == V(58, 2, pnorm(-3.561143))[2,2]
}
\keyword{moments}
\keyword{moments (conditional)}

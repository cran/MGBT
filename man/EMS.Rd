\encoding{utf8}
\name{EMS}
\alias{EMS}
\title{ Expected values of M and S }
\description{
Compute expected values of \eqn{M} and \eqn{S} given \eqn{q_\mathrm{min}} and define the quantity
\deqn{z_r = \Phi^{(1)}(q_\mathrm{min})\mbox{,}}
where \eqn{\Phi^{(1)}(\cdot)} is the inverse of the standard normal distribution. As result, \eqn{q_\mathrm{min}} is itself a probability because it is an argument to the \code{qnorm()} function. The expected value \eqn{M} is defined as
\deqn{M = \Psi(z_r, 1)\mbox{,}}
where \eqn{\Psi(a,b)} is the \code{\link{gtmoms}} function. The \eqn{S} requires the conditional moments of the Chi-square (\code{\link{CondMomsChi2}}) defined as the two value vector \eqn{\mbox{}_2S} that provides the values \eqn{\alpha = \mbox{}_2S_1^2 / \mbox{}_2S_2} and \eqn{\beta = \mbox{}_2S_2 / \mbox{}_2S_1}. The \eqn{S} is then defined by
\deqn{S = \sqrt{\beta}\biggl(\frac{\Gamma(\alpha+0.5)}{\Gamma(\alpha)}\biggr)\mbox{.}}
}
\usage{
EMS(n, r, qmin)
}
\arguments{
  \item{n}{The number of observations;}
  \item{r}{The number of truncated observations? (confirm); and}
  \item{qmin}{A nonexceedance probability threshold for \eqn{X > q_\mathrm{min}}.}
}
\value{
  The expected values of \eqn{M} and \eqn{S} in the form of an \R \code{vector}.
}
\note{
TAC sources call on the explicit first index of \eqn{M} as literally \dQuote{\code{Em[1]}} for the returned vector, which seems unnecessary. This is a potential weak point in design because the \code{gtmoms} function is naturally vectorized and could potentially produce a vector of \eqn{M} values. For the implementation here, only the first value in \code{qmin} is used and a warning otherwise issued. Such coding prevents the return value from \code{EMS} accidentally acquiring a length greater than two. For at least samples of size \eqn{n=2}, overranging in a call to \code{lgamma(alpha)} happens for \code{alpha=0}. A \code{suppressWarnings()} is wrapped around the applicable line. The resulting \code{NaN} cascades up the chain, which will end up inside \code{\link{peta}}, but therein \code{SigmaMp} is not finite and a p-value of unity is returned.
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, \code{P3_089(R).txt}---Named \code{EMS}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.
}
\author{ W.H. Asquith consulting T.A. Cohn sources }
\seealso{\code{\link{CondMomsChi2}}, \code{\link{EMS}}, \code{\link{VMS}}, \code{\link{V}}, \code{\link{gtmoms}}
}
\examples{
EMS(58,2,.5)
#[1] 0.7978846 0.5989138

#  Monte Carlo experiment to test EMS and VMS functions
"test_EMS" <- function(nrep=1000, n=100, r=0, qr=0.2, ss=1) { # TAC named function
   set.seed(ss)
   Moms <- replicate(n=nrep, {
          x <- qnorm(runif(n-r,min=qr,max=1));
          c(mean(x),var(x))}); xsi <- qnorm(qr);
          list(
    MeanMS_obs = c(mean(Moms[1,]), mean(sqrt(Moms[2,])), mean(Moms[2,])),
    EMS        = c(EMS(n,r,qr), gtmoms(xsi,2) - gtmoms(xsi,1)^2),
    CovMS2_obs = cov(t(Moms)),
    VMS2       = V(n,r,qr),
    VMS_obs    = array(c(var(     Moms[1,]),
                         rep(cov( Moms[1,], sqrt(Moms[2,])),2),
                         var(sqrt(Moms[2,]))),    dim=c(2,2)),
    VMS        = VMS(n,r,qr)  )
}
test_EMS()
}
\keyword{moments}
\keyword{moments (conditional)}
\keyword{utility functions}

\encoding{utf8}
\name{peta}
\alias{peta}
\title{ Probability of Eta }
\description{
Compute \code{peta}, which is the survival probability of the t-distribution for \code{eta =} \eqn{\eta}.

Define \eqn{b_r} as the inverse (quantile) of the Beta distribution for nonexceedance probability \eqn{F \in (0,1)} having two shape parameters (\eqn{\alpha} and \eqn{\beta}) as
\deqn{b_r = \mathrm{Beta}^{(-1)}(F; \alpha, \beta) = \mathrm{Beta}^{(-1)}(F; r, n+1-r)\mbox{,}}
for sample size \eqn{n} and number of truncated observations \eqn{r} and note that \eqn{b_r \in (0,1)}. Next, define \eqn{z_r} as the \eqn{Z}-score for \eqn{b_r}
\deqn{z_r = \Phi^{(-1)}(b_r)\mbox{,}}
where \eqn{\Phi^{(-1)}(\cdots)} is the inverse of the standard normal distribution.

Compute the covariance matrix \eqn{COV} of \eqn{M} and \eqn{S} from \code{\link{VMS}} as in \code{COV = VMS(n, r, qmin=br)}, and from which define
\deqn{\lambda = COV_{1,2} / COV_{2,2}\mbox{,}}
which is a covariance divided by a variance, and then define
\deqn{\eta_p = \lambda + \eta\mbox{.}}

Compute the expected values of \eqn{M} and \eqn{S} from \code{\link{EMS}} as in \eqn{EMp = } \code{EMp = EMS(n, r, qmin=br)}, and from which define
\deqn{\mu_{Mp} = EMp_1 - \lambda\times EMp_2\mbox{,}}
\deqn{\sigma_{Mp} = \sqrt{COV_{1,1} - COV_{1,2}^2/COV_{2,2}}\mbox{.}}

Compute the conditional moments from \code{\link{CondMomsChi2}} as in \eqn{momS2 =} \code{CondMomsChi2(n,r,zr)}, and from which define
\deqn{df = 2 momS2_1^2 / momS2_2\mbox{,}}
\deqn{\alpha = momS2_2 / momS2_1\mbox{,}}

}
\usage{
peta(pzr, n, r, eta)
}
\arguments{
  \item{pzr}{The probability level of a Beta distribution having shape1 \eqn{\alpha = r} and shape2 \eqn{\beta = n+1-r};}
  \item{n}{The number of observations;}
  \item{r}{The number of truncated observations; and}
  \item{eta}{The Grubbs--Beck statistic (\eqn{GB_r}, see \code{\link{MGBT}}).}
}
\value{
  The probability of the \code{eta} value.
}
\details{
Currently (2019), context is lost on the preformatted note of code note below. It seems possible that the intent by WHA was to leave a trail for future revisitation of the Beta distribution and its access, which exists in native \R code.
\preformatted{
      zr       <- qnorm(qbeta(the.pzr, shape1=r, shape2=n+1-r))
      CV       <- VMS(n, r, qmin=pnorm(zr))
}
}
\note{
Testing a very large streamgage dataset in Texas with GRH, shows at least one failure of the following computation was encountered for a short record streamgage numbered 08102900.
\preformatted{
  # USGS 08102900 (data sorted, 1967--1974)
  #https://nwis.waterdata.usgs.gov/nwis/peak?site_no=08102900&format=hn2
  Peaks <- c(40, 45, 53, 55, 88) # in cubic feet per second (cfs)
  MGBT(Peaks)
  # Here is the line in peta(): SigmaMp <- sqrt(CV[1,1] - CV[1,2]^2/CV[2,2])
  # *** In sqrt(CV[1, 1] - CV[1, 2]^2/CV[2, 2]) : NaNs produced
}
In implementation, a \code{suppressWarnings()} is wrapped on the \code{SigmaMp}. If the authors make no action in response to \code{NaN}, then the low-outlier threshold is 53 cubic feet per second (cfs) with a p-value for 40 cfs as 0.81 and 45 cfs as 0.0. This does not seem logical. The \code{is.finite} catch in the next line (see sources) is provisional under a \enc{naïve}{naive} assumption that the negative in the square root has barely undershot. The function is immediately exited with the returned p-value set to unity. Testing indicates that this is a favorable innate trap here within the \pkg{MGBT} package and will avoid higher up error trapping in larger application development.
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, \code{P3_089(R).txt}---Named \code{peta}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.
}
\author{ W.H. Asquith consulting T.A. Cohn source}
\seealso{\code{\link{EMS}}, \code{\link{VMS}}, \code{\link{CondMomsChi2}}, \code{\link{gtmoms}}
}
\examples{
peta(0.4, 58, 2, -2.3006)
#[1] 0.298834
}
\keyword{Grubbs--Beck statistic (probability level)}
\keyword{Grubbs--Beck statistic}

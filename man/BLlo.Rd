\encoding{utf8}
\name{BLlo}
\alias{BLlo}
\title{ Barnett and Lewis Test Adjusted for Low Outliers }
\description{
The Barnett and Lewis (1995, p. 224; \eqn{T_{\mathrm{N}3}}) so-labeled \dQuote{N3 method} with TAC adjustment to look for low outliers. The essence of the method, given the order statistics \eqn{x_{[1:n]} \le x_{[2:n]} \le \cdots \le x_{[(n-1):n]} \le x_{[n:n]}}, is the statistic
\deqn{BL_r = T_{\mathrm{N}3} =
\frac{ \sum_{i=1}^r x_{[i:n]} - r \times \mathrm{mean}\{x_{[1:n]}\} }
     {\sqrt{\mathrm{var}\{x_{[1:n]}\}}}\mbox{,}}
for the mean and variance of the observations.  Barnett and Lewis (1995, p. 218) brand this statistic as a test of the \dQuote{\eqn{k \ge 2} upper outliers} but for the \pkg{MGBT} package \dQuote{lower} applies in TAC reformulation. Barnett and Lewis (1995, p. 218) show an example of a modification for two low outliers as \eqn{(2\overline{x} - x_{[2:n]} - x_{[1:n]})/s} for the mean \eqn{\mu} and standard deviation \eqn{s}. TAC reformulation thus differs by a sign. The \eqn{BL_r} is a sum of internally studentized deviations from the mean:
\deqn{SP(t) \le {n \choose k} P\biggl(\bm{t}(n-2) > \biggr[\frac{n(n-2)t^2}{r(n-r)(n-1)-nt^2}\biggl]^{1/2}\biggr)\mbox{,}}
where \eqn{\bm{t}(df)} is the t-distribution for \eqn{df} degrees of freedom, and this is an inequality when
\deqn{t \ge \sqrt{r^2(n-1)(n-r-1)/(nr+n)}\mbox{,}}
where \eqn{SP(t)} is the probability that \eqn{T_{\mathrm{N}3} > t} when the inequality holds. For reference, Barnett and Lewis (1995, p. 491) example tables of critical values for \eqn{n=10} for \eqn{k \in 2,3,4} at 5-percent significant level are \eqn{3.18}, \eqn{3.82}, and \eqn{4.17}, respectively. One of these is evaluated in the \bold{Examples}.
}
\usage{
BLlo(x, r, n=length(x))
}
\arguments{
  \item{x}{The data values and note that base-10 logarithms of these are not computed internally;}
  \item{r}{The number of truncated observations; and}
  \item{n}{The number of observations.}
}
\value{
  The value for \eqn{BL_r}.
}
\note{
Regarding \code{n=length(x)}, it is not clear that TAC intended \code{n} to be not equal to the sample size. TAC chose to not determine the length of \code{x} internally to the function but to have it available as an argument. Also \code{\link{MGBTcohn2011}} and \code{\link{RSlo}} were designed similarly.
}
\source{
\code{LowOutliers_jfe(R).txt} and \code{LowOutliers_wha(R).txt}---Named \code{BL_N3}
}
\references{
Barnett, Vic, and Lewis, Toby, 1995, Outliers in statistical data: Chichester, John Wiley and Sons, ISBN~0--471--93094--6.

Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.
}
\author{ W.H. Asquith consulting T.A. Cohn sources }
\seealso{\code{\link{MGBTcohn2011}}, \code{\link{RSlo}}
}
\examples{
# See Examples under RSlo()

\donttest{ # WHA experiments with BL_r()
n <- 10; r <- 3; nsim <- 10000; alpha <- 0.05; Tcrit <- 3.82
BLs <- Ho <- RHS <- SPt <- rep(NA, nsim)
EQ <- sqrt(r^2*(n-1)*(n-r-1)/(n*r+n))
for(i in 1:nsim) { # some simulation results shown below
   BLs[i] <- abs(BLlo(rnorm(n), r)) # abs() correcting TAC sign convention
   t  <- sqrt( (n*(n-2)*BLs[i]^2) / (r*(n-r)*(n-1)-n*BLs[i]^2) )
   RHS[i] <- choose(n,r)*pt(t, n-2, lower.tail=FALSE)
   ifelse(t >= EQ, SPt[i] <- RHS[i], SPt[i] <- 1) # set SP(t) to unity?
   Ho[i]  <- BLs[i] > Tcrit
}
results <- c(quantile(BLs, prob=1-alpha), sum(Ho /nsim), sum(SPt < alpha)/nsim)
names(results) <- c("Critical_value", "Ho_rejected", "Coverage_SP(t)")
print(results) # minor differences are because of random number seeding
# Critical_value    Ho_rejected Coverage_SP(t)
#      3.817236       0.048200       0.050100 }
}
\keyword{low outlier (definition)}

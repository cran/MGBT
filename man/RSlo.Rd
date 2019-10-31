\encoding{utf8}
\name{RSlo}
\alias{RSlo}
\title{ Rosner RST Test Adjusted for Low Outliers }
\description{
The Rosner (1975) method or the essence of the method, given the order statistics \eqn{x_{[1:n]} \le x_{[2:n]} \le \cdots \le x_{[(n-1):n]} \le x_{[n:n]}}, is the statistic:
\deqn{RS_r =
\frac{ x_{[r:n]} - \mathrm{mean}\{x_{[(r+1)\rightarrow(n-r):n]}\} }
     {\sqrt{\mathrm{var}\{x_{[(r+1)\rightarrow(n-r):n]}\}}}\mbox{,}
}
}
\usage{
RSlo(x, r, n=length(x))
}
\arguments{
  \item{x}{The data values and note that base-10 logarithms of these are not computed internally;}
  \item{r}{The number of truncated observations; and}
  \item{n}{The number of observations.}
}
\value{
  The value for \eqn{RS_r}.
}
\note{
Regarding \code{n=length(x)}, it is not clear that TAC intended \code{n} to be not equal to the sample size. TAC chose to not determine the length of \code{x} internally to the function but to have it available as an argument. Also \code{\link{MGBTcohn2011}} and \code{\link{BLlo}} were similarly designed.
}
\source{
\code{LowOutliers_jfe(R).txt} and \code{LowOutliers_wha(R).txt}---Named \code{RST}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.

Rosner, Bernard, 1975, On the detection of many outliers: Technometrics, v. 17, no. 2, pp. 221--227.
}
\author{ W.H. Asquith consulting T.A. Cohn sources }
\seealso{\code{\link{MGBTcohn2011}}, \code{\link{BLlo}}
}
\examples{
\donttest{
# Long CPU time, arguments slightly modified to run faster and TAC had.
# TAC has maxr=n/2 (the lower tail) but WHA has changed to maxr=3 for
# speed to get the function usable during automated tests.
testMGBvsN3 <- function(n=100, maxr=3, nrep=10000) { # TAC named function
   for(r in 1:maxr) {
      set.seed(123457)
      res1 <- replicate(nrep, { x <- sort(rnorm(n))
                 c(MGBTcohn2011(x,r),BLlo(x,r),RSlo(x,r)) })
      r1 <- rank(res1[1,]); r2 <- rank(res1[2,]); r3 <- rank(res1[3,])
      v <- quantile(r1,.1); h <- quantile(r2,.1)
      plot(r1,r2)
      abline(v=v, col=2); abline(h=h, col=2)
      message(' BLlo ',r, " ", cor(r1,r2), " ", mean((r1 <= v) & (r2 <= h)))
      v <- quantile(r1,.1); h <- quantile(r2,.1)
      plot(r1,r3)
      abline(v=v, col=2); abline(h=h, col=2)
      mtext(paste0("Order statistic iteration =",r," of ",maxr))
      message('RSTlo ',r, " ", cor(r1,r3), " ", mean((r1 <= v) & (r3 <= h)))
   }
}
testMGBvsN3() #}
}
\keyword{low outlier (definition)}

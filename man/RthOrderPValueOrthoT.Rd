\encoding{utf8}
\name{RthOrderPValueOrthoT}
\alias{RthOrderPValueOrthoT}
\title{ P-value for the Rth Order Statistic }
\description{
Compute the p-value for the \eqn{r}th order statistic

\deqn{\eta(r,n) = \frac{x_{[r:n]} - \mathrm{mean}\{x_{[(r+1)\rightarrow n:n]}\}}
                 {\sqrt{\mathrm{var}\{x_{[(r+1)\rightarrow n:n]}\}}}\mbox{.}}

This function is the cumulative distribution function of the Grubbs--Beck statistic (\code{eta} = \eqn{GB_r(p)}). In distribution notation, this is equivalent to saying \eqn{F(GB_r)} for nonexceedance probability \cr
\eqn{F \in (0,1)}. The inverse or quantile function \eqn{GB_r(F)} is \code{\link{CritK}}.
}
\usage{
RthOrderPValueOrthoT(n, r, eta, n.sim=10000, silent=TRUE)
}
\arguments{
  \item{n}{The number of observations;}
  \item{r}{The number of truncated observations; and}
  \item{eta}{The pseudo-studentized magnitude of \eqn{r}th smallest observation;}
  \item{n.sim}{The sample size to attempt a Monte Carlo integration in case the numerical integration via \code{integrate()} encounters a divergent integral; and}
  \item{silent}{A logical controlling the silence of \code{try}.}
}
\value{
  The value a two-column \R \code{matrix}.
}
\note{
The extension to Monte Carlo integration in event of failure of the numerical integration an extension is by WHA. The \bold{Note} for \code{\link{MGBT}} provides extensive details in the context of a practical application.

Note that in conjunction with \code{RthOrderPValueOrthoT}, TAC provided an enhanced numerical integration interface (\code{integrateV()}) to \code{integrate()} built-in to \R. In fact, all that TAC did was wrap a vectorization scheme using \code{sapply()} on top of \code{\link{peta}}. The issue is that \code{\link{peta}} was not designed to be vectorized. WHA has simply inserted the \code{sapply} \R idiom inside \code{\link{peta}} and hence vectorizing it and removed the need in the \pkg{MGBT} package for the \code{integrateV()} function in the TAC sources.

TAC named this function with the \code{Kth} order. In code, however, TAC uses the variable \code{r}. WHA has migrated all references to \code{Kth} to \code{Rth} for systematic consistency. Hence, this function has been renamed to \code{RthOrderPValueOrthoT}.

TAC also provides a \code{KthOrderPValueOrthoTb} function and notes that it employs simple Gaussian quadrature to compute the integral much more quickly. However, it is slightly less accurate for tail probabilities. The Gaussian quadrature is from a function \code{gauss.quad.prob()}, which seems to not be found in the TAC sources given to WHA.
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, \code{P3_089(R).txt}---\cr
Named \code{KthOrderPValueOrthoT} + \code{KthOrderPValueOrthoTb}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.
}
\author{ W.H. Asquith consulting T.A. Cohn sources }
\seealso{\code{\link{MGBT}}, \code{\link{CritK}}
}
\examples{
# Running next line without the $value will show:
#0.001000002 with absolute error < 1.7e-05 # This is output from the integrate()
# function, which means that the numerical integration worked.
RthOrderPValueOrthoT(58, 2, -3.561143)$value

\donttest{
# Long CPU time
CritK(58, 2, RthOrderPValueOrthoT(58, 2, -3.561143)$value)
#[1] -3.561143  # Therefore CritK() is the inverse of this function.}

\donttest{
# Long CPU time
# Monte Carlo distribution of rth pseudo-studentized order statistic (TAC note)
testRthOrderPValueOrthoT <- function(nrep=1E4, r=2, n=100,
               test_quants = c(0.05,0.1,0.5,0.9,0.95),  ndigits=3, seed=1) {
   set.seed(seed)
   z <- replicate(nrep, { x <- sort(rnorm(n)); xr <- x[r]; x2 <- x[(r+1):n]
                         (xr - mean(x2))/sqrt(var(x2)) })
     res <- sapply(quantile(z, test_quants), function(q) {
                 c(q, RthOrderPValueOrthoT(n,r,q)$value) })
   round(res,ndigits)
}

nsim <- 1E4
for(n in 50) {   # original TAC sources had c(10,15,25,50,100,500)
   for(r in 5) { # original TAC sources had 1:min(10,floor(n/2))
      message("n=",n, " and r=",r)
      print(testRthOrderPValueOrthoT(nrep=nsim, n=n, r=r))
   }
}
# Output like this will be seen
# n=50 and r=5
#         5%    10%    50%    90%    95%
#[1,] -2.244 -2.127 -1.788 -1.523 -1.460
#[2,]  0.046  0.096  0.499  0.897  0.946
# that shows simulated percentages near the theoretical

# To get the MSE of the results (TAC note). See WHA note on a change below and
# it is suspected that TAC's "tests" might have been fluid in the sense that
# he would modify as needed and did not fully design as Examples for end users.
rr <- rep(0,10)
for(n in 50) {   # original TAC sources had c(10,15,25,50,100,500)
   for(r in 5) { # original TAC sources had 1:min(10,floor(n/2))
      message("n=",n, " and r=",r)
      for(i in 1:10) { # The [1,1] is WHA addition to get function to run.
         # extract the score for the 5% level
         rr[i] <- testRthOrderPValueOrthoT(nrep=nsim, n=n, r=r, seed=i)[1,1]
      }
      message("var (MSE):", sqrt(var(rr/100)))
   }
}
# Output like this will be seen
# n=50 and r=5
# var (MSE):6.915361322608e-05 }

\donttest{
#  Long CPU time
#  Monte Carlo computation of critical values for special cases (TAC note)
CritValuesMC <-
function(nrep=50, kvs=c(1,3,0.25,0.5), n=100, ndigits=3, seed=1,
         test_quants=c(0.01,0.10,0.50)) {
   set.seed(seed)
   k_values <- ifelse(kvs >= 1, kvs, ceiling(n*kvs))
   z  <- replicate(nrep, {
      x <- sort(rnorm(n))
      sapply(k_values, function(r) {
          xr <- x[r]; x2 <- x[(r+1):n]
         (xr-mean(x2)) / sqrt(var(x2)) })  })
   res <- round(apply(z, MARGIN=1, quantile, test_quants), ndigits)
   colnames(res) <- k_values; return(res)
}

# TAC example. Note that z acquires its square dimension from test_quants
# but Vr is used in the sapply(). WHA has reset Vr to
n=100; nrep=10000; test_quants=c(.05,.5,1); Vr=1:10 # This Vr by TAC
z <- CritValuesMC(n=n, nrep=nrep, test_quants=test_quants)
Vr <- 1:length(z[,1]) # WHA reset of Vr to use TAC code below. TAC Vr bug?
HH <- sapply(Vr, function(r) RthOrderPValueOrthoT(n, r, z[1,r])$value)
TT <- sapply(Vr, function(r) RthOrderPValueOrthoT(n, r, z[2,r])$value) #}
}
\keyword{hypothesis test}

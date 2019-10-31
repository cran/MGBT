\encoding{utf8}
\name{VMS}
\alias{VMS}
\title{  Covariance matrix of M and S }
\description{
Compute the covariance matrix of \eqn{M} and \eqn{S} given \eqn{q_\mathrm{min}}.  Define the vector of four moment expectations
\deqn{E_{i\in 1,2} = \Psi\bigl(\Phi^{(-1)}(q_\mathrm{min}), i\bigr)\mbox{,}}
where \eqn{\Psi(a,b)} is the \code{\link{gtmoms}} function and \eqn{\Phi^{(-1)}} is the inverse of the standard normal distribution. Define the scalar quantity \eqn{Es = } \code{EMS(n,r,qmin)[2]} as the expectation of \eqn{S} using the \code{\link{EMS}} function, and define the scalar quantity \eqn{E_s^2 = E_2 - E_1^2} as the expectation of \eqn{S^2}. Finally, compute the covariance matrix \eqn{COV} of \eqn{M} and \eqn{S} using the \code{\link{V}} function:
\deqn{COV_{1,1} = V_{1,1}\mbox{,}}
\deqn{COV_{1,2} = COV_{2,1} = V_{1,2}/2Es\mbox{,}}
\deqn{COV_{2,2} = E_s^2 - (E_s)^2\mbox{.}}
}
\usage{
VMS(n, r, qmin)
}
\arguments{
  \item{n}{The number of observations;}
  \item{r}{The number of truncated observations; and}
  \item{qmin}{A nonexceedance probability threshold for \eqn{X > q_\mathrm{min}}.}
}
\value{
  A 2-by-2 covariance \code{matrix}.
}
\note{
Because the \code{gtmoms} function is naturally vectorized and TAC sources provide no protection if \code{qmin} is a vector (see \bold{Note} under \code{\link{EMS}}). For the implementation here, only the first value in \code{qmin} is used and a warning issued if it is a vector.
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, \code{P3_089(R).txt}---Named \code{VMS}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.
}
\author{ W.H. Asquith consulting T.A. Cohn sources }
\seealso{\code{\link{EMS}}, \code{\link{V}}, \code{\link{gtmoms}}
}
\examples{
VMS(58,2,.5) # Note that [1,1] is the same as [1,1] for Examples under V().
#            [,1]        [,2]
#[1,] 0.006488933 0.003279548
#[2,] 0.003279548 0.004682506
}
\keyword{moments}
\keyword{moments (conditional)}
\keyword{utility functions}

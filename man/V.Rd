\encoding{utf8}
\name{V}
\alias{V}
\title{  Covariance matrix of M and S-squared }
\description{
Compute the covariance matrix of \eqn{M} and \eqn{S^2} (S-squared) given \eqn{q_\mathrm{min}}. Define the vector of four moment expectations
\deqn{E_{i\in 1,2,3,4} = \Psi\bigl(\Phi^{(-1)}(q_\mathrm{min}), i\bigr)\mbox{,}}
where \eqn{\Psi(a,b)} is the \code{\link{gtmoms}} function and \eqn{\Phi^{(-1)}} is the inverse of the standard normal distribution. Using these \eqn{E}, define a vector \eqn{C_{i\in 1,2,3,4}} as a system of nonlinear combinations:
\deqn{C_1 = E_1\mbox{,}}
\deqn{C_2 = E_2 -     E_1^2\mbox{,}}
\deqn{C_3 = E_3 - 3E_2E_1 + 2E_1^3\mbox{, and}}
\deqn{C_4 = E_4 - 4E_3E_1 + 6E_2E_1^2 - 3E_1^4\mbox{.}}
Given \eqn{k = n - r} from the arguments of this function, compute the symmetrical covariance matrix \eqn{COV} with variance of \eqn{M} as
\deqn{COV_{1,1} = C_2/k\mbox{,}}
the covariance between \eqn{M} and \eqn{S^2} as
\deqn{COV_{1,2} = COV_{2,1} = \frac{C_3}{\sqrt{k(k-1)}}\mbox{, and}}
the variance of \eqn{S^2} as
\deqn{COV_{2,2} = \frac{C_4 -  C_2^2}{k} + \frac{2C_2^2}{k(k-1)}\mbox{.}}
}
\usage{
V(n, r, qmin)
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
Because the \code{\link{gtmoms}} function is naturally vectorized and TAC sources provide no protection if \code{qmin} is a vector (see \bold{Note} under \code{\link{EMS}}). For the implementation here, only the first value in \code{qmin} is used and a warning issued if it is a vector.
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, \code{P3_089(R).txt}---Named \code{V}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.
}
\author{ W.H. Asquith consulting T.A. Cohn sources }
\seealso{\code{\link{EMS}}, \code{\link{VMS}}, \code{\link{gtmoms}}
}
\examples{
V(58,2,.5)
#            [,1]        [,2]
#[1,] 0.006488933 0.003928333
#[2,] 0.003928333 0.006851120
}
\keyword{moments}
\keyword{moments (conditional)}
\keyword{utility functions}

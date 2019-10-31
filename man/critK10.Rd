\encoding{utf8}
\name{critK10}
\alias{critK10}
\title{ Single Grubbs--Beck Critical Values for 10-percent Test as used in Bulletin 17B }
\description{
Return the critical values at the 10-percent (\eqn{\alpha_\mathrm{17B} = 0.10}) significance level for the single Grubbs--Beck test as in Bulletin 17B (IACWD, 1982).
}
\usage{
critK10(n)
}
\arguments{
  \item{n}{The number of observations.}
}
\value{
  The critical value for sample size \eqn{n} unless it is outside the range \eqn{10 \le n \le 149} for which the critical value is \code{NA}.
}
\note{
In the context of \code{critK10}, TAC defines a \code{.kngb()} function, which is recast as \code{KJRS()} in the \bold{Examples}. The function appears to be an approximation attributable to Jery R. Stedinger. The \bold{Examples} show a \dQuote{test} as working notes of TAC.
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, not \code{P3_089(R).txt}---Named \code{critK10}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.

Interagency Advisory Committee on Water Data (IACWD), 1982, Guidelines for determining flood flow frequency: Bulletin 17B of the Hydrology Subcommittee, Office of Water Data Coordination, U.S. Geological Survey, Reston, Va., 183 p.
}
\author{ W.H. Asquith consulting T.A. Cohn sources }
\seealso{\code{\link{CritK}}
}
\examples{
critK10(58)
#[1] 2.824

\donttest{ # Modified slightly from TAC sources (Original has the # Not run:)
# KJRS() is the ".kngb()" function in TAC sources
n <- 10:149; KJRS <- function(n) -0.9043+3.345*sqrt(log10(n))-0.4046*log10(n)
result <- data.frame(n=n, Ktrue=sapply(n, critK10), # 17B single Grubbs--Beck
                          KJRS= sapply(n, KJRS   )) # name mimic of TAC sources }

\dontrun{ # Near verbatim from TAC sources, GGBK() does not work, issues a stop().
# KJRS() is the ".kngb()" function in TAC sources
n <- 10:149; KJRS <- function(n) -0.9043+3.345*sqrt(log10(n))-0.4046*log10(n)
result <- data.frame(n=n, Ktrue=sapply(n, critK10), # 17B single Grubbs--Beck
                          KJRS= sapply(n, KJRS   ), # name mimic of TAC sources
                          KTAC= sapply(n, GGBK   )) # name mimic of TAC sources}
}
\keyword{Bulletin 17B}
\keyword{Single Grubbs--Beck Test (critical values)}
\keyword{critical values}

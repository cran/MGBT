\encoding{utf8}
\name{GGBK}
\alias{GGBK}
\title{ Cohn Approximation for New Generalized Grubbs--Beck Critical Values for 10-Percent Test }
\description{
Compute Cohn's approximation of critical values at the 10-percent significance level for the new generalized Grubbs--Beck test.
}
\usage{
GGBK(n)
}
\arguments{
  \item{n}{The number of observations.}
}
\value{
  The critical value of the test was to be returned, but a \code{stop()} is issued instead because there is a problem with the function's call of \code{\link{CritK}} (see \bold{Note}).
}
\note{
In TAC sources, \code{GGBK} is the consumer of two global scope functions \code{fw()} and \code{fw1()}. These should be defined within the function to keep the scope local as they are unneeded anywhere else in TAC sources, and these thus have local scope in the implementation for the \pkg{MGBT} package.

\bold{A BUG FIX NEEDED---}Note that TAC has a problem in sources in that this function is incomplete. The function \code{\link{CritK}} is the issue, that function requires three arguments and appears to work (see \bold{Examples} under \code{\link{CritK}}), but TAC code passes four in the context of \code{GGBK}. At present (packaging of \pkg{MGBT}), it is not known if the word \dQuote{generalized} in this test has the same meaning as \dQuote{multiple} in the Multiple Grubbs--Beck Test. Also in TAC sources, it is as yet unclear what the \dQuote{new} in the title of this function means.
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, not \code{P3_089(R).txt}---Named \code{GGBK}
}
\references{
Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.
}
\author{ W.H. Asquith consulting T.A. Cohn sources }
\seealso{\code{\link{critK10}}, \code{\link{CritK}}
}
\examples{
\dontrun{
GGBK(34) # but presently the function does not work }
}
\keyword{Generalized Grubbs--Beck Test (critical values)}

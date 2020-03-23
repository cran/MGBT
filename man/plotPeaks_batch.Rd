\encoding{utf8}
\name{plotPeaks_batch}
\alias{plotPeaks_batch}
\title{ Plot for More than One Streamgage that Peak Streamflows with Emphasis on Peak Discharge Qualification Codes }
\description{
Plot U.S. Geological Survey peak streamflows for multiple streamgages. This function is a wrapper on calls to \code{\link{plotPeaks}} with data retrieval occurring just ahead.
}
\usage{
plotPeaks_batch(sites, file=NA, do_plot=TRUE,
                       silent=FALSE, envir=NULL, ...)
}
\arguments{
  \item{sites}{A list of USGS site identification numbers for which one-by-one, the peaks will be pulled from \code{dataRetrieval::}\code{readNWISpeak};}
  \item{file}{A portable document format output path and file name, setting to \code{NA} will plot into the running application unless \code{do_plot=FALSE};}
  \item{do_plot}{A logical triggering the plotting operations. This is a useful feature for test or for a iterative use as seen in the second \bold{Example}. A setting of false will set \code{file=NA} internally regardless of argument given;}
  \item{silent}{A logical to control status messaging;}
  \item{envir}{An optional and previously populated enviroment by site number storing a table of peaks from the \code{\link{splitPeakCodes}} function (see \bold{Examples}); and}
  \item{...}{Additional arguments to pass to \code{\link{plotPeaks}}.}
}
\value{
  A list is returned by streamgage of the retrieved data with an attribute \code{empty_sites} storing those site numbers given for which no peaks were retrieved/processed. The data structure herein recognizes a \code{NA} for a site.
}
\author{ W.H. Asquith}
\references{
U.S. Geological Survey, 2019, USGS water data for the Nation: U.S. Geological Survey National Water Information System database, accessed October 11, 2019, at \url{https://doi.org/10.5066/F7P55KJN}.
}
\seealso{\code{\link{plotPeaks}}
}
\examples{
\donttest{
  # The dataRetrieval package is not required by MGBT algorithms, but needed
  # for the the plotPeaks_batch() function.
  sites <- c("07358570", "07358280", "07058980", "07258000")
  PK <- plotPeaks_batch(sites, xlab="WATER YEAR",         lot=0, 
                               ylab="STREAMFLOW, IN CFS", file=NA) # }

\donttest{
  # In this example, two calls to plotPeaks_batch() are made. The first is to use
  # the function as a means to cache the retrieval of the peaks without the 
  # plotting overhead etc. The second is to actually perform the plotting.
  pdffile <- "./peaks.pdf"
  sites <- c("08106300", "08106310", "08106350", "08106500") # 08106350 no peaks

  PK <- plotPeaks_batch(sites, do_plot=FALSE)    # a hack to use its wrapper on
  # dataRetrieval::readNWISpeak() to get the peaks retrieved and code parsed by
  # splitPeakCodes() and then we can save the PK for later purposes as needed.

  empty_sites <- attr(PK, "empty_sites") # we harvest 08106350 as empty
  message("EMPTY SITES: ", paste(empty_sites, collapse=", "))

  PK <- as.environment(PK) # now flipping to environment for the actually plotting
                           # plotting pass to follow next and retrieval is not made
  # save(empty_sites, PK, file="PEAKS.RData") # a way to save the work for later
  PK <- plotPeaks_batch(sites, xlab="WATER YEAR",        lot=0, envir=PK,
                               ylab="STREAMFLOW, IN CFS", file=pdffile)
  message("Peaks plotted in file=", pdffile) # }
}
\keyword{graphics}

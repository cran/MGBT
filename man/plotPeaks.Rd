\encoding{utf8}
\name{plotPeaks}
\alias{plotPeaks}
\title{ Plot Peak Streamflows with Emphasis on Peak Discharge Qualification Codes }
\description{
Plot U.S. Geological Survey peak streamflows in \code{peak_va} and discharge qualifications codes in the \code{peak_cd} columns of a peak-streamflow data retrieval from the National Water Information System (NWIS) (U.S. Geological Survey, 2019). This code makes use of the \code{add.log.axis} function from the \pkg{lmomco} package, and because this is the only instance of this dependency, the \pkg{lmomco} package is treated as a suggested package and not as a dependency for the \pkg{MGBT} package. This function is used for the basis of the logarithmic plot within the \code{\link{plotFFQevol}} function.

This function accommodates the plotting of various nuances of the peak including less than (code 4), greater than (code 8), zero peaks (plotted by a green tick on the horizontal axis), and peaks that are missing but the gage height was available (plotted by a light-blue tick on the horizontal axis if the \code{showGHyrs} argument is set). So-called code 5, 6, and 7 peaks are plotted by the numeric code as the plotting symbol, and so-called code C peaks are plotted by the letter \dQuote{C.} The very unusual circumstances of codes 3 and O are plotted by the letters \dQuote{D} (dam failure) and \dQuote{O} (opportunistic), respectively. These codes are are summarized within \code{\link{splitPeakCodes}}. The greater symbology set is described in the directory \code{MGBT/inst/legend} of the package sources.
}
\usage{
plotPeaks(x, codes=TRUE, lot=NULL, site="",
             xlab="", ylab="", xlim=NULL, ylim=NULL,
             xlim.inflate=TRUE, ylim.inflate=TRUE, aux.y=NULL,
             log.ticks=c(1, 2, 3, 5, 8),
             show48=FALSE, showDubNA=FALSE, showGHyrs=TRUE, ...)
}
\arguments{
  \item{x}{A \code{data.frame} having a mandatory column titled \code{peak_va} and \code{peak_cd}. It is advised to have run \code{\link{makeWaterYear}} and \code{\link{splitPeakCodes}} first, but if columns resulting from those two functions are are not detected, then the water year column and code split are made internally but not returned;}
  \item{codes}{A logical to trigger use of character plotting characters and not symbols;}
  \item{lot}{The low-outlier threshold, but if omitted, then \code{\link{MGBT}} is triggered internally;}
  \item{site}{An optional character string for a plot title, and in practice, this is expected to be a streamgage location;}
  \item{xlab}{An optional x-label of the plot;}
  \item{ylab}{An optional y-label of the plot;}
  \item{xlim}{An optional x-limit of the plot;}
  \item{ylim}{An optional y-limit of the plot;}
  \item{xlim.inflate}{A logical to trigger nudging the horizontal axis left/right to the previous/future decade;}
  \item{ylim.inflate}{A logical to trigger nudging the vertical axis down/up to the nearest sane increment of log10-cycles. This inflation also includes a \eqn{\pm}0.01 log10-cycle adjustment to the lower and upper values of the limit. This ensures (say) that a 50,000 cubic feet per second maximum, which is on a sane increment, is nudged up enough to make the upper limit 60,000 instead. The point itself plots at 50,000 cfs;}
  \item{aux.y}{An optional set of values to pack into the data just ahead of the the vertical axis limits computation;}
  \item{log.ticks}{The argument \code{logs} of the \code{add.log.axis} function;}
  \item{show48}{A logical, if \code{codes} is set, will draw a \dQuote{4} and \dQuote{8} for those respective codes instead of a reddish dot;}
  \item{showDubNA}{A logical, if set, will draw a \eqn{\pm}half-year polygon for the water years having both \code{NA} discharge (\code{peak_va}) and \code{NA} gage height (\code{gage_ht}). To some analysts, it might be useful to study the degree of these double empty entries in contrast to just the records of such years not even being present in NWIS. Double empty entries could represent incomplete data handling on part of the USGS or inadvertently missing zero discharges for \code{peak_va};}
  \item{showGHyrs}{A logical to trigger the light-blue special ticks for the water years having only gage height. The horizontal limits will be inflated accordingly if gage heights are outside the general discharge record; and}
  \item{...}{Additional arguments to pass to \code{plot}.}
}
\value{
  No values are returned; this function is used for its graphical side effects.
}
\author{ W.H. Asquith}
\references{
U.S. Geological Survey, 2019, USGS water data for the Nation: U.S. Geological Survey National Water Information System database, accessed October 11, 2019, at \url{https://doi.org/10.5066/F7P55KJN}.
}
\seealso{\code{\link{makeWaterYear}}, \code{\link{splitPeakCodes}}, \code{\link{plotFFQevol}}
}
\examples{
\donttest{
  # The dataRetrieval package is not required by MGBT algorithms.
  # Note that makeWaterYear() is not needed because splitPeakCodes() requires
  # the water_yr for gap analyses, and will call makeWaterYear() if it needs to.
  PK <- dataRetrieval::readNWISpeak("08167000", convertType=FALSE)
  PK <- splitPeakCodes(makeWaterYear(PK))
  plotPeaks(PK, codes=TRUE, showGHyrs=FALSE,
                xlab="Water year", ylab="Discharge, cfs") # }

\donttest{
  # The dataRetrieval package is not required by MGBT algorithms.
  # An example with zero flows
  PK <- dataRetrieval::readNWISpeak("07148400", convertType=FALSE)
  PK <- splitPeakCodes(PK)
  plotPeaks(PK, codes=TRUE,  showDubNA=TRUE,
                xlab="Water year", ylab="Discharge, cfs") # }

\donttest{
  # The dataRetrieval package is not required by MGBT algorithms.
  PK <- dataRetrieval::readNWISpeak("08329935", convertType=FALSE)
  PK <- splitPeakCodes(PK)
  plotPeaks(PK, codes=TRUE, showDubNA=TRUE,
                xlab="Water year", ylab="Discharge, cfs") # }
}
\keyword{graphics}

\encoding{utf8}
\name{plotPeaks}
\alias{plotPeaks}
\title{Plot Peak Streamflows with Emphasis on Peak Discharge Qualification Codes}
\description{
Plot U.S. Geological Survey peak streamflows in \code{peak_va} and discharge qualifications codes in the \code{peak_cd} columns of a peak-streamflow data retrieval from the National Water Information System (NWIS) (U.S. Geological Survey, 2019). This code makes use of the \code{add.log.axis} function from the \pkg{lmomco} package, and because this is the only instance of this dependency, the \pkg{lmomco} package is treated as a suggested package and not as a dependency for the \pkg{MGBT} package. The \code{add.log.axis} function also is used for the basis of the logarithmic plot within the \code{\link{plotFFQevol}} function.

This function accommodates the plotting of various nuances of the peak including less than (code 4), greater than (code 8), zero peaks (plotted by a green tick on the horizontal axis), and peaks that are missing but the gage height was available (plotted by a light-blue tick on the horizontal axis if the \code{showGHyrs} argument is set). So-called code 5, 6, and 7 peaks are plotted by the numeric code as the plotting symbol, and so-called code C peaks are plotted by the letter \dQuote{C.} The very unusual circumstances of codes 3 and O are plotted by the letters \dQuote{D} (dam failure) and \dQuote{O} (opportunistic), respectively. These codes are are summarized within \code{\link{splitPeakCodes}}. The greater symbology set is described in the directory \code{MGBT/inst/legend} of the package sources.

The logic herein also makes allowances for \dQuote{plotting} gage-height only streamgages but this requires that the \code{\link{splitPeakCodes}} function was called with the \code{all_peak_na_okay} set to \code{TRUE}. The gap analysis for gage-height only streamgages or streamgages for which the site changes from gage-height only to discharge and then back again or other permutations will likely result in the gap lines not being authoritative. The sub-bottom axis ticks for the gage-height only water years should always be plotting correctly.
}
\usage{
plotPeaks(x, codes=TRUE, lot=NULL, site="",
             xlab="", ylab="", xlim=NULL, ylim=NULL,
             xlim.inflate=TRUE, ylim.inflate=TRUE, aux.y=NULL,
             log.ticks=c(1, 2, 3, 5, 8), mtext.site=TRUE,
             show48=FALSE, showDubNA=FALSE, showGHyrs=TRUE,
             make_text_for_all_na_peaks=TRUE, ...)
}
\arguments{
  \item{x}{A \code{data.frame} having a mandatory column titled \code{peak_va} and \code{peak_cd}. It is advised to have run \code{\link{makeWaterYear}} and \code{\link{splitPeakCodes}} first, but if columns resulting from those two functions are are not detected, then the water year column and code split are made internally but not returned;}
  \item{codes}{A logical to trigger use of character plotting characters and not symbols;}
  \item{lot}{The low-outlier threshold, but if omitted, then \code{\link{MGBT}} is triggered internally. Use of \code{lot=0} is a mechanism to effectively bypass the plotting of a low-outlier threshold because a logarithmic vertical axis is used, and this setting would bypass a call to \code{\link{MGBT}};}
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
  \item{mtext.site}{A logical, if set, will call \code{mtext(site, line=0.5))} as the function completes. This is handy for emplacing the contents of \code{site} at the top of the plot but by having a logical switch for this, the \code{site} can be used for internal purposes, such as warnings, specific to this site. Setting \code{mtext.site=FALSE} might be preferable in data mining exercises with very many streamgages to help isolate those streamgages with data or lack thereof that cause \pkg{MGBT} problems;}
  \item{showDubNA}{A logical, if set, will draw a \eqn{\pm}half-year polygon for the water years having both \code{NA} discharge (\code{peak_va}) and \code{NA} gage height (\code{gage_ht}). To some analysts, it might be useful to study the degree of these double empty entries in contrast to just the records of such years not even being present in NWIS. Double empty entries could represent incomplete data handling on part of the USGS or inadvertently missing zero discharges for \code{peak_va};}
  \item{showGHyrs}{A logical to trigger the light-blue special ticks for the water years having only gage height. The horizontal limits will be inflated accordingly if gage heights are outside the general discharge record;}
  \item{make_text_for_all_na_peaks}{A logical to trigger a message to be centered in the middle of the plot alert the reader that there are no peaks to for the streamgage (\dQuote{There are no non-missing peaks for this streamgage.}); and }
  \item{...}{Additional arguments to pass to \code{plot}.}
}
\value{
  No values are returned; this function is used for its graphical side effects.
}
\note{
The file \code{inst/legend/legend_camera.pdf} has been configured also to be embedded into this user manual as shown below:

\if{html}{\figure{plotPeakslegend.jpg}}
\if{latex}{\figure{plotPeakslegend.pdf}{options: width=5.5in, trim=1in 0.2in 0.5in 0.6in, clip}}
}
\author{W.H. Asquith}
\references{
U.S. Geological Survey, 2019, USGS water data for the Nation: U.S. Geological Survey National Water Information System database, accessed October 11, 2019, at \doi{10.5066/F7P55KJN}.
}
\seealso{\code{\link{makeWaterYear}}, \code{\link{splitPeakCodes}}, \code{\link{plotFFQevol}}
}
\examples{
\dontrun{
  # The dataRetrieval package is not required by MGBT algorithms.
  # Note that makeWaterYear() is not needed because splitPeakCodes() requires
  # the water_yr for gap analyses, and will call makeWaterYear() if it needs to.
  PK <- dataRetrieval::readNWISpeak("08167000", convertType=FALSE)
  PK <- splitPeakCodes(makeWaterYear(PK))
  plotPeaks(PK, codes=TRUE, showGHyrs=FALSE, site="08167000",
                xlab="Water year", ylab="Discharge, cfs") # }

\dontrun{
  # The dataRetrieval package is not required by MGBT algorithms.
  # An example with zero flows
  PK <- dataRetrieval::readNWISpeak("07148400", convertType=FALSE)
  PK <- splitPeakCodes(PK)
  plotPeaks(PK, codes=TRUE,  showDubNA=TRUE, site="07148400",
                xlab="Water year", ylab="Discharge, cfs") # }

\dontrun{
  # The dataRetrieval package is not required by MGBT algorithms.
  PK <- dataRetrieval::readNWISpeak("08329935", convertType=FALSE)
  PK <- splitPeakCodes(PK)
  plotPeaks(PK, codes=TRUE, showDubNA=TRUE, site="08329935",
                xlab="Water year", ylab="Discharge, cfs") # }

\dontrun{
  PK <- dataRetrieval::readNWISpeak("02146285", convertType=FALSE)
  PK <- splitPeakCodes(PK, all_peaks_na_okay=TRUE)
  plotPeaks(PK, site="02146285") # see likely message about all
  # peaks being missing assume that this streamgage forevermore
  # is a gage-height only site }

\dontrun{
  PK <- dataRetrieval::readNWISpeak("02317600", convertType=FALSE)
  PK <- splitPeakCodes(PK, all_peaks_na_okay=TRUE)
  plotPeaks(PK, site="02317600") }
}
\keyword{graphics}

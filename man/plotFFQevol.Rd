\encoding{utf8}
\name{plotFFQevol}
\alias{plotFFQevol}
\title{ Plot Flood-Frequency in Time }
\description{
Plot the temporal evolution of peak-streamflow frequency using the method of L-moments on the systematic record, assuming that \code{appearsSystematic} fully identifies the systematic record (see \code{\link{splitPeakCodes}}), but have the results align on the far right side to other results. These other results are intended to be an official best estimate of the peak-streamflow frequency using all available information and are generally anticipated to be from Bulletin 17C (B17C) (England and others, 2018). The motivation for this function is that some have contacted one of the authors with a desire to show a temporal evolution of the estimates of the 2-, 10-, 100-, and 500-year peak-streamflow values but simultaneously have \dQuote{current} (presumably a modern year [not necessarily the last year of record]) results. A target year (called the \code{final_water_yr}) is declared. Base10 logarithmic offsets from the so-called final quantile values at that year are computed. These then are used additively to correct the temporal evolution of the L-moment based peak-streamflow frequency values.

The code herein makes use of the \code{f2flo}, \code{lmoms}, \code{parpe3}, \code{quape3}, \code{T2prob}, \code{x2xlo} functions from the \pkg{lmomco} package, and because this is the only instance of this dependency, the \pkg{lmomco} package is treated as a suggested package and not as a dependency for the \pkg{MGBT} package. The \bold{Examples} use the \pkg{dataRetrieval} package for downloading peak streamflow data.
}
\usage{
plotFFQevol(pkenv, lot=NULL, finalquas=NULL, log10offsets=NULL,
          minyrs=10, byr=1940, edgeyrs=c(NA, NA), logyaxs=TRUE,
          lego=NULL, maxs=NULL, mins=NULL, names=NULL, auxeyr=NA,
          xlab="Water Year", ylab="Peak streamflow, in cubic feet per second",
          title="Time Evolution of Peak-Frequency Streamflow Estimates\n",
          data_ext="_data.txt", ffq_ext="_ffq.txt", path=tempdir(),
          showfinalyr=TRUE, usewyall=FALSE, silent=TRUE, ...)
}
\arguments{
  \item{pkenv}{A \code{environment} having a mandatory column titled \code{peak_va} and \code{peak_cd}. It is required to have run \code{\link{makeWaterYear}} and \code{\link{splitPeakCodes}} first;}
  \item{lot}{A vector of low-outlier thresholds for the stations stored in \code{pkenv};}
  \item{finalquas}{A \code{data.frame} (see \bold{Examples}) of the final peak-streamflow frequency values to be soon (matched!) at the right side of the plot. These values could stem from B17C-like analyses using all the features therein. These values force, through an offsetting method in log10-space, the method of L-moment results in time to \dQuote{land} to the results at the end (right side).}
  \item{log10offsets}{An optional offset \code{data.frame} (see \bold{Examples}) to add to \code{finalquas} so that the final right side method of L-moment results match the results given in \code{finalquas};}
  \item{minyrs}{The minimum number of years (sample size) before trying to fit a log-Pearson type III distribution by method of L-moments---the \code{lot} will be honored through a conditional distribution trunction;}
  \item{byr}{The beginning year for which to even start consultion for peaks in time frequency analysis;}
  \item{edgeyrs}{The optional bounding years for the horizontal axis but potentially enlarged by the data as well as the setting of \code{usewyall};}
  \item{logyaxs}{A logical to trigger a logarithmic plot. The logarithmic plot is based on the function \code{\link{plotPeaks}} and is more sophisticated. The linear option is provided herein as some users have deemed logarthimic axes too complicated for engineers to understand (seriously);}
  \item{lego}{The year at which to start the legend;}
  \item{maxs}{The upper limit of the vertical axis;}
  \item{mins}{The lower limit of the vertical axis;}
  \item{names}{An optional character string vector to be used as a plot title;}
  \item{auxeyr}{An optional auxillary ending year to superceed the \code{final_water_yr} from the \code{finalquas}. The \code{auxeyr} could lead to interesting conflict in the graphic. For example, a 500-year value being less than the 100-year. If a distribution swings between sign of the skew parameter and because the offsets are computed at a the discrete point in time (\code{final_water_yr}), then the offset could be too large at the 500-yr level and cause overlap (see \bold{Examples});}
  \item{xlab}{An optional x-label of the plot;}
  \item{ylab}{An optional y-label of the plot;}
  \item{title}{An optional super title for the plot to be shown above \code{names};}
  \item{data_ext}{An optional file name extension to the data (only water year, peak streamflow value, and \code{appearsSystematic}) retained in this file relative to the greater results inside the \code{pkenv}. If an output file is not desired, set to \code{NA};}
  \item{ffq_ext}{An optional file name extension to the flood-flow frequency (water year and 2, 10, 100, and 500 year) output. If an output file is not desired, set to \code{NA};}
  \item{path}{The path argument for the output files with a default to a temporary directory for general protection of the user;}
  \item{showfinalyr}{A logical to control whether a line is drawn showing the location of the final water year (the year of the quantile estimates, \code{final_water_yr}) that  spans top to bottom on the graphic;}
  \item{usewyall}{A logical to consult the \code{isCode7} for determination of the horizontal axis limits;}
  \item{silent}{A logical for some status update messaging; and}
  \item{...}{Additional arguments to pass to \code{plot}.}
}
\value{
  The log10-offset values are returned if not given otherwise this function is used for its graphical side effects.
}
\author{ W.H. Asquith}
\source{
Earlier code developed by W.H. Asquith in about 2016.
}
\references{
England, J.F., Cohn, T.A., Faber, B.A., Stedinger, J.R., Thomas Jr., W.O., Veilleux, A.G., Kiang, J.E., and Mason, R.R., 2018, Guidelines for determining flood flow frequency Bulletin 17C: U.S. Geological Survey Techniques and Methods, book 4, chap. 5.B, 148 p., \url{https://pubs.er.usgs.gov/publication/tm4B5}
}
\seealso{\code{\link{plotPeaks}}
}
\examples{
\donttest{
# The dataRetrieval package is not required by MGBT algorithms.
opts <- options(scipen=7)
opar <- par(no.readonly=TRUE)
par(mgp=c(3,0.5,0), las=1) # going to tick inside, change some parameters

names <- c("08167000 Guadalupe River at Comfort, Tex.")
stations <- c("08167000"); LOT <- c(3110)
maxs <- c(525000); lego <- c(1940)

PKS <- new.env()
for(station in "08167000") {
  message("Pulling peaks for ",station)
  data <- dataRetrieval::readNWISpeak(station, convertType=FALSE)
  data <- splitPeakCodes(MGBT::makeWaterYear(data))
  assign(station, data, PKS)
}

# This example should run fine though the resulting curves will end in 2015 because
# this is the declared ending year of 2015. Data points after 2015 will be shown
# but the FFQ values will be the values plotted. Yes, other return periods are shown
# here and dealt with internally, but only the 2, 10, 100, and 500 are drawn.
FFQ <- data.frame(site_no="08167000", final_water_yr=2015,
                  Q002= 3692, Q005= 21000, Q010= 40610, Q025= 69740,
                  Q050=91480, Q100=111600, Q200=129400, Q500=149200)
# Now compute the offsets associated with those given above.
OFFSET <- plotFFQevol(PKS, lot=LOT, finalquas=FFQ)
# Notice the plotFFQevol() is called twice. One call is to compute the
# offsets, and the next is to use them and make a pretty plot.
plotFFQevol(PKS, lot=LOT, finalquas=FFQ, log10offsets=OFFSET,
            maxs=maxs, mins=rep(0,length(maxs)), names=names,
            lego=lego, logyaxs=FALSE, edgeyrs=c(1940,2020), usewyall=TRUE)

# Now a change up, lets say these values are good through the year 1980, and yes,
# these are the same values shown above.
FFQ$final_water_yr <- 1980
OFFSET <- plotFFQevol(PKS, lot=LOT, finalquas=FFQ) # compute offsets
# Now using auxeyr=2020, will trigger the evolution through time and the results in
# 1980 will match these given in the FFQ. One will see (presumably for many years
# after 2017) the crossing of the 500 year to the 100 year in about 1993.
plotFFQevol(PKS, lot=LOT, finalquas=FFQ, log10offsets=OFFSET,
            maxs=maxs, mins=rep(0,length(maxs)), names=names, auxeyr=2020,
            lego=lego, logyaxs=FALSE, edgeyrs=c(1940,2020), usewyall=FALSE)

# Now back to the original FFQ but in log10 space and plotPeaks().
FFQ$final_water_yr <- 2017
OFFSET <- plotFFQevol(PKS, lot=LOT, finalquas=FFQ) # compute offsets
# Now using logyaxs=TRUE and some other argument changes
plotFFQevol(PKS, lot=LOT, finalquas=FFQ, log10offsets=OFFSET, title="",
            maxs=maxs, mins=rep(0,length(maxs)), names=names, auxeyr=2020,
            lego=NULL, logyaxs=TRUE, edgeyrs=c(1890,2020), usewyall=TRUE,
            showfinalyr=FALSE)
options(opts) # restore the defaults
par(opar)     # restore the defaults}
}
\keyword{graphics}

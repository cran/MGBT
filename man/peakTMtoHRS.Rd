\encoding{utf8}
\name{peakTMtoHRS}
\alias{peakTMtoHRS}
\title{Peak Time to Decimal Hours or Compute Mean and Standard Deviations}
\description{
Parse the \code{peak_tm} (peak time) field from the USGS National Water Information System into decimal hours or compute the circular mean and standard deviations of the hours.
}
\usage{
peakTMtoHRS(x, type=c("asis", "musd"), parseHHMM=TRUE)
}
\arguments{
  \item{x}{A colon delimited hour:minutes format of \code{peak_tm} of retrieved peak streamflows as would be reported through \code{\link{splitPeakCodes}};}
  \item{type}{Return a vector of the decimal hours or compute the mean and standard deviations by circular mathematics; and}
  \item{parseHHMM}{A logical to parse the \code{HH:MM} notation from decimal hours in \code{x}, and setting to false will treat time has being in decimal hours.}
}
\value{
  A vector of decimal hours (24-hour clock) or the circular mean and standard deviation.
}
\source{
Consultation of \pkg{circular} package sources.
}
\references{
Mardia, K.V., 1972, Statistics of directional data: London, Academic Press, London, sec. 26.5, p. 617.

Lund, U., Agostinelli, C., Arai, H., Gagliardi, A., \enc{García-Portugués}{Garcia-Portugues}, E., Giunchi, D., Irisson, JO., Pocernich, M., and Rotolo, F., 2024, circular---Circular statistics: R package version 0.5-1, dated August 29, 2024, accessed March 17, 2025, at \doi{10.32614/CRAN.package.circular}.
}
\author{ W.H. Asquith }
\seealso{\code{\link{splitPeakCodes}}
}
\examples{
\dontrun{
  pk <- dataRetrieval::readNWISpeak("08167000", convertType=FALSE)
  pk <- splitPeakCodes(pk, all_peaks_na_okay=TRUE)
  pk <- pk[! is.na(pk$peak_va) & pk$peak_va >= median(pk$peak_va, na.rm=TRUE),]
  pk$hours <- peakTMtoHRS(pk)
  print(peakTMtoHRS(pk, type="musd"))

  circ <- circular::circular(pk$hours, type="angle", units="hours",
                   template="clock24", rotation="clock")
  mu <- as.vector(circular::mean.circular(circ, na.rm=TRUE))
  mu <- ifelse(mu < 0, 24 + mu, mu)
  print(c(mu, circular::sd.circular(circ, na.rm=TRUE)))
  circular::plot.circular(circ) #}

\dontrun{
  LAarea <- c("10263500", "10263900", "10264530", "11085000", "11092450", "11097000",
              "11098000", "11101250", "11102300", "11108134", "11109525", "11109550")
  PK <- new.env(); frm0000 <- NULL; n <- 0
  for(site in LAarea) {
    pk <- dataRetrieval::readNWISpeak(site, convert=FALSE)
    pk <- MGBT::splitPeakCodes(pk); pk <- pk[! is.na(pk$peak_tm),]
    pk <- pk[pk$peak_tm != "00:00", ]; pk <- pk[pk$peak_tm != "12:00", ]
    if(nrow(pk) == 0) { message("skipping", site); next }
    pk$hrs <- peakTMtoHRS(pk)
    musd <- peakTMtoHRS(pk, type="musd"); n <- n + musd[3]
    frm0000 <- c(frm0000, musd[1])
  }
  names(  frm0000) <- NULL
  frm0000[frm0000 > 12] <- 24 - frm0000[frm0000 > 12]
  musd <- peakTMtoHRS(frm0000, type="musd", parseHHMM=FALSE)
  message(paste0("For ", length(LAarea), " streamgages in Los Angeles county\n",
          "and ", n, " occurrences of peak streamflow time has a\n",
          "mean site-mean of ", round(musd[1], digits=2), " hours from midnight."))
}
}
\keyword{peak time}

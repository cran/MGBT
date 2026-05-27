\encoding{utf8}
\name{ratioPeakMax1Day}
\alias{ratioPeakMax1Day}
\title{Relations Between Peak Streamflows to 1-Day Annual Maxima}
\description{
Both daily mean streamflow and instantaneous peak streamflows for many U.S. Geological Survey streamgages are available within the National Water Information System (NWIS). Peak streamflows are aggregated by water year (October 1 to September 30) NWIS and such streamflows are especially useful for flood frequency computations. Daily mean streamflows can be processed on a water year basis and 1-day annual maxima computed, and these maxima represent a 1-day volume as it were and are useful for flood volume frequency computations.

By strict definition, peak streamflows must equal or exceed 1-day annual maxima. Further 1-day annual maxima themselves should also underestimate a true volume maxima because a 1-day time step (midnight to midnight) is not a moving 24-hour or finer time-step resolution through streamflow hydrographs. Therefore, there are two mechanisms for 1-day maxima to be lesser than the peaks.

If a watershed is very large, flood hydrographs can be expected to be temporally wide and slow changing, such situations might have peak and 1-day annual maxima effectively equal to each other. Alternatively, if watershed is very small, flood hydrographs can be expected to be temporally of short duration and the peaks much larger than the 1-day annual maxima.

In terms of frequency computations, the peaks represent one dataset and the 1-day annual maxima another. There is not a restriction that the 1-day annual maxima be contemporaneously timed with the peak; although the hydrograph producing the peak is often aligned with maximum volume. The computations described herein consider the peak and the 1-day annual maxima to be simply distinct events and no processing of contemporaneousness is made aside from the simple water year designation.

This function supports optional caching of daily streamflow values and the peak streamflow values by environment keyed on the streamflow identification number. If a given cache is not provided, then the retrieval of the daily and (or) peak data are made using the \pkg{dataRetrieval} package. For the daily value cache, simply a data frame is needed of the \code{Date} and \code{Flow} columns from a previous or similar operation of the \code{dataRetrieval::readNWISdv()} function. For the peak value cache, simply a data frame from a previous operation of \code{dataRetrieval::readNWISpeak()} function is needed. The peak table internally is processed through \code{\link{splitPeakCodes}} to isolate peak streamflows by water year that appear to be part of systematic data collection activities. So called \dQuote{opportunistic} peaks are ignored, but so-called \dQuote{historical} peaks might be retained if the record appears as systematic.
}
\usage{
ratioPeakMax1Day(siteNumber, dvenv=NULL, pkenv=NULL, as.list=FALSE,
                 missing.days=0, rm.ratios.lt1=TRUE, silent=TRUE, ...)
}
\arguments{
  \item{siteNumber}{USGS streamgage identification number and nomenclature matches that of the \pkg{dataRetrieval} package. This function does not support a vector of site numbers and only the first will be silently used.}
  \item{dvenv}{An optional \R \code{environment} previously populated with daily mean streamflows. The environment needs the USGS streamgage identification number as the key and the value needs to be a data frame having just the two columns \code{Date} and \code{Flow};}
  \item{pkenv}{An optional \R \code{environment} previously populated peak streamflows. The environment needs the USGS streamgage identification number as the key;}
  \item{as.list}{Return an extensive list of the various tables produced internally, a ratio table for ratios less than 1, and the ratio table. The later table could include ratios less than 1 if \code{rm.ratios.lt1} is false.}
  \item{missing.days}{The number of permissible missing days in a given year on which to still report it as a complete water year, which is the \dQuote{annual} basis of the aggregation for the 1-day annual maxima.}
  \item{rm.ratios.lt1}{Remove records for which the ratio of peak to 1-day maxima is less than 1. Situations such as this might exist because of (1) minor differences in the numerical values, (2) outright erroneous information in NWIS and a user might want to contact the local USGS office operating the streamgage and possibly the database could be legitimately fixed, or (3) other reasons. The default as true is likely the more suitable for data-mining endeavors rather than trying to \dQuote{find} erroneous situations.}
  \item{silent}{Suppress informative calls to \code{message()}.}
  \item{...}{Additional arguments to pass to \code{dataRetrieval::}\code{readNWISdv()}.}
}
\value{
  An \R \code{data.frame} (the \dQuote{ratio table}) is returned for a false \code{as.list}:
  \item{site_no}{The streamgage identification number;}
  \item{water_yr}{The water year and support only for water year (October 1 through September 30) is made because USGS peak streamflows are uniquely water-year based;}
  \item{dvflow_count}{The number of days available in the water year with regard to a condition that a complete water year has greater than or equal to 365 minus \code{missing.days}. This logic does support leap year and therefore the minimum \code{dvflow_count} for a given year would be either be 363 or 364 if \code{missing.days=2}.}
  \item{peak_flow}{The reported peak streamflow water year identified as stemming from systematic data collection activities in accordance to algorithm within the \code{\link{splitPeakCodes}} for a year for which annual 1-day maxima exist;}
  \item{mx1d_flow}{The 1-day annual maxima streamflow (midnight to midnight) for the water year. Note, that the 1-day annual maxima are themselves underestimated because a moving window through unit values (the instantaneous streamflows, such as hourly or 15 minute) are not considered in the computations and therefore no \dQuote{fixed-interval bias correction} is involved in the operation of this function.}
  \item{ratio}{Ratio of the \code{peak_flow} to the \code{mx1d_flow}. Because a fixed-interval bias correction on the 1-day maxima was not involved in the computations (see \code{mx1d_flow}), this ratio is thought of as biased high. The values are rounded to eight digits, which is more than enough in practical circumstances but also at data-mining scales will produce somewhat smaller file sizes.}
  \item{log10diff}{The base-10 logarithmic difference between the \code{peak_flow} and the \code{mx1d_flow} defined as the logarithmic peak minus the logarithmic maxima. Because a fixed-interval bias correction on the 1-day maxima was not involved in the computations (see \code{mx1d_flow}), this ratio is thought of as biased high. The values are rounded to eight digits, which is more than enough in practical circumstances but also at data-mining scales will produce somewhat smaller file sizes.}
}
\seealso{ \code{\link{splitPeakCodes}} }
\examples{
\dontrun{
  site <- "08062700" # Example of 1980 peak being lower than 1-day maxima.
  D <- ratioPeakMax1Day(site, missing.days=0, rm.ratios.lt1=FALSE)
  print(D[D$ratio < 1,]) # 1980 on 18 Sep 2024 retrieval tests #}

\dontrun{
  # site <- "08019200" # Example of peaks and maxima nearly identical.
    site <- "08167000" # Example of peaks and maxima being quite different.
  D <- ratioPeakMax1Day(site, missing.days=7) # permit a week of missing
  if(class(D) == "list") D <- D$ratios # if user added as.list=TRUE to call
  por <- paste0(min(D$water_yr), "-", max(D$water_yr),
                 " (", nrow(D), " processed water years)")
  plot(D$mx1d_flow, D$peak_flow, pch=21, col="black", bg="white", log="xy",
       xlab="Water year 1-day annual maxima streamflow, in cfs",
       ylab="Water year annual peak streamflow, in cfs")
  abline(0, 1); mtext(site, line=0.25)
  legend("topleft",    "Equal value line", bty="n", lty=1, pch=NA)
  legend("bottomright", por,               bty="n", lty=0, pch=NA)

  # Continuing, consider the two data sets via their order statistics, with us also
  # breaking the joint water year coupling in some study
  peakQ <- sort( log10(D$peak_flow) ) # order statistics of the peaks
  mx1dQ <- sort( log10(D$mx1d_flow) ) # order statistics of the maxima
  # let us for demonstration purposes only, use a log-normal distribution
  # and hence the use of log10() in the previous two calls
  peak_mu <- mean( peakQ ); peak_sd <- sd( peakQ ) # mean + std deviation
  mx1d_mu <- mean( mx1dQ ); mx1d_sd <- sd( mx1dQ ) # mean + std deviation

  FF <- seq(0.005, 0.995, by=0.005) # nonexceedances for drawing curves
  qFF <- qnorm(FF) # standard normal variates to transform x-axis
  peakPP  <- rank(peakQ) / (length(peakQ) + 1) # Weibull plotting position
  mx1dPP  <- rank(mx1dQ) / (length(mx1dQ) + 1) # Weibull plotting position
  peakFFQ <- 10^qnorm(FF, mean=peak_mu, sd=peak_sd) # Log-normal curves
  mx1dFFQ <- 10^qnorm(FF, mean=mx1d_mu, sd=mx1d_sd) # Log-normal curves

  ylim <- range(c(1, peakQ / mx1dQ, log10(D$peak_flow) / log10(D$mx1d_flow)))
  x <- log10(D$mx1d_flow);     y <- log10(D$peak_flow) / log10(D$mx1d_flow)
  plot(mx1dQ, peakQ / mx1dQ, type="l", col="darkgreen", pch=21, bg="lightgreen",
       xlab="log10(water year 1-day annual maxima streamflow)", ylim=ylim, las=1,
       ylab="Ratio of annual peak to 1-day annual maxima")
  lines(x[order(x)], y[order(x)], pch=23, col="salmon4",   bg="salmon1"   )
  points(mx1dQ, peakQ / mx1dQ,    pch=21, col="darkgreen", bg="lightgreen", cex=1.3)
  points(log10(D$mx1d_flow), log10(D$peak_flow) / log10(D$mx1d_flow),
         pch=23, col="salmon4", bg="salmon1", cex=0.8)
  txt <- c("Joint ratio by separate sorting of peak and annual maxima",
           "Ratio by maintaining water year as the connection")
  legend("bottomright", txt, cex=0.8, bty="n", pch=c(21, 23), lty=c(1, 1), bg="white",
         col=c("darkgreen", "salmon4"), pt.cex=1, pt.bg=c("lightgreen", "salmon1"))
  mtext(site, line=0.25)

  plot(qnorm(FF), peakFFQ, type="n", log="y",
       ylim=range(c(10^peakQ, 10^mx1dQ, peakFFQ, mx1dFFQ)),
       xlab="Standard normal variate", ylab="Flood magnitude, in cfs")
  points(qnorm(peakPP), 10^peakQ, pch=21, col="red",  bg="white")
  points(qnorm(peakPP), 10^mx1dQ, pch=22, col="blue", bg="white")
  lines(qFF, peakFFQ, col="red",  lty=2); lines(qFF, mx1dFFQ, col="blue", lty=2)
  txt <- c("Peak streamflow frequency to data shown",
           "1-day annual maxima frequency to data shown",
           "Observed annual peak streamflow",
           "Observed 1-day annual maxima streamflow")
  legend("topleft", txt, bty="n", lty=c(2, 2, NA, NA), bg="white",
          pch=c(NA, NA, 21, 22), col=c("red", "blue", "red", "blue"))
  mtext(site, line=0.25) # }

\dontrun{
  # Continuation of the previous dontrun{} block, we now try to "use" the ratio by
  # imagining what flood frequency would look like without having the peaks themselves.
  # The context is to imagine streamflow data stemming from nonUSGS streamflow sources
  # and there are no peaks available. Perhaps one could study analog watersheds of the
  # USGS and create a statistical model to predict a correction of the 1-day to the
  # peaks based on watershed properties and derived pseudo-peaks. First two "obvious"
  # ways to look at the peak to maxima ratios by means of the ratios:
  phi_simpl <-    mean(D$ratio)                                      # arithmetic
  phi_geomu <- cumprod(D$ratio)[length(D$ratio)]^(1/length(D$ratio)) # geometric

  # Then, let us think about the coupling between the two datasets by their order
  # statistics. This is potentially more informative because structurally the frequency
  # curves themselves derived in some methods from the order statistics (L-moments or
  # product spacings).
  phi_infor <- mean(peakQ / mx1dQ) # mean ratio of the log10 flows
  phi_log10 <- mean(D$log10diff)   # mean log10 offset (phi_geomu == 10^phi_log10)

  # Continuing with simple log-normal distribution model, now correct maxima
  mx1d_mu_simpl <- mean( log10( phi_simpl * 10^mx1dQ ) )
  mx1d_sd_simpl <-   sd( log10( phi_simpl * 10^mx1dQ ) )
  mx1dFFQ_simpl <- 10^qnorm(FF, mean=mx1d_mu_simpl, sd=mx1d_sd_simpl)
  # Correct maxima by goemetric mean
  mx1d_mu_geomu <- mean( log10( phi_geomu * 10^mx1dQ ) )
  mx1d_sd_geomu <-   sd( log10( phi_geomu * 10^mx1dQ ) )
  mx1dFFQ_geomu <- 10^qnorm(FF, mean=mx1d_mu_geomu, sd=mx1d_sd_geomu)
  mx1dFFQ_log10 <- 10^(  log10(mx1dFFQ) + phi_log10  ) # This is the same as if
  # the geometric mean were used as shown for mx1dFFQ_geomu. It seems more concise
  # to think of a simple log10 offset than geometric mean.

  # Now, consider relative variation as a constant and therefore the variation
  # (standard deviation) likely needs some scaling as well. We use the CV of the
  # original sample and after the mean is rescaled, the standard deviation is too.
  cv <- sd( mx1dQ ) / mean( mx1dQ ) # coefficient of variation (CV)
  mx1d_mu_infor <- mean( mx1dQ ) *     phi_infor
  mx1d_sd_infor <-            cv * mx1d_mu_infor
  mx1dFFQ_infor <- 10^qnorm(FF, mean=mx1d_mu_infor, sd=mx1d_sd_infor)

  cols <- c("red", "blue", "turquoise3", "purple", "palegreen4", "red", "blue")
  plot(qnorm(FF), peakFFQ, type="n", log="y",
       ylim=range(c(10^peakQ, 10^mx1dQ, peakFFQ, mx1dFFQ)),
       xlab="Standard normal variate", ylab="Flood magnitude, in cfs")
  points(qnorm(peakPP), 10^peakQ, pch=21, col=cols[1], bg="white", lty=2)
  points(qnorm(peakPP), 10^mx1dQ, pch=21, col=cols[2], bg="white", lty=2)
  lines(qFF, peakFFQ, col=cols[1], lty=2); lines(qFF, mx1dFFQ, col=cols[2], lty=2)
  lines(qFF, mx1dFFQ_simpl, col=cols[3], lwd=2)
  lines(qFF, mx1dFFQ_log10, col=cols[4], lwd=2)
  lines(qFF, mx1dFFQ_infor, col=cols[5], lwd=3)
  legend("topleft", c("Peak streamflow frequency to data shown",
   "1-day annual maxima frequency to data shown", "Pseudo-peak frequency by mean-ratio",
   "Pseudo-peak frequency by log10-offset", "Pseudo-peak frequency by mean-cv-logratio",
   "Observed annual peak streamflow", "Observed 1-day annual maxima streamflow"),
         bty="n", pch=c(NA, NA, NA, NA, NA, 21, 21), pt.cex=1, bg="white",
         lwd=c(1, 1, 2, 2, 3, NA, NA), lty=c(2, 2, 1, 1, 1, NA, NA), col=cols)
  mtext(site, line=0.25) # }
}
\keyword{data retrieval}
\keyword{annual daily maxima}
\keyword{peak streamflow}
\keyword{ratio peak to 1-day maxima}

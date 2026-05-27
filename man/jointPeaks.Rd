\encoding{utf8}
\name{jointPeaks}
\alias{jointPeaks}
\title{Construct Joint Peak Object and Empirical Probabilities}
\description{
Construct a joint peak object by retrieval of peak streamflows for two sites and joining the union of the peaks by water year and create empirical probabilities of the bivariate relation. This function is especially helpful when one is studying joint probabilities in peaks.
}
\usage{
jointPeaks(Asite_no="--", Bsite_no="--", appearsSystematic=FALSE, a=0,
           Adf=NULL, Bdf=NULL, ...)
}
\arguments{
  \item{Asite_no}{The \dQuote{A site} (think horizontal axis) \code{siteNumber} (\pkg{dataRetrieval} parlance) and usually required for this function as a character identifier of a streamgage and is usually an 8 digit number (USGS data tables use \code{site_no} as the column title). The default is deliberate to provide flexibility if \code{Adf} is provided;}
  \item{Bsite_no}{The \dQuote{B site} (think horizontal axis) \code{siteNumber} (\pkg{dataRetrieval} parlance) and usually required for this function  as a character identifier of a streamgage and is usually an 8 digit number (USGS data tables use \code{site_no} as the column title). The default is deliberate to provide flexibility if \code{Bdf} is provided;}
  \item{appearsSystematic}{Whereas one might want only to do the univariate marginal distributional analysis and not rely on non-systematic record without more sophisticated analysis methods of censored information, a decision is made to have the default for this argument false so as to maximize information into the deep tail of the bivariate relation between the peaks at the two sites. Remember that if copulas were to be used in analyses of the joint distribution that such operations are in probability space and not the real space of the analysis. We think better deep-tail information is available if the default is false. See also the \code{\link{splitPeakCodes}} function for more information on \code{appearsSystematic};}
  \item{a}{A value for the plotting-position formula from package \pkg{lmomco} \code{pp} function, default is \eqn{a = 0}, which returns the Weibull plotting positions;}
  \item{Adf}{An optional data frame of \dQuote{peaks} to supercede the retrieval of peaks. This feature is useful if a user want to inject another quantity, such as flood volume, into this function. The \code{Adf} is required to have a \code{water_yr}, \code{peak_dt}, \code{peak_va}, \code{appearsSystematic}, and \code{nwiscda_sqmi} or columns not actually needed such as the user could also provide \code{dev_lat_va}, \code{dev_long_va};}
  \item{Bdf}{An optional data frame of \dQuote{peaks} to supercede the retrieval of peaks. This feature is useful if a user want to inject another quantity, such as flood volume, into this function. The \code{Bdf} is required to have a \code{water_yr}, \code{peak_dt}, \code{peak_va}, \code{appearsSystematic}, and \code{nwiscda_sqmi} or columns not actually needed such as the user could also provide \code{dev_lat_va}, \code{dev_long_va}; and}
  \item{...}{Other arguments to pass (if ever needed).}
}
\value{
  A list is returned with elements:
  \item{Asite_no}{Data frame of the entire period of record for the \code{Asite_no} streamgage after the \code{\link{splitPeakCodes}} call with the addition of decimal latitude and longitude values and the contributing drainage area from the attribution information from the \code{dataRetrieval::readNWISpeak()};}
  \item{Bsite_no}{Data frame of the entire period of record for the \code{Bsite_no} streamgage after the \code{\link{splitPeakCodes}} call with the addition of decimal latitude and longitude values and the contributing drainage area from the attribution information from the \code{dataRetrieval::readNWISpeak()};}
  \item{AB}{Data frame as the joint peak object containing the pairwise matches in peaks and influenced or not by \code{appearsSystematic} argument. The data frame contains the \code{water_yr} (USGS definition of October 1 through September and numbered by the year for which it ends), both site numbers (\code{Asite_no}, \code{Bsite_no}), the contributing drainage areas from USGS database (\code{Anwiscda_sqmi}, \code{Bnwiscda_sqmi}), the date of the peaks (\code{Apeak_dt}, \code{Bpeak_dt}) (as character to thus support incomplete dates but still retained to maximize pair-wise peak counts), the peak values (\code{Apeak_va}, \code{Bpeak_va}), absolute number of days in the water year between the peaks (\code{absdays_between_pks}), nonexceedance probabilities by plotting position between the peaks (these are useful for bivariate copula analyses) (\code{U} [site A], \code{V} [site B]), and a \code{cex} column of a scaled symbol size based on the \code{absdays_between_pks}. The \code{cex} have a minimum of 1 and maximum of 2 and if the days between peak is \code{NA}, the \code{cex} is set to 0.9. This preprocessing to have the \code{cex} makes it easier for the user to produce diagnostic plots.}
}
\source{
Original \R by WHA for this package.
}
\author{W.H. Asquith}
\seealso{\code{\link{splitPeakCodes}}
}
\examples{
\dontrun{
AB <- jointPeaks("08167000", "08167500") # }
}
\keyword{Joint Peak Table}
\keyword{Joint Probability}

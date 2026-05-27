\encoding{utf8}
\name{splitPeakCodes}
\alias{splitPeakCodes}
\title{Split the Peak Discharge Qualifications Codes into Separate Columns }
\description{
Split the U.S. Geological Survey (USGS) peak discharge qualifications codes (Asquith and others, 2017; U.S. Geological Survey, 2021) in the \code{peak_cd} column of a peak-streamflow data retrieval from the USGS National Water Information System (NWIS) (U.S. Geological Survey, 2019) in a \code{data.frame} into separate columns of the input \code{data.frame}. The NWIS system stores all the codes within a single database field. It can be useful for graphical (\code{\link{plotPeaks}}) or other statistical study to have single logical variable for each of the codes, and such is the purpose of this function. Note because of the \code{appearsSystematic} field is based computations involving the \code{water_yr} (water year), this function needs the \code{\link{makeWaterYear}} to have been run first; however, the function will autodetect and call that function internally if needed and those effects are provided on the returned \code{data.frame}. (See also \code{\link{plotPeaks}}; see also the \code{inst/legend/} subdirectory of this package for a script to produce a legend as well as \code{inst/legend/legend_camera.pdf}, which has been dressed up in a vector graphics editing program.)
}
\usage{
splitPeakCodes(x, all_peaks_na_okay=FALSE)
}
\arguments{
  \item{x}{A \code{data.frame} having a mandatory column titled \code{peak_cd} with discharge qualification codes. Except for a check on only one station being present in \code{site_no} column, no other information in \code{x} is consulted or otherwise used; and}
  \item{all_peaks_na_okay}{A logical controlling whether a test on all the peak values (\code{peak_va}) being \code{NA} is made and if all the peak values are missing, then \code{NULL} is returned. Because much of this package is built around working with real peak discharges, the default is to be rejectionary to gage-height only streamgages. However, the \code{\link{plotPeaks}} function does have logic to work out ways to make plots of gage-height only streamgages.}
}
\value{
  The \code{x} as originally inputted is returned with the addition of these columns:
  \item{appearsSystematic}{The \code{appearsSystematic} column produced by the \code{splitPeakCodes} function is intended to provide a type of \emph{canonical} flag on which to subset the record, which can be important for many statistical procedures;}
  \item{anyCodes}{Are \emph{any} of the codes that follow present for a given record (row, water year) in the input data;}
  \item{isCode1}{Is a discharge qualification code of 1 present for a given record---Streamflow is a maximum daily average;}
  \item{isCode2}{Is a discharge qualification code of 2 present for a given record---Streamflow is an estimate;}
  \item{isCode3}{Is a discharge qualification code of 3 present for a given record---Streamflow affected by dam failure;}
  \item{isCode4}{Is a discharge qualification code of 4 present for a given record---Streamflow is less than indicated value, which is the minimum recordable value at this site;}
  \item{isCode5}{Is a discharge qualification code of 5 present for a given record---Streamflow affected to an unknown degree by regulation or diversion;}
  \item{isCode6}{Is a discharge qualification code of 6 present for a given record---Streamflow is affected by regulation or diversion;}
  \item{isCode7}{Is a discharge qualification code of 7 present for a given record---Streamflow is a historical peak;}
  \item{isCode8}{Is a discharge qualification code of 8 present for a given record---Streamflow is actually greater than the indicated value;}
  \item{isCode9}{Is a discharge qualification code of 9 present---Streamflow is affected by snow melt, hurricane, ice-jam, or debris-dam breakup;}
   \item{isCodeA}{Is a discharge qualification code of A present for a given record---Year of occurrence is unknown or not exact;}
   \item{isCodeB}{Is a discharge qualification code of B present for a given record---Month or day of occurrence is unknown or not exact;}
   \item{isCodeC}{Is a discharge qualification code of C present for a given record---All or part of the record is affected by urbanization, mining, agricultural changes, channelization, or other anthropogenic activity;}
   \item{isCodeD}{Is a discharge qualification code of D present for a given record---Base streamflow changed during this year;}
   \item{isCodeE}{Is a discharge qualification code of E present for a given record---Only annual peak streamflow available for this year;}
   \item{isCodeF}{Peak supplied by another agency;}
   \item{isCodeO}{Is a discharge qualification code of E present for a given record---Opportunistic value not from systematic data collection. By extension, the presence of this code will trigger the \code{appearsSystematic} to false even if the peak itself otherwise appears part of systematic record from the gap analysis, and}
   \item{isCodeR}{Revised.}
}
\note{
Concerning \code{appearsSystematic}: All records but missing discharges are assumed as systematic records unless the peak streamflows are missing or peaks have a code 7 but there are existing years on either side of individual peaks coded as 7. The logic also handles a so-called \dQuote{roll-on} and \dQuote{roll-off} of the data by only testing the leading or trailing year---except this becomes a problem if multiple stations are involved, so the code will return early with a warning. Importantly, it is possible that some code 7s can be flagged as systematic and these are not necessarily in error. Testing indicates that some USGS Water Science Centers (maintainers of databases) have historically slightly different tendencies in application of the code 7. The USGS NWIS database does not actually contain a field indicating that a peak was collected as part of systematic operation and hence that peak is part of an assumed random sample. Peaks with gage height only are flagged as nonsystematic by fiat---this might not be the best solution over all, but because virtually all statistics use the discharge column this seems okay (feature is subject to future changes).

Concerning \code{anyCodes}, only the regular expression \code{A|B|C|D|O|1|2|3|4|5|6|7|8|9} will trigger setting \code{anyCodes} to \code{TRUE}. For example, it is felt that codes \code{E}, \code{F}, and \code{R} are basically meaningless for statistic study and do not represent the \dQuote{special circumstances} of information as reflected in the aforementioned list of others codes. The codes \code{Bd} and \code{Bm} are uninformative too and the column appending by \code{\link{makeWaterYear}} accommodates the necessary structure for further study of date precision if needed by a developer.

A comprehensive listing of peak codes (\code{peak_cd}) and peak gage heights (\code{gage_cd}) follows (U.S. Geological Survey, 2021):
\preformatted{
peak_cd
  1	----> Discharge is a Maximum Daily Average
  2	----> Discharge is an Estimate
  3	----> Discharge affected by Dam Failure
  4	----> Discharge less than indicated value which is Minimum Recordable
            Discharge at this site
  5	----> Discharge affected to unknown degree by Regulation or Diversion
  6	----> Discharge affected by Regulation or Diversion
  7	----> Discharge is an Historic Peak
  8	----> Discharge actually greater than indicated value
  9	----> Discharge due to Snowmelt, Hurricane, Ice-Jam or Debris Dam breakup
  A	----> Year of occurrence is unknown or not exact
 Bd	----> Day of occurrence is unknown or not exact
 Bm	----> Month of occurrence is unknown or not exact
  C	----> All or part of the record affected by Urbanization, Mining,
            Agricultural changes, Channelization, or other
  D	----> Base Discharge changed during this year
  E	----> Only Annual Maximum Peak available for this year
  F	----> Peak supplied by another agency
  O	----> Opportunistic value not from systematic data collection
  R	----> Revised

gage_cd
  1	----> Gage height affected by backwater
  2	----> Gage height not the maximum for the year
  3	----> Gage height was at a different site and/or datum
  4	----> Gage height below minimum recordable elevation
  5	----> Gage height is an estimate
  6	----> Gage height datum changed during this year
  7	----> Debris, mud, or hyper-concentrated flow
  8	----> Gage height tidally affected
 Bd	----> Day of occurrence is unknown or not exact
 Bm	----> Month of occurrence is unknown or not exact
  F	----> Peak supplied by another agency
  R	----> Revised
}

The authors of the \pkg{MGBT} package want to make a remark that the code 4 in many parts of the United States might not reflect the complete idea of \dQuote{which is minimum recordable discharge at this site.} More generally, the code 4 should be thought of a censoring flag for a \dQuote{less then discharge} (Asquith and others, 2018). There are streamgages that in theory could be recording zero to infinite discharge, but then when information is compiled for the year, the annual peak might be flagged as a less than some threshold and have no bearing on the idea of a minimum recordable for the streamgage itself. Such a situation could stem from multiple gage height recording apparatus measuring culvert-like flow circumstances (Asquith and others, 2018).
}
\references{
Asquith, W.H., Harwell, G.R., and Winters, K.E., 2018, Annual and approximately quarterly series peak streamflow derived from interpretations of indirect measurements for a crest-stage gage network in Texas through water year 2015: U.S. Geological Survey Scientific Investigations Report 2018--5107, 24 p., \doi{10.3133/sir20185107}.

Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site peak-streamflow frequency analyses for very low annual exceedance probabilities: U.S. Geological Survey Scientific Investigation Report 2017--5038, 93 p., \doi{10.3133/sir20175038}.

U.S. Geological Survey, 2019, USGS water data for the Nation: U.S. Geological Survey National Water Information System database, accessed October 11, 2019, at \doi{10.5066/F7P55KJN}.

U.S. Geological Survey, 2021, Peak Streamflow and Stage Special Conditions (PEAK.peak_cd and PEAK.gage_ht_cd).
}
\author{ W.H. Asquith}
\seealso{\code{\link{makeWaterYear}}, \code{\link{plotPeaks}}
}
\examples{
\dontrun{
  # The dataRetrieval package is not required by MGBT algorithms.
  PK <- dataRetrieval::readNWISpeak("08167000", convertType=FALSE)
  PK <- splitPeakCodes(PK)
  names(PK) # See that the columns are there.}
}
\keyword{NWIS operator}

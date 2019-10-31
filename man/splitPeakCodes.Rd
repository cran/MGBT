\encoding{utf8}
\name{splitPeakCodes}
\alias{splitPeakCodes}
\title{ Split the Peak Discharge Qualifications Codes into Separate Columns }
\description{
Split the U.S. Geological Survey (USGS) peak discharge qualifications codes (Asquith and others, 2017) in the \code{peak_cd} column of a peak-streamflow data retrieval from the USGS National Water Information System (NWIS) (U.S. Geological Survey, 2019) in a \code{data.frame} into separate columns of the input \code{data.frame}. The NWIS system stores all the codes within a single database field. It can be useful for graphical (\code{\link{plotPeaks}}) or other statistical study to have single logical variable for each of the codes, and such is the purpose of this function. Note because of the \code{appearsSystematic} field is based computations involving the \code{water_yr} (water year), this function needs the \code{\link{makeWaterYear}} to have been run first; however, the function will autodetect and call that function internally if needed and those effects are provided on the returned \code{data.frame}. (See also the \code{inst/legend/} subdirectory of this package for a script to produce a legend as well as \code{inst/legend/legend_camera.pdf}, which has been dressed up in a vector graphics editing program.)
}
\usage{
splitPeakCodes(x)
}
\arguments{
  \item{x}{A \code{data.frame} having a mandatory column titled \code{peak_cd} with discharge qualification codes. Except for a check on only one station being present in \code{site_no} column, no other information in \code{x} is consulted or otherwise used.}
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
   \item{isCodeE}{Is a discharge qualification code of E present for a given record---Only annual peak streamflow available for this year; and}
   \item{isCodeO}{Is a discharge qualification code of E present for a given record--Opportunistic value not from systematic data collection. By extension, the presence of this code will trigger the \code{appearsSystematic} to false even if the peak itself otherwise appears part of systematic record from the gap analysis.}
}
\note{
Concerning \code{appearsSystematic}: All records but missing discharges are assumed as systematic records unless the peak streamflows are missing or peaks have a code 7 but there are existing years on either side of individual peaks coded as 7. The logic also handles a so-called \dQuote{roll-on} and \dQuote{roll-off} of the data by only testing the leading or trailing year---except this becomes a problem if multiple stations are involved, so the code will return early with a warning. Importantly, it is possible that some code 7s can be flagged as systematic and these are not necessarily in error. Testing indicates that some USGS Water Science Centers (maintainers of databases) have historically slightly different tendencies in application of the code 7. The USGS NWIS database does not actually contain a field indicating that a peak was collected as part of systematic operation and hence that peak is part of an assumed random sample. Peaks with gage height only are flagged as nonsystematic by fiat---this might not be the best solution over all, but because virtually all statistics use the discharge column this seems okay (feature is subject to future changes).
}
\references{
Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site peak-streamflow frequency analyses for very low annual exceedance probabilities: U.S. Geological Survey Scientific Investigation Report 2017--5038, 93 p., \url{https://doi.org/10.3133/sir20175038}.

U.S. Geological Survey, 2019, USGS water data for the Nation: U.S. Geological Survey National Water Information System database, accessed October 11, 2019, at \url{https://doi.org/10.5066/F7P55KJN}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{makeWaterYear}}, \code{\link{plotPeaks}}
}
\examples{
\donttest{
  # The dataRetrieval package is not required by MGBT algorithms.
  PK <- dataRetrieval::readNWISpeak("08167000", convertType=FALSE)
  PK <- splitPeakCodes(PK)
  names(PK) # See that the columns are there.}
}
\keyword{NWIS operator}

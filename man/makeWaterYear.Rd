\encoding{utf8}
\name{makeWaterYear}
\alias{makeWaterYear}
\title{Make Water Year Column}
\description{
Make water year, year, month, and day columns from the date stamp of a U.S. Geological Survey peak-streamflow data retrieval from the National Water Information System (NWIS) (U.S. Geological Survey, 2019) in an \R \code{data.frame} into separate columns of the input \code{data.frame}. Note that NWIS (through \pkg{dataRetrieval} package) will insert padded zeros for missing month and years with in the peak-streamflow data when the variable type conversion is false (see \bold{Examples}). This function will changes those zeros in the separated columns to missing.
}
\usage{
makeWaterYear(x, datestr="peak_dt")
}
\arguments{
  \item{x}{A \code{data.frame} having a default column titled \code{peak_dt} that is in a character format. No other information in \code{x} is consulted or otherwise used. A feature of of the function is that \code{x} could also be simply a vector of characters in the basic \code{"YYYY-MM-DD"}, \code{"YYYY-MM"}, or \code{"YYYY"} structure for the conversions; and}
  \item{datestr}{The column name containing the date on which to parse for creation of new columns. The default is aligned with USGS peak-streamflow data.}
}
\value{
  The \code{x} is returned containing only these columns (if \code{x} is \code{is.character}) or with the addition of these columns:
  \item{year_va}{The calendar year extracted from \code{peak_dt};}
  \item{month_va}{The optional month extracted from \code{peak_dt};}
  \item{day_va}{The optional day extracted from \code{peak_dt}; and}
  \item{water_yr}{The water year, which is not equal to \code{year_va} if \code{month_va} is greater than or equal to 10 (October).}
}
\author{W.H. Asquith}
\seealso{\code{\link{splitPeakCodes}}, \code{\link{plotPeaks}}
}
\references{
U.S. Geological Survey, 2019, USGS water data for the Nation: U.S. Geological Survey National Water Information System database, accessed October 11, 2019, at \doi{10.5066/F7P55KJN}.
}
\examples{
print(makeWaterYear(c("1888-07-00", "1889", "1889-11", "1891-03-04")))
#        date year_va month_va day_va water_yr
#1 1888-07-00    1888        7     NA     1888
#2       1889    1889       NA     NA     1889
#3    1889-11    1889       11     NA     1890
#4 1891-03-04    1891        3      4     1891

\dontrun{
  # The dataRetrieval package is not required by MGBT algorithms.
  PK <- dataRetrieval::readNWISpeak("08167000", convertType=FALSE)
  PK <- makeWaterYear(PK) # Note: The convertType=FALSE is critical.
  names(PK) # See that the columns are there and see that there are padded
  # zeros for missing days for "1869-07-00" and "1936-09-00" and the
  # day_va column will have NAs in lieu of 0s in these examples}
}
\keyword{NWIS operator}
\keyword{Water Year}

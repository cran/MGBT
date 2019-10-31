\encoding{utf8}
\name{makeWaterYear}
\alias{makeWaterYear}
\title{ Make Water Year Column }
\description{
Make water year, year, month, and day columns from the date stamp of a U.S. Geological Survey peak-streamflow data retrieval from the National Water Information System (NWIS) (U.S. Geological Survey, 2019) in an \R \code{data.frame} into separate columns of the input \code{data.frame}.
}
\usage{
makeWaterYear(x)
}
\arguments{
  \item{x}{A \code{data.frame} having a mandatory column titled \code{peak_dt} presented by \cr \code{as.character}.  No other information in \code{x} is consulted or otherwise used.}
}
\value{
  The \code{x} is returned with the addition of these columns:
  \item{year_va}{The calendar year extracted from \code{peak_dt};}
  \item{month_va}{The optional month extracted from \code{peak_dt};}
  \item{day_va}{The optional day extracted from \code{peak_dt}; and}
  \item{water_yr}{The water year, which is not equal to \code{year_va} if \code{month_va} is greater than or equal to 10 (October).}
}
\author{ W.H. Asquith}
\seealso{\code{\link{splitPeakCodes}}, \code{\link{plotPeaks}}
}
\references{
U.S. Geological Survey, 2019, USGS water data for the Nation: U.S. Geological Survey National Water Information System database, accessed October 11, 2019, at \url{https://doi.org/10.5066/F7P55KJN}.
}
\examples{
\donttest{
  # The dataRetrieval package is not required by MGBT algorithms.
  PK <- dataRetrieval::readNWISpeak("08167000", convertType=FALSE)
  PK <- makeWaterYear(PK) # Note: The convertType=FALSE is critical.
  names(PK) # See that the columns are there.}
}
\keyword{NWIS operator}

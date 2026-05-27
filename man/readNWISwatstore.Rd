\encoding{utf8}
\name{readNWISwatstore}
\alias{readNWISwatstore}
\title{Read NWIS WATSTORE-Formatted Period of Record Peak Streamflows and Other Germane Operations}
\description{
Read U.S. Geological Survey (USGS) National Water Information System (NWIS) (U.S. Geological Survey, 2019) peak streamflows. But with a major high-level twist what is retained or produced by the operation. This function retrieves the WATSTORE formatted version of the peak streamflow data on a streamgage by streamgage basis and this is the format especially suitable for the USGS PeakFQ software (U.S. Geological Survey, 2020). That format will reflect the period of record. This function uses direct URL pathing to NWIS to retrieve this format and write the results to the user's file system. The function does not use the \pkg{dataRetrieval} package for the WATSTORE format. Then optionally, this function uses the \pkg{dataRetrieval} package to retrieve a tab-delimited version of the peak streamflows and write those to the user's file system. Peak streamflow visualization with attention to the peak discharge qualification codes and other features is optionally made here by the \code{\link{plotPeaks}} function and optionally a portable document formatted graphics file can be written as well. This function is explicitly design around a single directory for each streamgage to store the retrieved and plotted data.
}
\usage{
readNWISwatstore(siteNumbers, path=".", dirend="d", return.progress=FALSE,
                              tabpk=TRUE, vispk=TRUE, vispdf=TRUE,
                              unlinkpath=FALSE, citeNWISdoi=TRUE,
                              all_peaks_na_okay=TRUE, ...)
}
\arguments{
  \item{siteNumbers}{USGS site number(or multiple sites) as string(s). This is usually an 8 digit number but not exclusively so and is the \code{site_no} slot in the NWIS database. The WATSTORE formatted data will be written into  file name being set to \code{site_no".pkf"} (the extension \code{".pkf"} is to be read as \dQuote{peak flows});}
  \item{path}{A directory path with default to current working directory;}
  \item{dirend}{Optional character content appended to the site number as part of directory creation. For example, if the \code{site_no} is \code{08167000} and the default \code{dirend="d"}, then if the \code{path} is the current working directory, then the full path to the directory staged to contain results of this function is \code{./08167000d};}
  \item{return.progress}{A logical to trigger the return of a two-column data frame of the \code{siteNumbers} and a logical vector of whether peaks appear to have been successfully returned or not by the loop within the function. This table could be useful in identifying which streamgages need to be removed from some parent list during initial data-mining efforts;}
  \item{tabpk}{A logical to trigger writing of the period of record of the peak streamflows in a tab-delimited format by the \code{write.table()} function with the file name being set to \code{site_no".txt"};}
  \item{vispk}{A logical to trigger \code{\link{plotPeaks}} for the peak streamflow visualization;}
  \item{vispdf}{A logical to trigger \code{pdf()} and \code{dev.off()} to create a portable document format of the peak streamflow visualization by \code{\link{plotPeaks}} file name being set to \code{site_no".pdf"};}
  \item{unlinkpath}{A logical to trigger unlinking of the full path ahead of its recreation and then operation of the side effects of this function. The unlinking is recursive, so attention to settings of \code{path} and \code{dirend} are self evident;}
  \item{citeNWISdoi}{A logical to trigger the writing of a \code{CITATION.md} file to the NWIS DOI on the date of the retrieval. It is USGS policy to use the citation to NWIS by its digital object identifier (DOI) and provide an accessed date. The \code{CITATION.md} has been written in a simple markdown format so that software repository infrastructure (such as \code{code.usgs.gov}) would markup this file to a simple web page when selected by the user in an internet browser;}
  \item{all_peaks_na_okay}{A logical controlling how \code{\link{splitPeakCodes}} will behave if there are no non-missing peaks. This is the same argument so named in that function, but has a different default for \code{readNWISwatstore} because the anticipating is that someone using this function is likely doing large batch processing and greater risk of a gage-height only streamgage being in the list; and}
  \item{...}{Additional arguments to pass to \code{\link{plotPeaks}}.}
}
\value{
  No values are returned; this function is used for its file system side effects. Those effects will include \code{".pkf"} (WATSTORE formatted peak streamflow data) and potentially include the following additional file by extension: \code{".pdf"}, \code{".txt"} (tab-delimited peak streamflow data), and \code{CITATION.md} file recording a correct policy-compliant citation to the USGS NWIS DOI number with access date as per additional arguments to this function. This function does not recognize the starting and ending period options that the \pkg{dataRetrieval} package provides because the WATSTORE format is ensured to be period of record. Therefore, the disabling of optional period retrievals ensures that the \code{".pkf"} and \code{".[txt|pdf]"} files have the same data.
}
\author{W.H. Asquith}
\references{
U.S. Geological Survey, 2019, USGS water data for the Nation: U.S. Geological Survey National Water Information System database, accessed October 11, 2019, at \doi{10.5066/F7P55KJN}.

U.S. Geological Survey, 2020, PeakFQ---Flood frequency analysis based on Bulletin 17C and recommendations of the Advisory Committee on Water Information (ACWI) Subcommittee on Hydrology (SOH) Hydrologic Frequency Analysis Work Group (HFAWG), version 7.3.
}
\seealso{\code{\link{plotPeaks}}
}
\examples{
\dontrun{
  # The dataRetrieval package provides a readNWISpeak() function but that works on
  # the tab-delimited like format structure and not the WATSTORE structure. The
  # WATSTORE structure is used by the USGS PeakFQ software and direct URL is needed
  # to acquire such data
  path <- tempdir()
  readNWISwatstore("08167000", path=path)
  # path/08167000d/08167000.[pdf|pkf|txt]
  #                CITATION.md
  # files will be created in the temporary directory }

\dontrun{
  # a gage-height only site, table created, codes split, and then the plot so
  # produced is produced with a message in the middle about all peaks missing
  readNWISwatstore("02146285", path=tempdir(), dirend="d",
            tabpk=TRUE, vispk=TRUE, vispdf=TRUE, unlinkpath=FALSE,
            citeNWISdoi=TRUE, all_peaks_na_okay=TRUE) # }

\dontrun{
  sites <- c("08171000", "02231100", "02317600")
  gotEM <- readNWISwatstore(sites, path=tempdir(),
                            return.progress=TRUE, unlinkpath=FALSE)
  # WHA On January 23, 2023, we see first this message from dataRetrieval:
  # No sites/data found using the selection criteria specified
  # WHA then readNWISwatstore() then adds clarity on which streamgage failed
  #   for streamgage '02231100'
  # WHA because this streamgage does not have any peaks entries and appears to be a
  # WHA measurement only site. The readNWISwatstore() continues and the following
  # WHA warnings occur
  # Warning messages:
  # 1: In makeWaterYear(x) : water year 2019 occurs more than once (x2)
  # 2: In plotPeaks2(pkall, codes = TRUE, showDubNA = TRUE, site = site,  :
  # site '02317600', water year 2019 occurs more than once (x2) (drawing them all)
  # WHA because streamgage 02317600 has two records that process into the 2019
  # WHA water years, which technically is not possible with annual maxima series.
  head(gotEM)
  #      site gotThePeaks
  # 1 08171000        TRUE # successful retrieval w/o identified problems
  # 2 02231100       FALSE # no peak records to retrieve
  # 3 02317600        TRUE # records retrieved but the 2019 water year is duplicated }
}
\keyword{graphics}

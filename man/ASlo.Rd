\encoding{utf8}
\name{ASlo}
\alias{ASlo}
\title{ Regression of a Heuristic Method for Identification of Low Outliers in Texas Annual Peak Streamflow }
\description{
Asquith and others (1995) developed a regression equation based on the first three moments of non-low-outlier truncated annual peak streamflow data in an effort to semi-objectively compute low-outlier thresholds for log-Pearson type III (Bulletin 17B consistent; IACWD, 1982) analyses (Asquith and Slade, 1997). A career hydrologist in for USGS in Texas, Raymond M. Slade, Jr., was particularly emphatic that aggressive low-outlier identification is needed for Texas hydrology as protection from mixed population effects.

WHA and RMS heuristically selected low-outlier thresholds for 262 streamgages in Texas with at least 20 years from unregulated and unurbanized watersheds. These thresholds were then regressed, along with help from Linda Judd, against the product moments of the logarithms (base-10) of the whole of the sample data (zeros not included). The regression equation is
\deqn{\log_{10}[AS_{\mathrm{Texas}}(\mu, \sigma, \gamma)]=1.09\mu-0.584\sigma+0.14\gamma - 0.799\mbox{,}}
where \eqn{AS_{\mathrm{Texas}}} is the low-outlier threshold, \eqn{\mu} is the mean, \eqn{\sigma} is the standard deviation, and \eqn{\gamma} is skew. The R-squared is 0.75, and those authors unfortunately do not appear to list a residual standard error. The suggested limitations are \eqn{1.9 < \mu < 4.842}, \eqn{0.125 < \sigma < 1.814}, and \eqn{-2.714 < \gamma < 0.698}.

The \eqn{AS_{\mathrm{Texas}}} equation was repeated in a footnote in Asquith and Roussel (2009, p. 19) because of difficulty in others acquiring copies of Asquith and others (1995). (File \code{AsquithLOT(1995).pdf} with this package is a copy.) Low-outlier thresholds using this regression were applied before the development of a generalized skew map in Texas (Judd and others, 1996) in turn used by Asquith and Slade (1997). A comparison of \eqn{AS_{\mathrm{Texas}}} to the results of MGBT is shown in the \bold{Examples}.

\bold{The \code{ASlo} equation is no longer intended for any practical application with the advent of the MGBT approach.} It is provided here for historical context only and shows a heuristic line of thought independent from the mathematical rigor provided by TAC and others leading to MGBT. The \eqn{AS_{\mathrm{Texas}}} incidentally was an extensive topic of long conversation between WHA and TAC at the National Surface Water Conference and Hydroacoustics Workshop (USGS Office of Surface Water), March 28--April 1, 2011, Tampa, Florida. The conversation was focused on the critical need for aggressive low-outlier identification in arid to semi-arid regions such as Texas. TAC was showcasing MGBT on a poster.
}
\usage{
ASlo(mu, sigma, gamma)
}
\arguments{
  \item{mu}{The arthimetic mean of the logarithms of non-low-outlier truncated annual peak streamflow data (zeros removed);}
  \item{sigma}{The standard deviation of the logarithms of non-low-outlier truncated annual peak streamflow data (zeros removed); and}
  \item{gamma}{The skewness (product moment) of the logarithms of non-low-outlier truncated annual peak streamflow data (zeros removed).}
}
\value{
  The value for the regression equation \eqn{AS_{\mathrm{Texas}}(\mu, \sigma, \gamma)} after re-transformation.
}
\references{
Asquith, W.H., 2019, lmomco---L-moments, trimmed L-moments, L-comoments, censored \cr L-moments, and many distributions: R package version 2.3.2 (September 20, 2018), accessed March 30, 2019, at \url{https://cran.r-project.org/package=lmomco}.

Asquith, W.H., Slade, R.M., and Judd, Linda, 1995, Analysis of low-outlier thresholds for log-Pearson type III peak-streamflow frequency analysis in Texas, in Texas Water *95, American Society of Civil Engineers First International Conference, San Antonio, Texas, 1995, Proceedings: San Antonio, Texas, American Society of Civil Engineers, pp. 379--384.

Asquith, W.H., and Slade, R.M., 1997, Regional equations for estimation of peak-streamflow frequency for natural basins in Texas: U.S. Geological Survey Water-Resources Investigations Report 96--4307, 68 p., \url{https://pubs.usgs.gov/wri/wri964307/}

Asquith, W.H., and Roussel, M.C., 2009, Regression equations for estimation of annual peak-streamflow frequency for undeveloped watersheds in Texas using an L-moment-based, PRESS-minimized, residual-adjusted approach: U.S. Geological Survey Scientific Investigations Report 2009--5087, 48 p., \url{https://pubs.usgs.gov/sir/2009/5087/}.

Interagency Advisory Committee on Water Data (IACWD), 1982, Guidelines for determining flood flow frequency: Bulletin 17B of the Hydrology Subcommittee, Office of Water Data Coordination, U.S. Geological Survey, Reston, Va., 183 p.

Judd, Linda, Asquith, W.H., and Slade, R.M., 1996, Techniques to estimate generalized skew coefficients of annual peak streamflow for natural basins in Texas: U.S. Geological Survey Water Resources Investigations Report 96--4117, 28 p., \url{https://pubs.usgs.gov/wri/wri97-4117/}
}
\source{
Original \R by WHA for this package.
}
\author{ W.H. Asquith}
\examples{
# USGS 08066300 (1966--2016) # cubic feet per second (cfs)
#https://nwis.waterdata.usgs.gov/nwis/peak?site_no=08066300&format=hn2
Peaks <- c(3530, 284, 1810, 9660,  489,  292, 1000,  2640, 2910, 1900,  1120, 1020,
   632, 7160, 1750,  2730,  1630, 8210, 4270, 1730, 13200, 2550,  915, 11000, 2370,
  2230, 4650, 2750,  1860, 13700, 2290, 3390, 5160, 13200,  410, 1890,  4120, 3930,
  4290, 1890, 1480, 10300,  1190, 2320, 2480, 55.0,  7480,  351,  738,  2430, 6700)
#ASlo(3.3472,     0.4865,    -0.752)     # moments from USGS-PeakFQ (v7.1)
 ASlo(3.34715594, 0.4865250, -0.7517086) # values from lmomco::pmoms(log10(Peaks))
# computes 288 cubic feet per second, and now compare this to MGBT()
# MGBT(Peaks)$LOThres # computes 284 cubic feet per second
# ---***--------------***--- Remarkable similarity! ---***--------------***---
# ---***--------------***--- Not true in all cases. ---***--------------***---
}
\keyword{Texas low outliers}
\keyword{low outlier (definition)}
\keyword{low outlier (Texas)}
\keyword{Bulletin 17B}

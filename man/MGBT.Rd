\encoding{utf8}
\name{MGBT}
\alias{MGBT}
\alias{MGBTnb}
\alias{MGBTcohn2016}
\alias{MGBTcohn2013}
\alias{MGBTcohn2011}
\alias{MGBT17c}
\alias{MGBT17c.verb}
\title{ Multiple Grubbs--Beck Test (MGBT) for Low Outliers }
\description{
Perform the Multiple Grubbs--Beck Test (MGBT; Cohn and others, 2013) for low outliers (LOTs, low-outlier threshold; potentially influential low floods, PILFs) that is implemented in the USGS-PeakFQ software (USGS, 2014; Veilleux and others, 2014) for implementation of Bulletin 17C (B17C) (England and others, 2018). The test internally transforms the data to logarithms (base-10) and thus is oriented for positively distributed data but accommodates zeros in the dataset.

The essence of the MGBT, given the order statistics \eqn{x_{[1:n]} \le x_{[2:n]} \le \cdots \le x_{[(n-1):n]} \le x_{[n:n]}}, is the statistic
\deqn{GB_r = \omega_r =
\frac{ x_{[r:n]} - \mathrm{mean}\{x_{[(r+1)\rightarrow n:n]}\} }
     {\sqrt{\mathrm{var}\{x_{[(r+1)\rightarrow n:n]}\}}}\mbox{,}
}
which is can be computed by \code{MGBTcohn2011} that is a port a function of TAC's used in a testing script that is reproduced in the \bold{Examples} of \code{\link{RSlo}}. Variations of this pseudo-standardization scheme are shown for \code{\link{BLlo}} and \code{\link{RSlo}}. Also, \eqn{GB_r} is the canonical form of the variable \code{eta} in TAC sources and \code{peta}=\code{\link{peta}} will be its associated probability.
}
\usage{
MGBT(...) # A wrapper on MGBT17C()---This is THE function for end users.

     MGBT17c(x, alphaout=0.005, alphain=0, alphazeroin=0.10,
                n2=floor(length(x)/2), napv.zero=TRUE, offset=0, min.obs=0)
MGBT17c.verb(x, alphaout=0.005, alphain=0, alphazeroin=0.10,
                n2=floor(length(x)/2), napv.zero=TRUE, offset=0, min.obs=0)

MGBTcohn2016(x, alphaout=0.005, alphazeroin=0.10, n2=floor(length(x)/2),
                napv.zero=TRUE, offset=0)
MGBTcohn2013(x, alphaout=0.005, alphazeroin=0.10, n2=floor(length(x)/2),
                napv.zero=TRUE, offset=0)
      MGBTnb(x, alphaout=0.005, alphazeroin=0.10, n2=floor(length(x)/2),
                napv.zero=TRUE, offset=0)

MGBTcohn2011(x, r=NULL, n=length(x)) # only computes the GB_r, not a test
}
\arguments{
  \item{...}{Arguments to pass to the MGBT family of functions;}
  \item{x}{The data values and note that base-10 logarithms of these are computed internally except for the operation of the \code{MGBTcohn2011} function, which does not (see \bold{Examples} for \code{\link{RSlo}}). Also protection from zero or negative values is made by the \R function \code{pmax}, and these values are replaced with a \dQuote{small} value of \code{1e-8} and tacitly TAC has assumed that p-values for these will be significantly small and truncated away;}
  \item{alphaout}{Literally the \eqn{\alpha_\mathrm{out}} of Bulletin 17C. This is the significance level of the \dQuote{sweep out} portion of MGBT;}
  \item{alphain}{This is the significance level of the \dQuote{sweep in} portion of MGBT but starts at one plus the order statistic identified by \code{alphaout};}
  \item{alphazeroin}{Literally the \eqn{\alpha_\mathrm{in}} of Bulletin 17C. This is the significance level of the \dQuote{sweep in} portion of MGBT;}
  \item{napv.zero}{A logical switch to reset a returned \code{NA} from \code{\link{RthOrderPValueOrthoT}} to zero. This is a unique extension by WHA based on large-scale batch testing of the USGS peak-values database (see \bold{Note}). This being said, the fall-back to Monte Carlo integration if the numerical integration fails, seems to mostly make this argument superfluous;}
  \item{offset}{The offset, if not \code{NA}, is added from the threshold unless the threshold itself is already zero. In practical application, this offset, if set, would likely be a negative quantity. This argument is a unique extension by WHA;}
  \item{min.obs}{The minimum number of observations. This option is provided to streamline larger applications, but the underlying logic in \code{MGBT17C} is robust and on failures because of small sizes return a threshold of \code{0} anyway;}
  \item{n2}{The number of \code{n2}-smallest values to be evaluated in the MGBT;}
  \item{r}{The number of truncated observations, which can be though of the rth order statistic and below; and}
  \item{n}{The number of observations. It is not clear that TAC intended \code{n} to be not equal to the sample size but TAC chose to not keep the length of \code{x} as determined internally to the function but to have it also available as an argument. Functions \code{\link{BLlo}} and \code{\link{RSlo}} also were designed similarly.}
}
\value{
  The MGBT results as an \R \code{list}:
  \item{index}{The sample size \eqn{n}, the value for \code{n2}, and the three indices of the \dQuote{sweep out,} \dQuote{sweep in,} and \dQuote{sweep in from zero} processing (only for \code{MGBT17c} as this is an extension from TAC);}
  \item{omegas}{The \eqn{GB_r = \omega_r} statistics for which the p-values in \code{pvalues} are shown. These are mostly returned for aid in debugging and verification of the algorithms;}
  \item{x}{The \code{n2}-smallest values in increasing order (only for \code{MGBT17c} as this is an extension from TAC);}
  \item{pvalues}{The p-values of the \code{n2}-smallest values of the sample (not available for \code{MGBT17c} because of algorithm design for speed);}
  \item{klow}{The number of low outliers detected;}
  \item{LOThresh}{The low-outlier threshold for the \code{klow+1} index of the sample (and possibly adjusted by the \code{offset}) or simply zero; and}
  \item{message}{Possibly message in event of some internal difficulty.}
The inclusion of \code{x} in the returned value is to add symmetry because the p-values are present. The inclusion of \eqn{n} and \code{n2} might make percentage computations of inward and outward sweep indices useful in exploratory analyses. Finally, the inclusion of the sweep indices is important as it was through inspection of these that the problems in TAC sources were discovered.
}
\note{
\bold{Porting from TAC sources}---TAC used MGBT for flood-frequency computations by a call
\preformatted{
  oMGBT <- MGBT(Q=o_in@qu[o_in@typeSystematic])
}
in file \code{P3_089(R).txt}, and note the named argument \code{Q=} but consider in the definition \code{Q} is not defined as a named argument. For the \pkg{MGBT} package, the \code{Q} has been converted to a more generic variable \code{x}. Development of TAC's B17C version through the \code{P3_089(R).txt} or similar sources will simply require \code{Q=} to be removed from the \code{MGBT} call.

The original MGBT algorithms in \R by TAC will throw some errors and warnings that required testing elsewhere for completeness (non-\code{NULL}) in algorithms \dQuote{up the chain.} These errors appear to matter materially in pratical application in large-scale batch processing of USGS Texas peak-values data by WHA and GRH. For package \pkg{MGBT}, the TAC computations have been modified to wrap a \code{try()} around the numerical integration within \code{\link{RthOrderPValueOrthoT}} of \code{\link{peta}}, and insert a \code{NA} when the integration fails if and only if a second integration (fall-back) attempt using Monte Carlo integration fails as well.

The following real-world data were discovered to trigger the error/warning messages. These example data crash TAC's MGBT and the data point of 25 cubic feet per second (cfs) is the culpret. If a failure is detected, Monte Carlo integration is attempted as a fall-back procedure using defaults of \code{\link{RthOrderPValueOrthoT}}, and if that integration succeeds, \code{MGBT}, which is not aware, simply receives the p-value. If Monte Carlo integration also fails, then for the implementation in package \pkg{MGBT}, the p-value is either a \code{NA} (\code{napv.zero=FALSE}) or set to zero if \code{napv.zero=TRUE}. Evidence suggests that numerical difficulties are encountered when small p-values are involved.
\preformatted{
  # Peak streamflows for 08385600 (1952--2015, systematic record only)
  #https://nwis.waterdata.usgs.gov/nwis/peak?site_no=08385600&format=hn2
  Data  <- c(  8100, 3300,  680, 14800,  25.0, 7310, 2150, 1110, 5200, 900, 1150,
   1050,  880, 2100, 2280, 2620,   830,  4900,  970,  560,  790, 1900, 830,  255,
   2900, 2100,    0,  550, 1200,  1300,   246,  700,  870, 4350,  870, 435, 3000,
    880, 2650,  185,  620, 1650,   680, 22900, 3290,  584, 7290, 1690, 2220, 217,
   4110,  853,  275, 1780, 1330,  3170,  7070, 2660) # cubic feet per second (cfs)
  MGBT17c(     Data, napv.zero=TRUE)$LOThres    # [1] 185
  MGBT17c.verb(Data, napv.zero=TRUE)$LOThres    # [1] 185
  MGBTcohn2016(Data, napv.zero=TRUE)$LOThres    # [1] 185
  MGBTcohn2013(Data, napv.zero=TRUE)$LOThres    # [1] 185
  MGBTnb(      Data, napv.zero=TRUE)$LOThres    # [1] 185
}
Without having the fall-back Monte Carlo integration in \code{\link{RthOrderPValueOrthoT}}, if \code{napv.zero=} \code{FALSE}, then the low-outlier threshold is 25 cfs, but if \code{napv.zero=TRUE}, then the low-outlier threshold is 185 cfs, which is the value matching USGS-PeakFQ (v7.1) (FORTRAN code base). Hence, the recommendation that \code{napv.zero=TRUE} for the default, though such a setting will for this example will still result in 185 cfs because Monte Carlo integration can not be turned off for the implementation here.

Noting that USGS-PeakFQ (7.1) reports the p-value for the 25 cfs as 0.0002, a test of the \pkg{MGBT} implementation with the backup Monte Carlo integration in \code{\link{RthOrderPValueOrthoT}} shows a p-value of 1.748946e-04 for 25 cfs, which is congruent with the 0.0002 of USGS-PeakFQ (v7.1). Another Monte Carlo integration produced a p-value of 1.990057e-04 for 25 cfs, and thus another result congruent with USGS-PeakFQ (v7.1) is evident.

Using original TAC sources, here are the general errors that can be seen:
\preformatted{
   Error in integrate(peta, lower = 1e-07, upper = 1 - 1e-07, n = n, r = r, :
   the integral is probably divergent In addition:
   In pt(q, df = df, ncp = ncp, lower.tail = TRUE) :
     full precision may not have been achieved in 'pnt[final]'
}
For both the internal implementations of \code{\link{RthOrderPValueOrthoT}} and \code{\link{peta}} error trapping is present to return a \code{NA}. It is not fully known whether the integral appears divergent when \code{pt()} (probability of the t-distribution) reaches an end point in apparent accuracy or not---although, this is suspected. For this package, a \code{suppressWarnings()} has been wrapped around the call to \code{pt()} in \code{\link{peta}} as well as in one other computation that at least in small samples can result in a square root of a negative number (see \bold{Note} under \code{\link{peta}}).

There is another error to trap for this package. If all the data values are identical, a low-outlier threshold set at that value leaks back. This is the motivation for a test added by WHA using \code{length(unique(x)) == 1} in the internals of \code{MGBTcohn2016}, \code{MGBTcohn2013}, and \code{MGBTnb}.

A known warning message might be seen at least in microsamples:
\preformatted{
   MGBT(c( 1, 26300))       # throws    warnings, zero is the threshold
     # In EMS(n, r, qmin) : value out of range in 'lgamma'
   MGBT(c( 1, 26300, 2600)) # throws no warnings, zero is the threshold
}
The author wraps a \code{suppressWarnings()} on the line in \code{\link{EMS}} requiring \code{lgamma()}. This warning appears restricted to nearly a degenerate situation anyway and failure will result in \code{\link{peta}} and therein the situation results in a p-value of unity and hence no risk of identifying a threshold.

Regarding \code{n=length(x)} for \code{MGBTcohn2011}, it is not clear whether TAC intended \code{n} to be not equal to the sample size. TAC chose to not determine the length of \code{x} internally to the function but to have it available as an argument. Also \code{\link{BLlo}} and \code{\link{RSlo}} were designed similarly.

\bold{(1) Lingering Issue of Inquiry}---TAC used a \code{j1} index in \code{MGBTcohn2016}, \code{MGBTcohn2013}, and \code{MGBTnb}, and this index is used as part of \code{alphaout}. The \code{j1} and is not involved in the returned content of the MGBT approach. This seems to imply a problem with the \dQuote{sweep out} approach but TAC inadvertantly seems to make \dQuote{sweep out} work in \code{MGBTnb} but therein creates a \dQuote{sweep in} problem. The \dQuote{sweep in} appears fully operational in \code{MGBTcohn2016} and \code{MGBTcohn2013}. Within the source for \code{MGBTcohn2016} and \code{MGBTcohn2013}, a fix can be made with just one line after the \code{n2} values have been processed. Here is the WHA commentary within the sources, and it is important to note that the fix is not turned on because \bold{use of \code{MGBT17c} via the wrapper \code{MGBT} is the recommended interface}:
\preformatted{
   # ---***--------------***--- TAC CRITICAL BUG ---***--------------***---
   # j2 <- min(c(j1,j2)) # WHA tentative completion of the 17C alogrithm!?!
   # HOWEVER MAJOR WARNING. WHA is using a minimum and not a maximum!!!!!!!
   # See MGBT17C() below. In that if the line a few lines above that reads
   # if((pvalueW[i] < alpha1 )) { j1 <- i; j2 <- i }
   # is replaced with if((pvalueW[i] < alpha1 )) j1 <- i
   # then maximum and not the minimum becomes applicable.
   # ---***--------------***--- TAC CRITICAL BUG ---***--------------***---
}

\bold{(2) Lingering Issue of Inquiry}---TAC used recursion in \code{MGBTcohn2013} and \code{MGBTnb} for a condition \code{j2 == n2}. This recursion does not exist in \code{MGBTcohn2016}. TAC seems to have explored the idea of a modification to the \code{n2} to be related to an idea of setting a limit of at least five retained observations below half the sample (default \code{n2}) when up to half the sample is truncated away. Also in the recursion, TAC resets the two alpha's with \code{alphaout=0.01} from the default of \code{alpha1=0.005} and \code{alphazeroin=0.10}. This reset means that had TAC hardwired these inside \code{MGBTcohn2013}, which partially defeats the purpose of having them as arguments in the first place.

\bold{(3) Lingering Issue of Inquiry}---TAC used recursion in \code{MGBTcohn2013} and \code{MGBTnb} for a condition \code{j2 == n2} but the recursion calls \code{MGBTcohn2013}. Other comments about the recursion in Inquiry (2) about the alpha's are applicable here as well. More curious about \code{MGBTnb} is that the \code{alphazeroin} is restricted to a test on only the p-value for the smallest observation and is made outside the loop through the data. The test is \code{if(pvalueW[1] <} \code{alphazeroin &} \code{j2 == 0)} \code{j2 <- 1}, which is a logic flow that differs from that in \code{MGBTcohn2013} and \code{MGBTcohn2016}.

\bold{On the Offset Argument}---The MGBT approach identifies the threshold as the first order statistic (\eqn{x_{[r+1:n]}}) above that largest \dQuote{outlying} order statistic \eqn{x_{[r:n]}} with the requisite small p-value (see the example in the \bold{Examples} section herein). There is a practical application in which a nudge on the MGBT-returned low-outlier threshold could be useful. Consider that the optional \code{offset} argument is added to the threshold unless the threshold itself is already zero. The offset should be small, and as an example \eqn{{-}0.001} would be a magnitude below the resolution of the USGS peak-values database.

Why? If algorithms other than USGS-PeakFQ are involved in frequency analyses, the concept of \dQuote{in} or \dQuote{out} of analyses, respectively, could be of the form \code{xin <-} \code{x[x > threshold]} and \code{xlo <-} \code{x[x <= threshold]}. Note the \dQuote{less than or equal to} and its assocation with those data to be truncated away, which might be more consistent with the idea of truncation level of left-tail censored data in other datasets for which the MGBT approach might be used. For example, the \code{x2xlo()} function of the \pkg{lmomco} package by Asquith (2019) uses such logic in its implementation of conditional probability adjustment for the presence of low outliers. This logic naturally supports a default truncation for zeros.

Perhaps one reason for the difference in how a threshold is implemented is based on two primary considerations. First, if restricted to choosing a threshold to the sample order statistics, then the \code{MGBT} approach, by returning the first (earliest) order statistics that is not statistically significant, requires \code{x[x < threshold]} for the values to leave out. TAC clearly intended this form. Second, TAC seems to approach the zero problem with \code{MGBT} by replacing all zeros with \code{1e-8} as in
\preformatted{
  log10(pmax(1e-8,x))
}
immediately before the logarithmic transformation. This might be a potential weak link in \code{MGBT}. It assumes that \code{1e-8} is small, which it certainly is for the problem of flood-frequency analysis using peaks in cubic feet per second. TAC has hardwired this value. Reasonable enough.

But such logic thus requires the \code{MGBT} to identify these pseudo-zeros (now \code{1e-8}) in all circumstances as low outliers. Do the algorithms otherwise do this? This approach is attractive for B17C because one does not have to track a subsample of those values greater than zero because the low-outlier test will capture them. An implementation such as Asquith (2019) automatically removes zero without the need to use a low-outlier identification method; hence, Asquith's choice of \code{x[x <= threshold]} with \code{threshold=0} by default for the values to leave out. The inclusion of \code{offset} permits cross compatibility for \pkg{MGBT} package purposes trancending Bulletin 17C.
}
\source{
\code{LowOutliers_jfe(R).txt}, \code{LowOutliers_wha(R).txt}, \code{P3_089(R).txt}---Named \code{MGBT} + \code{MGBTnb}
}
\references{
Asquith, W.H., 2019, lmomco---L-moments, trimmed L-moments, L-comoments, censored \cr L-moments, and many distributions: R package version 2.3.2 (September 20, 2018), accessed March 30, 2019, at \url{https://cran.r-project.org/package=lmomco}.

Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.

Cohn, T.A., England, J.F., Berenbrock, C.E., Mason, R.R., Stedinger, J.R., and Lamontagne, J.R., 2013, A generalized Grubbs-Beck test statistic for detecting multiple potentially influential low outliers in flood series: Water Resources Research, v. 49, no. 8, pp. 5047--5058.

England, J.F., Cohn, T.A., Faber, B.A., Stedinger, J.R., Thomas Jr., W.O., Veilleux, A.G., Kiang, J.E., and Mason, R.R., 2018, Guidelines for determining flood flow frequency Bulletin 17C: U.S. Geological Survey Techniques and Methods, book 4, chap. 5.B, 148 p., \url{https://doi.org/10.3133/tm4B5}

U.S. Geological Survey (USGS), 2018, PeakFQ---Flood frequency analysis based on Bulletin 17B and recommendations of the Advisory Committee on Water Information (ACWI) Subcommittee on Hydrology (SOH) Hydrologic Frequency Analysis Work Group (HFAWG), version 7.2: Accessed November 29, 2018, at \url{https://water.usgs.gov/software/PeakFQ/}.

Veilleux, A.G., Cohn, T.A., Flynn, K.M., Mason, R.R., Jr., and Hummel, P.R., 2014, Estimating magnitude and frequency of floods using the PeakFQ 7.0 program: U.S. Geological Survey Fact Sheet 2013--3108, 2 p., \url{https://dx.doi.org/10.3133/fs20133108}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{RthOrderPValueOrthoT}}
}
\examples{
\donttest{
# USGS 08066300 (1966--2016) # cubic feet per second (cfs)
#https://nwis.waterdata.usgs.gov/nwis/peak?site_no=08066300&format=hn2
Values <- c(3530, 284, 1810, 9660,  489,  292, 1000,  2640, 2910, 1900,  1120, 1020,
   632, 7160, 1750,  2730,  1630, 8210, 4270, 1730, 13200, 2550,  915, 11000, 2370,
  2230, 4650, 2750,  1860, 13700, 2290, 3390, 5160, 13200,  410, 1890,  4120, 3930,
  4290, 1890, 1480, 10300,  1190, 2320, 2480, 55.0,  7480,  351,  738,  2430, 6700)
MGBT(Values) # Results LOT=284 cfs leaving 55.0 cfs (p-value=0.0119) censored.
#$index
#      n      n2    ix_alphaout     ix_alphain   ix_alphazeroin
#     51      25              0              0                1
#$omegas
# [1] -3.781980 -2.268554 -2.393569 -2.341027 -2.309990 -2.237571
# [7] -2.028614 -1.928391 -1.720404 -1.673523 -1.727138 -1.671534
#[13] -1.661346 -1.391819 -1.293324 -1.246974 -1.276485 -1.272878
#[19] -1.280917 -1.310286 -1.372402 -1.434898 -1.226588 -1.237743
#[25] -1.276794
#$x
# [1]   55  284  292  351  410  489  632  738  915 1000 1020 1120 1190 1480 1630 1730
#[17] 1750 1810 1860 1890 1890 1900 2230 2290 2320
#$pvalues
# [1] 0.01192184 0.30337879 0.08198836 0.04903091 0.02949836 0.02700114 0.07802324
# [8] 0.11185553 0.31531749 0.34257170 0.21560086 0.25950150 0.24113157 0.72747052
#[15] 0.86190920 0.89914152 0.84072131 0.82381908 0.78750571 0.70840262 0.55379730
#[22] 0.40255392 0.79430336 0.75515103 0.66031442
#$LOThresh
#[1] 284

# The USGS-PeakFQ (v7.1) software reports:
#   EMA003I-PILFS (LOS) WERE DETECTED USING MULTIPLE GRUBBS-BECK TEST   1     284.0
#      THE FOLLOWING PEAKS (WITH CORRESPONDING P-VALUES) WERE CENSORED:
#            55.0    (0.0123)
# As a curiosity, see Examples under ASlo().#}

\donttest{
# MGBTnb() has a sweep in problem.
SweepIn <- c(1, 1, 3200, 5270, 26300, 38400, 8710, 23200, 39300, 27800, 21000,
  21000, 21500, 57000, 53700, 5720, 10700, 4050, 4890, 10500, 26300, 16600, 20900,
  21400, 10800, 8910, 6360)  # sweep in and out both identify index 2.
MGBT17c(SweepIn, alphaout=0)$LOThres # LOT = 3200 # force no sweep outs
MGBTnb(SweepIn)$LOThres              # LOT = 3200 # because sweep out is the same!
MGBTnb(SweepIn, alphaout=0) # LOT = 1  # force no sweep outs, it fails.}
}
\keyword{MGBT}
\keyword{Multiple Grubbs--Beck Test}
\keyword{low outlier (definition)}
\keyword{Bulletin 17C}

\encoding{utf8}
\name{MGBT-package}
\docType{package}
\alias{MGBT-package}
\title{Multiple Grubbs--Beck Low-Outlier Test}
\description{
The \pkg{MGBT} package provides the Multiple Grubbs--Beck low-outlier test (MGBT) (Cohn and others, 2013), and almost all users are only interested in the function \code{\link{MGBT}}. This function explicitly wraps the recommended implementation of the test, which is \code{\link{MGBT17c}}. Some other studies of low-outlier detection and study of the MGBT and related topic can be found in Cohn and others (2019), Lamontagne and Stedinger (2015), and Lamontagne and others (2016).

The package also provides some handy utility functions for noninterpretive processing of U.S. Geological Survey National Water Information System (NWIS) annual-peak streamflow data. These utilities include \code{\link{makeWaterYear}} that adds the water year and parses the date-time stamp into useful subcomponents, \code{\link{splitPeakCodes}} that splits the peak discharge qualification codes, and that \code{\link{plotPeaks}} plots the peak time series with emphasis on visualization of select codes, zero flow values, and missing records (annual gaps).

The context of this package is useful to discuss. When logarthimic transformations of data prior to parameter estimation of probability models are used and interest in the the right-tail of the distribution exists, the MGBT is effective in adding robustness to flood-frequency analyses. Other similar distributed earth-system data analyses could also benefit from the test. The test can be used to objectively identify \dQuote{low outliers} (generic) or specific to floods, \dQuote{potentially influential low floods} (PILFs)---in general, these terms are synonymous.

Motivations of the \pkg{MGBT} package are related to the so-called \dQuote{Bulletin 17C} guidelines (England and others, 2018) for flood-frequency analyses. These are updated guidelines to those in Bulletin 17B (IACWD, 1982).  Bulletin 17C (B17C) are Federal guidelines for performing flood-frequency analyses in the United States. The MGBT is implemented in the U.S. Geological Survey (USGS)-PeakFQ software (USGS, 2014; Veilleux and others, 2014), which implements much of B17C (England and others, 2018).

The MGBT test is especially useful in practical applications in which small (possibily uninteresting) events (low-magnitude tail, left-tail side) can occur from divergent populations or processes than those forming the high-magnitude tail (right-tail side) of the probability distribution. One such large region of the earth is much of the arid to semi-arid hydrologic setting for much of Texas for which a heuristic method predating and unrelated to MGBT was used for low-outlier threshold identification (see \code{\link{ASlo}}). Arid and semi-arid regions are particularly sensitive to the greater topic motivating the MGBT (Timothy A. Cohn, personal commun., 2007).

\bold{Note on Sources and Historical Preservation}---Various files (\code{.txt}) of \R code are within this package and given and are located within the directory \code{/inst/sources}. The late Timothy A. Cohn (TAC) communicated \R code to WHA (author William H. Asquith) in August 2013 for computation of MGBT within a flood-frequency project of WHA's. The August 2013 code is preserved verbatim in file \code{LowOutliers_wha(R).txt}, which also contains code by TAC to related concepts. Separately, TAC communicated \R code to JFE (contributor John F. England) in 2016 (actually over many years they had extensive and independent communication from those with WHA) for computation of MGBT and other low-outlier related concepts. This 2016 code is preserved verbatim in file \code{LowOutliers_jfe(R).txt}. TAC also communicated \R code to JFE for computation of MGBT and other low-outlier related concepts for production of figures for the MGBT paper (Cohn and others, 2013). (Disclosure, here it is unclear whether the \R code given date as early as this paper or before when accounting for the publication process.)

The code communications are preserved verbatim in file \code{FigureMacros_jfe(R)}\code{.txt} for which that file is dependent on \code{P3_075_jfe(R).txt}. The \code{_jfe} has been added to denote stewardship at some point by JFE. The \code{P3_075_jfe(R).txt} though is superceded by \code{P3_089(R)}\code{.txt} in which TAC was editing as late as the summer of 2016. The \code{P3_089(R).txt} comes to WHA through Julie E. Kiang (USGS, May 2016). This file should be considered TAC's canonical and last version for MGBT as it appears in the last set of algorithms TAC while he was working on development of a USGS-PeakFQ-like Bulletin 17C implementation in \R. As another historical note, file \code{P3_085_wha(R).txt} is preserved verbatim and was given to WHA at the end of November 2015 less than two weeks before TAC dismissed himself for health reasons from their collaboration on a cooperative research project in cooperation with the U.S. Nuclear Regulatory Commission (Asquith and others, 2017).

Because of a need for historical preservation at this juncture, there is considerable excess and directly-unrelated code to MGBT and low-outlier identification in the aforementioned files though MGBT obviously is contained therein. In greater measure, much of the other code is related to the expected moments algorithm (EMA) for fitting the log-Pearson type III distribution to annual flood data. The \pkg{MGBT} package is purely organized around MGBT and related concepts in a framework suitable for more general application than the purposes of B17C and thus the contents of the \code{P3_###(R).txt} series of files. \bold{It is, however, the prime objective of the \pkg{MGBT} package to be nearly plug-in replacement for code presently (2019) bundled into \code{P3_###(R).txt} or derivative products. Therefore, any code related to B17C herein must not be considered canonical in any way.}

\bold{Notes on Bugs in Sources by TAC}---During the core development phase of this package made in order to stabilized history left by TAC and other parts intimately known by JFE (co-author and contributor), several inconsistencies to even bugs manifested. These need very clear discussion.

First, there is the risk that the author (WHA) ported TAC-based \R to code in this package and introduced new problems. Second, there is a chance that TAC had errors and (or) WHA has misunderstood some idiom. Third, as examples herein show, there is (discovery circa June 2017) a great deal of evidence that TAC incompletely ported from presumably earlier(?) FORTRAN, which forms the basis of the USGS-PeakFQ software, that seems correct, into \R---very subtle and technical issues are involved. Fourth, WHA and GRH (contributor George \dQuote{Rudy} Herrmann) in several very large batch processing tests (1,400+ time series of real-world peak streamflows) on Texas data pushed limits of \R numerics, and these are discussed in detail as are WHA's compensating mechanisms. Several questions of apparent bugs or encounters with the edges of \R performance would be just minutes long conversations with TAC, but this is unfortunately not possible.

In short, it appears that several versions of MGBT by TAC in \R incorrectly performed a computation known as \dQuote{swept out} from the median (\code{\link{MGBTcohn2016}} and \code{\link{MGBTcohn2013}}). Curiously a specific branch (\code{\link{MGBTnb}}) seems to fix that but caused a problem in a computation known as \dQuote{sweep in} from the first order statistic.

Further, numerical cases can be found triggering divergent integral warnings from \code{integrate()}---WHA compensated by adding a Monte Carlo integration routine as backup. It is beyond the scope here to speculate on FORTRAN code performance. In regards to \R and numerically, cases can be found trigging numerical precision warnings from the cumulative distribution function of the t-distribution (\code{pt()})---WHA compensated by setting p-value to limiting small value (zero). Also numerically, cases can be found triggering a square-root of a negative number in \code{\link{peta}}---WHA compensates by effectively using a vanishingly small probability of the t-distribution. TAC's MGBT approach fails if all data values are equal---WHA compensates by returning a default result of a zero-value MGBT threshold. This complexity leads to a lengthy \bold{Examples} section in this immediate documentation as well as in the \code{\link{MGBT}} function. All of these issues finally led WHA to preserve within the \pkg{MGBT} package several MGBT-focused implementations as distinct functions.

\bold{Note on Mathematic Nomenclature}---On first development of this package, the mathematics largely represent the port from the sources into a minimal structure to complete description herein. TAC and others have published authoritative mathematics elsewhere. The primary author (WHA) deliberately decided to build the \pkg{MGBT} package up from the TAC sources first. Little reference to TAC's publications otherwise is made.
}
\references{
Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site peak-streamflow frequency analyses for very low annual exceedance probabilities: U.S. Geological Survey Scientific Investigation Report 2017--5038, 93 p., \url{https://doi.org/10.3133/sir20175038}.

Cohn, T.A., 2013--2016, Personal communication of original R source code: U.S. Geological Survey, Reston, Va.

Cohn, T.A., Barth, N.A., England, J.F., Jr., Faber, B.A., Mason, R.R., Jr., and Stedinger, J.R., 2019, Evaluation of recommended revisions to Bulletin 17B: U.S. Geological Survey Open-File Report 2017--1064, 141 p., \url{https://doi.org/10.3133/ofr20171064}.

Cohn, T.A., England, J.F., Berenbrock, C.E., Mason, R.R., Stedinger, J.R., and Lamontagne, J.R., 2013, A generalized Grubbs--Beck test statistic for detecting multiple potentially influential low outliers in flood series: Water Resources Research, v. 49, no. 8, pp. 5047--5058.

England, J.F., Cohn, T.A., Faber, B.A., Stedinger, J.R., Thomas Jr., W.O., Veilleux, A.G., Kiang, J.E., and Mason, R.R., 2018, Guidelines for determining flood flow frequency Bulletin 17C: U.S. Geological Survey Techniques and Methods, book 4, chap. 5.B, 148 p., \url{https://doi.org/10.3133/tm4B5}

Interagency Advisory Committee on Water Data (IACWD), 1982, Guidelines for determining flood flow frequency: Bulletin 17B of the Hydrology Subcommittee, Office of Water Data Coordination, U.S. Geological Survey, Reston, Va., 183 p.

Lamontagne, J.R., and Stedinger, J.R., 2015, Examination of the Spencer--McCuen outlier-detection test for log-Pearson type 3 distributed data: Journal of Hydrologic Engineering, v. 21, no. 3, pp. 04015069:1--7.

Lamontagne, J.R., Stedinger, J.R., Yu, Xin, Whealton, C.A., and Xu, Ziyao, 2016, Robust flood frequency analysis---Performance of EMA with multiple Grubbs--Beck outlier tests: Water Resources Research, v. 52, pp. 3068--3084.

U.S. Geological Survey (USGS), 2018, PeakFQ---Flood frequency analysis based on Bulletin 17B and recommendations of the Advisory Committee on Water Information (ACWI) Subcommittee on Hydrology (SOH) Hydrologic Frequency Analysis Work Group (HFAWG), version 7.2: Accessed November 29, 2018, at \url{https://water.usgs.gov/software/PeakFQ/}.

Veilleux, A.G., Cohn, T.A., Flynn, K.M., Mason, R.R., Jr., and Hummel, P.R., 2014, Estimating magnitude and frequency of floods using the PeakFQ 7.0 program: U.S. Geological Survey Fact Sheet 2013--3108, 2 p., \url{https://dx.doi.org/10.3133/fs20133108}.
}
\author{William H. Asquith (WHA) \email{wasquith@usgs.gov} }
\seealso{
   \code{\link{MGBT}}
}
\examples{
\donttest{
# Peaks for 08165300 (1968--2016, systematic record only)
#https://nwis.waterdata.usgs.gov/nwis/peak?site_no=08385600&format=hn2
Peaks <- c(3200, 44, 5270, 26300, 1230, 55, 38400, 8710, 143, 23200, 39300, 1890,
  27800, 21000, 21000, 124, 21, 21500, 57000, 53700, 5720, 50, 10700, 4050, 4890, 1110,
  10500, 475, 1590, 26300, 16600, 2370, 53, 20900, 21400, 313, 10800, 51, 35, 8910,
  57.4, 617, 6360, 59, 2640, 164, 297, 3150, 2690)

MGBTcohn2016(Peaks)
#$klow
#[1] 24
#$pvalues
# [1] 0.8245714657 0.7685258183 0.6359392507 0.4473443285 0.2151390091 0.0795065159
# [7] 0.0206034851 0.0036001474 0.0003376923 0.0028133490 0.0007396869 0.0001427225
#[13] 0.0011045550 0.0001456356 0.0004178758 0.0004138897 0.0123954279 0.0067934260
#[19] 0.0161448464 0.0207025800 0.0483890616 0.0429628125 0.0152045539 0.0190853626
#$LOThresh
#[1] 3200

# ---***--------------***--- Note the mismatch ---***--------------***---
#The USGS-PeakFQ (v7.1) software reports:
#EMA003I-PILFS (LOS) WERE DETECTED USING MULTIPLE GRUBBS-BECK TEST  16    1110.0
#  THE FOLLOWING PEAKS (WITH CORRESPONDING P-VALUES) WERE CENSORED:
#            21.0    (0.8243)
#            35.0    (0.7680)
#            44.0    (0.6349)
#            50.0    (0.4461)      # Authors' note:
#            51.0    (0.2150)      # Note that the p-values from USGS-PeakFQv7.1 are
#            53.0    (0.0806)      # slightly different from those emanating from R.
#            55.0    (0.0218)      # These are thought to be from numerical issues.
#            57.4    (0.0042)
#            59.0    (0.0005)
#           124.0    (0.0034)
#           143.0    (0.0010)
#           164.0    (0.0003)
#           297.0    (0.0015)
#           313.0    (0.0003)
#           475.0    (0.0007)
#           617.0    (0.0007)
# ---***--------------***--- Note the mismatch ---***--------------***---

# There is a problem somewhere. Let us test each of the TAC versions available.
# Note that MGBTnb() works because the example peaks are ultimately a "sweep out"
# problem. MGBT17c() is a WHA fix to TAC algorithm, whereas, MGBT17c.verb() is
# a verbatim, though slower, WHA port of the written language in Bulletin 17C.
MGBTcohn2016(Peaks)$LOThres # LOT=3200  (WHA sees TAC problem with "sweep out".)
MGBTcohn2013(Peaks)$LOThres # LOT=16600 (WHA sees TAC problem with "sweep out".)
MGBTnb(Peaks)$LOThres       # LOT=1110  (WHA sees TAC problem with "sweep in". )
MGBT17c(Peaks)$index        # LOT=1110  (sweep indices:
                            #   ix_alphaout=16, ix_alphain=16, ix_alphazeroin=0)
MGBT17c.verb(Peaks)$index   # LOT=1110  (sweep indices:
                            #   ix_alphaout=16, ix_alphain=NA, ix_alphazeroin=0)

# Let us now make a problem, which will have both "sweep in" and "sweep out"
# characteristics, and note the zero and unity outliers for the "sweep in" to grab.
Peaks <- c(0,1,Peaks)
MGBTcohn2016(Peaks)$LOThres # LOT=3150         ..ditto..
MGBTcohn2013(Peaks)$LOThres # LOT=16600        ..ditto..
MGBTnb(Peaks)$LOThres       # LOT=1110         ..ditto..
MGBT17c(Peaks)$index        # LOT=1110  (sweep indices:
                            #   ix_alphaout=18, ix_alphain=18, ix_alphazeroin=2
MGBT17c.verb(Peaks)$index   # LOT=1110  (sweep indices:
                            #   ix_alphaout=18, ix_alphain=NA, ix_alphazeroin=2

#The USGS-PeakFQ (v7.1) software reports:
#    EMA003I-PILFS (LOS) WERE DETECTED USING MULTIPLE GRUBBS-BECK TEST  17    1110.0
#      THE FOLLOWING PEAKS (WITH CORRESPONDING P-VALUES) WERE CENSORED:
#               1 ZERO VALUES
#             1.0    (0.0074)
#            21.0    (0.4305)
#            35.0    (0.4881)
#            44.0    (0.3987)
#            50.0    (0.2619)
#            51.0    (0.1107)
#            53.0    (0.0377)
#            55.0    (0.0095)
#            57.4    (0.0018)
#            59.0    (0.0002)
#           124.0    (0.0018)
#           143.0    (0.0006)
#           164.0    (0.0002)
#           297.0    (0.0010)
#           313.0    (0.0002)
#           475.0    (0.0005)
#           617.0    (0.0005) #}
}
\keyword{Multiple Grubbs--Beck Test}
\keyword{Bulletin 17B}
\keyword{Bulletin 17C}

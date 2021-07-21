# README (./MGBT/inst/README.md)

#### Author:           William H. Asquith
#### Point of contact: William H. Asquith (wasquith@usgs.gov)

***
***

This README file provides original source context to the **MGBT** package and other package design notes. These are partial notes and the `man/MGBT-package.Rd` is contains much more information. The `./inst/` subdirectory is standard for _R_ packages. The `CITATION` file is standard for the _R_ language. The documentation below provides a manifest of sorts for the contents of this directory.

# README (`./MGBT/inst/README.md)

***
***

# DIRECTORY `./MGBT/inst/`

**FILE:** `AsquithLOT(1995).pdf` — A hard to find reference concerning low-outlier thresholds in Texas by the author.

**FILE:** `tim-low-outliers-2010-notes.pdf` — John F. England (JFE) visited Reston, Virginia (USGS Headquarters) in Spring 2017 after the passing of Timothy A. Cohn (TAC). He scanned Cohn's personal note books. One is titled "Tim Cohn Low Outliers" and contains 33 pages of mathematical notes. TAC, within `COHN_MGBT_LowOutliers(R).txt`, describes "Orthogonal evaluation of p-value" using "mathematical notes dated December 16, 2009" as spanning 39 pages and that these are "not for the faint of heart ;-)" This note is not found in the file `./sources/P3_089(R).txt`. It does not appear that the 33 pages scanned are the 39 pages being referenced.

**FILE:** `USGSapproval20190924.pdf` — This is a copy of the approval email from USGS internal publication tracking authorizing dissemination (release) of the product.

## Subdirectory `./MGBT/inst/helpers/`

Convenient research scripts related to experimental efforts by William H. Asquith (WHA). These will not necessarily do anything for end users.

## Subdirectory `./MGBT/inst/legend/`

This directory contains reference information and portable document format (PDF) files related to explanation of the `MGBT::plotPeaks()` function.

## Subdirectory `./MGBT/inst/sources/`

This directory holds original source code acquired from TAC at various times by colleagues. It is advised to see also the documentation `./MGBT/man/MGBT-package.Rd`.

### Source Code File `./sources/FigureMacros_jfe(R).txt`

Source code exchanged between TAC and JFE focused on low-outlier thresholds (potentially influential low floods).

### Source Code File `./sources/LowOutliers_jfe(R).txt`

Source code exchanged between TAC and JFE focused on low-outlier thresholds (potentially influential low floods).


### Source Code File `./sources/LowOutliers_wha(R).txt`

On or about August 21, 2013, TAC sent a file in an email attachment titled `COHN_MGBT_LowOutliers.R` to WHA. At the time, WHA was needing an MGBT implementation for a study of "modern periods of statitic flood flow regulation" through flood frequency computations on annual peak streamflow data for USACE Fort Worth. Asquith was using the method of L-moments, but wanted the MGBT as protection against some mixed population effects. This communication to WHA appears to be 100 percent independent of from TAC's personal _R_ code base related to the Expected Moments Algorithm (EMA) and Bulletin 17C (log-Pearson type III distribution) communicated with JFE in the years prior. This file has been retitled to `LowOutliers_wha(R).txt`.

The source files have a `*(R).txt` in their name. This is deliberate. They are _R_ scripts, but _R_ will complain on an `R CMD check MGBT` when any `.R` file exits in the `./inst/` subdirectory of a package. Cohn has many functions related to low-outliers in his code bases, but many are not directly related to MGBT for purposes of implementation.  All of these are ported into the MGBT package and documented. It is clear that Cohn shared functions with Asquith in `./LowOutliers_wha(R).txt` that are not available in the file `./sources/P3_089(R).txt`.  The typical design of _R_ packages by WHA to break as much code into separate `.R` files as possibly (logically) and generally have about a one-to-one pairing of `.Rd` files.

### Source Code File `./sources/P3_075_jfe(R).txt`

Source code exchanged between TAC and JFE at some point in time.

### Source Code File `./sources/P3_085_wha(R).txt`

Source code exchanged between TAC and WHA in November 2015.

### Source Code File `./sources/P3_089(R).txt`

On May 16, 2017, Julie E. Kiang sent was she believes to be TAC's master `.R` file of the EMA-17C code. Within which separate "MGBT" functions from the WHA communication appear present. The short comments by TAC indicate changes as late as June 28, 2016. These are left intact. The **MGBT** package will first pass into maturity as CRAN-compliant through communication between TAC and WHA and then port evidently TAC's later developments. Asquith suggests appending "2013" to the original functions (e.g. MGBT.2013) given to Asquith and eventually use `MGBT()` as the canonical version as embedded into 17C.

## Subdirectory `./MGBT/www/`

This directory is a holding ground for files to enrich the Internet presentation of the package.

# SOME WORKING NOTES ON THE MGBT PACKAGE

Nancy A. Barth has encountered a situation where USGS PeakFQ software and the **MGBT** package at first glance appear divergent from each other in regards to the threshold defined by the MGBT. For now (summer 2019), this paragraph just lists USGS streamflow-gaging station number(s) of such sites; further study is needed.

* 06430898 for the period of record of 1989–2013


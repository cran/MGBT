.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
  "USGS Research Package:
https://owi.usgs.gov/R/packages.html#research
This package is an approved software release, and
is shown to be consistent with USGS-PeakFQ software.",
  collapse="\n"))
}

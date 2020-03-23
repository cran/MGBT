"plotPeaks_batch" <-
function(sites, file=NA, do_plot=TRUE,
                silent=FALSE, envir=NULL, ...) {
  if(! do_plot) file <- NA
  if(is.null(envir)) {
    pkenv <- new.env()
  } else {
    if(is.environment(envir)) {
      pkenv <- envir
    } else {
      stop("envir given but not an environment")
    }
  }

  numpk <- 0
  for(site in sites) {
    if(exists(site, pkenv)) next
    if(! silent) message("Pulling peaks for ",site)
    pk <- NA
    pk <- dataRetrieval::readNWISpeak(site, convert=FALSE)
    suppressWarnings(pk <- splitPeakCodes(pk))
    numpk <- numpk + length(pk$peak_va[pk$appearsSystematic == TRUE])
    if(is.null(pk)) pk <- NA
    assign(site, pk, envir=pkenv)
  }
  if(is.null(envir) & ! silent) message("DONE (num. systematic peaks = ",numpk,")\n")

  numpk <- 0
  empty_sites <- NULL
  if(! is.na(file)) pdf(file, useDingbats=FALSE)
    for(site in sites) {
      if(do_plot & ! silent) message("Plotting ", site, appendLF=FALSE)
      pk <- get(site, envir=pkenv)
      if(length(pk) == 1) {
        if(do_plot & ! silent) message("---empty")
        if(do_plot) {
          plot(0:2,0:2, xlab="", ylab="", xaxt="n", yaxt="n", type="n")
          text( 1,  1,  "EMPTY PEAK FILE")
          mtext(site)
        }
        empty_sites <- c(empty_sites, site)
      } else {
        numpk <- numpk + length(pk$peak_va[pk$appearsSystematic == TRUE])
        if(do_plot) {
          plotPeaks(pk, ...)
          mtext(site)
        }
        if(do_plot & ! silent) message("")
      }
    }
    if(do_plot & ! silent) message("DONE (num. systematic peaks = ",numpk,")")
  if(! is.na(file)) dev.off()
  pkenv <- as.list(pkenv)
  attr(pkenv, "empty_sites") <- empty_sites
  return(pkenv)
}

"makeWaterYear" <- function(x) {
   if(! is.data.frame(x)) {
      stop("a data.frame as in 'dataRetrieval::readNWISpeak(station, convertType=FALSE)' required")
   }
   x$peak_va  <- as.numeric(x$peak_va) # presuming dataRetrieval::readNWISpeak(station, convertType=FALSE)
   x$year_va  <- as.numeric(sapply(x$peak_dt, function(t) strsplit(t, split="-")[[1]][1]))
   x$month_va <- as.numeric(sapply(x$peak_dt, function(t) strsplit(t, split="-")[[1]][2]))
   x$day_va   <- as.numeric(sapply(x$peak_dt, function(t) strsplit(t, split="-")[[1]][3]))
   x$water_yr <- x$year_va
   x$water_yr[! is.na(x$month_va) & x$month_va >= 10] <-
   x$water_yr[! is.na(x$month_va) & x$month_va >= 10] + 1
   return(x)
}

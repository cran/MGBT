"makeWaterYear" <- function(x, datestr="peak_dt") {
   if(is.character(x)) {
     zz <- data.frame(date=x)
     zz$year_va  <- as.numeric(sapply(x, function(t) strsplit(t, split="-")[[1]][1]))
     zz$month_va <- as.numeric(sapply(x, function(t) strsplit(t, split="-")[[1]][2]))
     zz$day_va   <- as.numeric(sapply(x, function(t) strsplit(t, split="-")[[1]][3]))
     zz$water_yr <- zz$year_va
     zz$water_yr[! is.na(zz$month_va) & zz$month_va >= 10] <-
     zz$water_yr[! is.na(zz$month_va) & zz$month_va >= 10] + 1
     zz$month_va[zz$month_va == 0] <- NA
     zz$day_va[  zz$day_va   == 0] <- NA
     hh <- aggregate(zz[, "water_yr"], by=list(zz$water_yr), length)
     hh <- hh[hh$x > 1,]
     if(nrow(hh) > 0) {
       for(i in 1:nrow(hh)) {
         warning("water year ", hh$Group.1[i], " occurs more than once (x", hh$x[i], ")")
       }
     }
     return(zz)
   }
   if(is.data.frame(x)) {
     if(exists(datestr, x)) {
       d <- x[ , datestr]
       x$year_va  <- as.numeric(sapply(d, function(t) strsplit(t, split="-")[[1]][1]))
       x$month_va <- as.numeric(sapply(d, function(t) strsplit(t, split="-")[[1]][2]))
       x$day_va   <- as.numeric(sapply(d, function(t) strsplit(t, split="-")[[1]][3]))
       x$water_yr <- x$year_va
       x$water_yr[! is.na(x$month_va) & x$month_va >= 10] <-
       x$water_yr[! is.na(x$month_va) & x$month_va >= 10] + 1
       x$month_va[x$month_va == 0] <- NA
       x$day_va[  x$day_va   == 0] <- NA
       hh <- aggregate(x[, "water_yr"], by=list(x$water_yr), length)
       hh <- hh[hh$x > 1,]
       if(nrow(hh) > 0) {
         for(i in 1:nrow(hh)) {
           warning("water year ", hh$Group.1[i], " occurs more than once (x", hh$x[i], ")")
         }
       }
       return(x)
     } else {
       warning("'datestr' does not exist in the data.frame 'x', returning unmodified 'x'")
       return(x)
     }
   } else {
     warning("a data.frame with 'peak_dt' as in ",
             "'dataRetrieval::readNWISpeak(site_no, convertType=FALSE)' required")
     return(x)
   }
}

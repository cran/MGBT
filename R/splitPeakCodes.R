"splitPeakCodes" <- function(x) {
   if(! is.data.frame(x)) {
      warning("a data.frame as in 'dataRetrieval::readNWISpeak(station, convertType=FALSE)' required")
      return(NULL)
   }
   nu <- length(unique(x$site_no))
   if(nu > 1) {
      message("more than one site_no encountered, please just use one station ",
              "at a time and then rbind() the successive results on your own, ",
              "testing indicates some difficulty in setting appearsSystematic ",
              "as the algorithms rolls onto or off of a data frame with multiple ",
              "streamgages, it is safer and easier to check if this algorithm ",
              "works for all situations of gaps and code 7 this way")
      return(NULL)
   } else if(nu == 0) {
      message("no rows of peaks to process")
      return(NULL)
   }
   site_for_messaging <- x$site_no[1]

   if(length(x$water_year) == 0) {
     x <- makeWaterYear(x)
   }
   x$appearsSystematic <- TRUE
   x$appearsSystematic[is.na(x$peak_va)] <- FALSE
   tmp <- x[! is.na(x$peak_va),]
   if(length(tmp$peak_va) == 0) {
     warning("no non NA peaks available for ",site_for_messaging,", returning NULL")
     return(NULL)
   }
   ix <- range(tmp$water_yr)[1]:range(tmp$water_yr)[2]
   gap <- sapply(ix, function(i) ifelse(length(tmp$water_yr[tmp$water_yr == i]) == 0,i,NA))
   gap <- gap[! is.na(gap)]

   x$peak_va <- as.numeric(x$peak_va) # presuming dataRetrieval::readNWISpeak(station, convertType=FALSE)
   x$gage_ht <- as.numeric(x$gage_ht) # ..ditto..

   x$anyCodes <- NA
   x$isCode9 <- x$isCode8 <- x$isCode7 <-
   x$isCode6 <- x$isCode5 <- x$isCode4 <-
   x$isCode3 <- x$isCode2 <- x$isCode1 <- FALSE
   x$isCode9 <- x$isCode8 <- x$isCode7 <-
   x$isCodeO <- x$isCodeE <- x$isCodeD <- x$isCodeC <-
   x$isCodeB <- x$isCodeA <- FALSE
   x$isCodeA[grep("A", x$peak_cd)] <- TRUE
   x$isCodeB[grep("B", x$peak_cd)] <- TRUE
   x$isCodeC[grep("C", x$peak_cd)] <- TRUE
   x$isCodeD[grep("D", x$peak_cd)] <- TRUE
   x$isCodeE[grep("E", x$peak_cd)] <- TRUE
   x$isCodeO[grep("O", x$peak_cd)] <- TRUE
   x$isCode1[grep("1", x$peak_cd)] <- TRUE
   x$isCode2[grep("2", x$peak_cd)] <- TRUE
   x$isCode3[grep("3", x$peak_cd)] <- TRUE
   x$isCode4[grep("4", x$peak_cd)] <- TRUE
   x$isCode5[grep("5", x$peak_cd)] <- TRUE
   x$isCode6[grep("6", x$peak_cd)] <- TRUE
   x$isCode7[grep("7", x$peak_cd)] <- TRUE
   x$isCode8[grep("8", x$peak_cd)] <- TRUE
   x$isCode9[grep("9", x$peak_cd)] <- TRUE
   x$anyCodes[grep("A|B|C|D|E|O|1|2|3|4|5|6|7|8|9", x$peak_cd)] <- TRUE
   x$anyCodes[is.na(x$anyCodes)]  <- FALSE
   x$appearsSystematic[x$isCodeO] <- FALSE
   for(seven in c(x$water_yr[x$isCode7], max(x$water_yr))) {
      yrs <- c(gap[gap == (seven-1)], gap[gap == (seven+1)])
      if(length(yrs) != 0) x$appearsSystematic[x$water_yr == seven] <- FALSE
   }
   return(x)
}

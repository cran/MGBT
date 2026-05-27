"jointPeaks" <-
function(Asite_no="--", Bsite_no="--", appearsSystematic=FALSE, a=0,
         Adf=NULL, Bdf=NULL, ...) {
  cda1 <- "--"
  cda2 <- "--"
  if(is.null(Adf)) {
    PK1 <- NULL
    try( PK1 <- dataRetrieval::readNWISpeak(Asite_no, convert=FALSE), silent=TRUE)
    if(is.null(PK1)) {
      message("readNWISpeak('", Asite_no, "') returned NULL, possible internet hickup, returning NULL")
      return(NULL)
    }
    if(length(PK1) == 0) {
      message("site ", Asite_no, " does not seem to have peaks, returning NULL")
      return(NULL)
    }
    PK1 <- splitPeakCodes(PK1)
    SF1 <- attr(PK1, "siteInfo")         # dataRetrieval appends site information
    PK1 <- as.data.frame(as.matrix(PK1)) # removes the attributes by dataRetrieval
    PK1$peak_va <- as.numeric(PK1$peak_va)
    cda1 <- pmin(SF1$drain_area_va, SF1$contrib_drain_area_va, na.rm=TRUE)
    if(exists("dec_lat_va",   SF1)) PK1$dev_lat_va   <- SF1$dec_lat_va  # defensive programming
    if(exists("dec_long_va",  SF1)) PK1$dev_long_va  <- SF1$dec_long_va # defensive programming
    if(exists("contrib_drain_area_va",  SF1)) PK1$nwiscda_sqmi <- cda1  # defensive programming
  } else {
    PK1 <- Adf
    nm  <- names(PK1)
    if(length(grep("nwiscda_sqmi", nm)) == 1) cda1 <- Adf[, grep("nwiscda_sqmi", nm)][1]
  }

  if(is.null(Bdf)) {
    PK2 <- NULL
    try( PK2 <- dataRetrieval::readNWISpeak(Bsite_no, convert=FALSE), silent=TRUE)
    if(is.null(PK2)) {
      message("readNWISpeak('", Bsite_no, "') returned NULL, possible internet hickup, returning NULL")
      return(NULL)
    }
    if(length(PK2) == 0) {
      message("site ", Bsite_no, " does not seem to have peaks, returning NULL")
      return(NULL)
    }

    PK2 <- splitPeakCodes(PK2)
    SF2 <- attr(PK2, "siteInfo")         # dataRetrieval appends site information
    PK2 <- as.data.frame(as.matrix(PK2)) # removes the attributes by dataRetrieval
    PK2$peak_va <- as.numeric(PK2$peak_va)
    cda2 <- pmin(SF2$drain_area_va, SF2$contrib_drain_area_va, na.rm=TRUE)
    if(exists("dec_lat_va",   SF2)) PK2$dev_lat_va   <- SF2$dec_lat_va  # defensive programming
    if(exists("dec_long_va",  SF2)) PK2$dev_long_va  <- SF2$dec_long_va # defensive programming
    if(exists("contrib_drain_area_va",  SF2)) PK2$nwiscda_sqmi <- cda2  # defensive programming
  } else {
    PK2 <- Bdf
    nm  <- names(PK2)
    if(length(grep("nwiscda_sqmi", nm)) == 1) cda2 <- Bdf[, grep("nwiscda_sqmi", nm)][1]
  }

  strs <- c("appearsSystematic", "anyCodes", "isCode1", "isCode2", "isCode3", "isCode4", "isCode5",
            "isCode6", "isCode7", "isCode8", "isCode9", "isCodeA", "isCodeB", "isCodeC", "isCodeD",
            "isCodeE", "isCodeF", "isCodeO")
  for(str in strs) { # the as.data.frame(as.matrix()) operations above break the logical type cast
    PK1[,str] <- as.logical(PK1[,str])
    PK2[,str] <- as.logical(PK2[,str])
  }

  if(appearsSystematic) {
    PK1z <- PK1[PK1$appearsSystematic, ]
    PK2z <- PK2[PK2$appearsSystematic, ]
  } else {
    PK1z <- PK1
    PK2z <- PK2
  }

  allwy <- sort( unique( c(PK1z$water_yr, PK2z$water_yr) ) )
  D <- NULL
  for(wy in allwy) {
    pk1 <- PK1z$peak_va[PK1z$water_yr == wy]
    if(length(pk1) == 0) pk1 <- NA
    pk2 <- PK2z$peak_va[PK2z$water_yr == wy]
    if(length(pk2) == 0) pk2 <- NA
    df  <- data.frame(water_yr=wy, Asite_no=Asite_no, Apeak_va=pk1, Bsite_no=Bsite_no, Bpeak_va=pk2)
    D   <- rbind(D, df)
  }

  if(nrow(D) == 0) {
    warning("not matching water year peaks in data.frame D, returning NULL")
    return(NULL)
  }
  J <- D[complete.cases(D), ]
  if(nrow(J) == 0) {
    warning("not matching water year peaks in data.frame J, returning NULL")
    return(NULL)
  }


  row.names(J) <- NULL # cleans things up, remember numbers not sync'd when rows deleted, decorative
  J$Apeak_dt <- NA     # stub out
  J$Bpeak_dt <- NA     # stub out
  J$Anwiscda_sqmi <- cda1 # insert the contributing drainage area
  J$Bnwiscda_sqmi <- cda2 # insert the contributing drainage area
  for(wy in J$water_yr) {
    J$Apeak_dt[J$water_yr == wy] <- PK1$peak_dt[PK1$water_yr == wy]
    J$Bpeak_dt[J$water_yr == wy] <- PK2$peak_dt[PK2$water_yr == wy]
  }
  J <- J[, c("water_yr", "Asite_no", "Bsite_no", "Anwiscda_sqmi", "Bnwiscda_sqmi",
                         "Apeak_dt", "Bpeak_dt", "Apeak_va", "Bpeak_va")]
  J$absdays_between_pks <- as.numeric( abs(as.Date(J$Apeak_dt) - as.Date(J$Bpeak_dt)) )
  J$U <- round(lmomco::pp(J$Apeak_va, a=a, sort=FALSE), digits=8)
  J$V <- round(lmomco::pp(J$Bpeak_va, a=a, sort=FALSE), digits=8)

  J$cex <- as.numeric(J$absdays_between_pks/366) + 1
  J$cex[is.na(J$cex)] <- 1
  zz <- list(Asite_no=PK1, Bsite_no=PK2, AB=J)
  return(zz)
}

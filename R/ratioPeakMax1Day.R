"ratioPeakMax1Day" <-
function(siteNumber, dvenv=NULL, pkenv=NULL, as.list=FALSE,
                     missing.days=0, rm.ratios.lt1=TRUE, silent=TRUE, ...) {

  siteNumber <- siteNumber[1]

  attrs <- c("url", "siteInfo", "variableInfo", "disclaimer", "statisticInfo",
                   "queryTime", "headerInfo") # named attributes for DVs and peaks by dataRetrieval

  if(! is.null(dvenv)) {
    if(is.environment(dvenv)) {
      if(   exists(siteNumber, envir=dvenv)) {
        dv <-  get(siteNumber, envir=dvenv)
      } else {
        if(! silent) message(siteNumber, " daily values for site ", siteNumber, " are not in dvenv object")
        return(NULL)
      }
    }
  } else {
    if(! silent) message(siteNumber, " reading daily values from NWIS")
    dv <- dataRetrieval::readNWISdv(  siteNumber, parameterCd="00060", ...)
    dv <- dataRetrieval::renameNWISColumns( dv )
    for(str in attrs) attr(dv, which=str) <- NULL
  }
  if(is.null(dv)) {
    if(! silent) message(siteNumber, " dv object is NULL")
    return(NULL)
  }
  if(nrow(dv) == 0) {
    if(! silent) message(siteNumber, " number of rows in dv object is zero")
    return(NULL)
  }
  if(! exists("Date", dv)) {
    if(! silent) message(siteNumber, " daily values do not have a column named Date")
    if(! as.list) return(NULL)
    return(list(dv=dv, pk=NULL))
  }
  if(! exists("Flow", dv)) {
    if(! silent) message(siteNumber, " daily values do not have a column named Flow")
    if(! as.list) return(NULL)
    return(list(dv=dv, pk=NULL))
  }

  dv$year  <- as.integer( strftime(dv$Date, format="%Y", usetz=FALSE) )
  dv$month <- as.integer( strftime(dv$Date, format="%m", usetz=FALSE) )
  dv$water_yr <- dv$year + as.integer(dv$month >= 10)

  if(! is.null(pkenv)) {
    if(is.environment(pkenv)) {
      if(   exists(siteNumber, envir=pkenv)) {
        pk <-  get(siteNumber, envir=pkenv)
      } else {
        if(! silent) message(siteNumber, " peak values for site ", siteNumber, " are not in pkenv object")
        return(NULL)
      }
    }
  } else {
    if(! silent) message("reading peak values from NWIS")
    pk <- dataRetrieval::readNWISpeak(siteNumber, convertType=FALSE  )
    for(str in attrs) attr(pk, which=str) <- NULL
  }
  if(is.null(pk)) {
    if(! silent) message(siteNumber, " peaks nonexistent ( pk  == NULL)")
    if(! as.list) return(NULL)
    return(list(dv=dv, pk=pk))
  }
  if(nrow(pk) == 0) {
    if(! silent) message(siteNumber, " peaks nonexistent (nrow ==    0)")
    if(! as.list) return(NULL)
    return(list(dv=dv, pk=pk))
  }


  pk <- MGBT::splitPeakCodes( pk )
  pk <- pk[! pk$isCodeO, ] # remove all opportunistic peaks
  pk <- pk[pk$appearsSystematic, ]
  if(nrow(pk) == 0) {
    if(! silent) message(siteNumber, " systematic peaks nonexistent")
    if(! as.list) return(NULL)
    return(list(dv=dv, pk=pk))
  }

  myleap_year <- function(year) { # near copy of the lubridate::leap_year() function but only for
    (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0)) } # numeric year

  if(nrow(dv) == 0) {
    if(! silent) message(siteNumber, " no rows in dv to aggregate1")
    if(! as.list) return(NULL)
    return(list(dv=dv, pk=pk, mx1d=mx1d, ratios.less.than.one=yy, ratios=zz))
  }
  h  <- aggregate(dv, by=list(dv$water_yr), length);# print("aggregate1")
  h$isLeapYear <- myleap_year(h$Group.1)
  h$days_in_year <- 365
  h$days_in_year[h$isLeapYear] <- 366
  h  <- h[h$Date >= h$days_in_year - missing.days, ]

  if(nrow(h) == 0) {
    if(! silent) message(siteNumber, " no rows in h after missing.days check in aggregate1")
    if(! as.list) return(NULL)
    return(list(dv=dv, pk=pk, mx1d=mx1d, ratios.less.than.one=yy, ratios=zz))
  }

  dv <- dv[dv$water_yr %in% h$Group.1, ]
  if(nrow(dv) == 0) {
    if(! silent) message(siteNumber, " no rows in dv to aggregate2")
    if(! as.list) return(NULL)
    return(list(dv=dv, pk=pk, mx1d=mx1d, ratios.less.than.one=yy, ratios=zz))
  }
  pk <- pk[pk$water_yr %in% h$Group.1, ]

  mx1d <- aggregate(dv, by=list(dv$water_yr), max); #print("aggregate2")
  mx1d$dvflow_count <- NA
  for(water_yr in h$Group.1) {
    mx1d$dvflow_count[mx1d$water_yr == water_yr] <- h$water_yr[h$Group.1 == water_yr]
  }
  if(nrow(pk) == 0) {
    if(! silent) message(siteNumber, " peaks nonexistent after dv water year missing days check")
    if(! as.list) return(NULL)
    return(list(dv=dv, pk=pk, mx1d=mx1d))
  }

  if(nrow(pk) != nrow(mx1d)) {
    if(! silent) message("ALERT: ", siteNumber, " row numbers in the dv and pk tables are not equal")
    zz <- merge(pk, mx1d, by="water_yr", all=TRUE)
    zz <- zz[! is.na(zz$agency_cd.x), ]; zz <- zz[! is.na(zz$agency_cd.y), ]
    zz <- data.frame(site_no     = zz$site_no.x, water_yr  = as.integer(zz$water_yr),
                     dvflow_count= zz$dvflow_count,
                     peak_flow   = zz$peak_va,   mx1d_flow = zz$Flow)
  } else {
    zz <- data.frame(site_no     = pk$site_no,   water_yr  = as.integer(pk$water_yr),
                     dvflow_count=mx1d$dvflow_count,
                     peak_flow   = pk$peak_va,   mx1d_flow = mx1d$Flow)
  }
  zz <- zz[complete.cases(zz), ]
  zz <- zz[zz$peak_flow > 0,   ]
  zz <- zz[zz$mx1d_flow > 0,   ]
  zz$ratio     <- round(      zz$peak_flow  /       zz$mx1d_flow,  digits=8)
  zz$log10diff <- round(log10(zz$peak_flow) - log10(zz$mx1d_flow), digits=8)

  if(rm.ratios.lt1) {
    yy <- zz[zz$ratio <  1, ]
    zz <- zz[zz$ratio >= 1, ]
    if(nrow(zz) == 0) {
      if(! silent) message(siteNumber, " no records after ratio >= 1 check (rm.ratios.lt1)")
      if(! as.list) return(NULL)
      return(list(dv=dv, pk=pk, mx1d=mx1d, ratios.less.than.one=yy, ratios=zz))
    }
  }

  if(as.list) {
    zz <- list(dv=dv, pk=pk, mx1d=mx1d, ratios.less.than.one=yy, ratios=zz)
  }

  return(zz)
}

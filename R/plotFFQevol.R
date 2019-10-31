"plotFFQevol" <-
function(pkenv, lot=NULL, finalquas=NULL, log10offsets=NULL,
         minyrs=10, byr=1940, edgeyrs=c(NA, NA), logyaxs=TRUE,
         lego=NULL, maxs=NULL, mins=NULL, names=NULL, auxeyr=NA,
         xlab="Water Year", ylab="Peak streamflow, in cubic feet per second",
         title="Time Evolution of Peak-Frequency Streamflow Estimates\n",
         data_ext="_data.txt", ffq_ext="_ffq.txt", path=tempdir(),
         showfinalyr=TRUE, usewyall=FALSE, silent=TRUE, ...) {

  if(! is.environment(pkenv)) {
    warning("require an environment holding the peaks along the lines of:\n",
            "pks <- dataRetrieval::readNWISpeak(station, convertType=FALSE);\n",
            "pks <- MGBT::splitPeakCodes(MGBT::makeWaterYear(data))")
    return(NULL)
  }
  if(! is.data.frame(finalquas)) {
    warning("require a data.frame holding the final quantiles and the final water year\n")
    return(NULL)
  }

  plotem <- ifelse(is.null(log10offsets), FALSE, TRUE)
  stations <- ls(pkenv)
  tyrs <- c(2, 5, 10, 25, 50, 100, 200, 500)
  FF <- lmomco::T2prob(tyrs)

  Maroon <- grDevices::rgb(159/256,  59/256,  56/256)
  Purple <- grDevices::rgb(112/256,  48/256, 160/256)
  Green  <- grDevices::rgb(175/256, 201/256, 122/256)
  Blue   <- grDevices::rgb(114/256, 154/256, 202/256)

  n <- length(stations)
  zz <- data.frame(site_no=rep(NA,n), Q002=rep(NA,n), Q005=rep(NA,n),
                      Q010=rep(NA,n), Q025=rep(NA,n), Q050=rep(NA,n),
                      Q100=rep(NA,n), Q200=rep(NA,n), Q500=rep(NA,n))
  zz <- zz[0,] # will become the offset values

  for(i in 1:length(stations)) {
     station <- stations[i]; lothres <- lot[i]
     if(! silent) message("Working on ",station, " ", appendLF=FALSE)
     fv  <- finalquas[  finalquas$site_no == station, ]
     if(length(fv$site_no) == 0) {
       warning("could not find ",station," on the finalquas argument data.frame")
       next
     }
     fvo <- log10offsets[log10offsets$site_no == station, ]
     if(is.null(fvo)) {
       fvo <- data.frame(site_no=station,
                         Q002=0, Q005=0, Q010=0, Q025=0,
                         Q050=0, Q100=0, Q200=0, Q500=0)
     } else if(length(fvo$site_no) == 0) {
       warning("could not find ",station," on the logoffsets argument data.frame")
       next
     }
     minyears <- range(c(byr,fv$final_water_yr, auxeyr), na.rm=TRUE)
     minyears <- minyears[1]:minyears[2]
     ffq050 <- ffq025 <- ffq010 <- ffq005 <- ffq002 <-
        data.frame(water_yr=minyears, quantile=rep(NA, length(minyears)))
     ffq500 <- ffq200 <- ffq100 <- ffq050
     pks <- get(station, pkenv); pks <- pks[! is.na(pks$peak_va), ]
     Qall <- pks$peak_va; Wall <- pks$water_yr; appearsSystematic.all <- pks$appearsSystematic
     if(plotem) {
       xlim <- range(c(edgeyrs, Wall[pks$appearsSystematic]), na.rm=TRUE)
       if(usewyall) xlim <- range(c(edgeyrs, Wall[! pks$appearsSystematic]), na.rm=TRUE)
       ylim <- c(mins[i],maxs[i])
     }
     if(plotem & logyaxs) {
        tmp <- c(mins[i],maxs[i]); tmp <- tmp[tmp > 0]
        plotPeaks(pks, lot=lothres, xlab=xlab, ylab=ylab, xlim=xlim, xlim.inflate=FALSE, aux.y=tmp)
     } else if(plotem) {
        plot(Wall[appearsSystematic.all],Qall[appearsSystematic.all], xaxs="i", yaxs="i", xlim=xlim,
             ylim=ylim, pch=16, tcl=0.5,
             xlab=xlab, ylab=ylab)
        points(Wall[! appearsSystematic.all], Qall[! appearsSystematic.all], pch=16, cex=1, col=6)
    }
    if(plotem) {
      if(showfinalyr) lines(rep(fv$final_water_yr,2), par()$usr[3:4], lty=4, lwd=0.8)
      lines(xlim, rep(lothres, 2), lty=2, lwd=0.8)
      if(! is.null(title)) mtext(paste0(title, names[i]), cex=0.90)
    }
    for(j in 1:length(minyears)) {
       minyear <- minyears[j]
       pks <- pks[pks$appearsSystematic,]
       W <- pks$water_yr[pks$water_yr <= minyear]
       Q <- pks$peak_va[ pks$water_yr <= minyear]
       if(length(Q) < minyrs) {
         if(! silent) message(" skipping ", minyear)
         next;
       }
       xlo <- lmomco::x2xlo(Q, leftout=lothres, a=0); #print(xlo); stop()
       lmr <- lmomco::lmoms(log10(xlo$xin)); pe3lmr <- lmomco::parpe3(lmr)
       ffq <- 10^lmomco::quape3(lmomco::f2flo(FF, pp=xlo$pp), pe3lmr)
       ffq002[j, 2] <- ffq[1]; ffq005[j, 2] <- ffq[2]; ffq010[j, 2] <- ffq[3]
       ffq025[j, 2] <- ffq[4]; ffq050[j, 2] <- ffq[5]; ffq100[j, 2] <- ffq[6]
       ffq200[j, 2] <- ffq[7]; ffq500[j, 2] <- ffq[8]

       if(minyear == fv$final_water_yr) {
         if(! silent) message("intercepting final water year=", minyear)
         offQ002 <- log10(fv$Q002/ffq002[j,2]); offQ005 <- log10(fv$Q005/ffq005[j,2])
         offQ010 <- log10(fv$Q010/ffq010[j,2]); offQ025 <- log10(fv$Q025/ffq025[j,2])
         offQ050 <- log10(fv$Q050/ffq050[j,2]); offQ100 <- log10(fv$Q100/ffq100[j,2])
         offQ200 <- log10(fv$Q200/ffq200[j,2]); offQ500 <- log10(fv$Q500/ffq500[j,2])
         zz <- rbind(zz, data.frame(site_no=station,
                          Q002=offQ002, Q005=offQ005, Q010=offQ010, Q025=offQ025,
                          Q050=offQ050, Q100=offQ100, Q200=offQ200, Q500=offQ500))
       }
       ffq002[j, 2] <- 10^(log10(ffq002[j, 2]) + fvo$Q002)
       ffq005[j, 2] <- 10^(log10(ffq005[j, 2]) + fvo$Q005)
       ffq010[j, 2] <- 10^(log10(ffq010[j, 2]) + fvo$Q010)
       ffq025[j, 2] <- 10^(log10(ffq025[j, 2]) + fvo$Q025)
       ffq050[j, 2] <- 10^(log10(ffq050[j, 2]) + fvo$Q050)
       ffq100[j, 2] <- 10^(log10(ffq100[j, 2]) + fvo$Q100)
       ffq200[j, 2] <- 10^(log10(ffq200[j, 2]) + fvo$Q200)
       ffq500[j, 2] <- 10^(log10(ffq500[j, 2]) + fvo$Q500)
    }
    if(plotem) {
      lines(minyears, ffq002$quantile, lwd=1.25, col=Blue  )
      lines(minyears, ffq010$quantile, lwd=1.25, col=Green )
      lines(minyears, ffq100$quantile, lwd=1.25, col=Purple)
      lines(minyears, ffq500$quantile, lwd=1.25, col=Maroon)
      if(! is.null(lego)) {
        legend(lego[i], maxs[i],
             c("Annual peak streamflow value (filled grey to avoid confusion)",
               "2-year return period estimate of streamflow",
               "10-year return period estimate of streamflow",
               "100-year return period estimate of streamflow",
               "500-year return period estimate of streamflow",
               "Low-outlier threshold used (if line is present)"),
              bty="n", lwd=c(NA,rep(1.25, 4),0.8), lty=c(NA,1,1,1,1,2),
              pch=c(21,NA,NA,NA,NA,NA), pt.bg=c(8,NA,NA,NA,NA,NA),cex=0.85,
              col=c(1, Blue, Green, Purple, Maroon))
      }
    }
    datafile <- paste0(path,"/",stations[i],data_ext)
    if(! is.na(data_ext)) {
       if(! silent) message("writing external datafile=",datafile)
       write.table(data.frame(water_yr=Wall, peak_va=Qall, appearsSystematic=appearsSystematic.all),
                   file=datafile, sep=",", quote=FALSE, row.names=FALSE)
    }
    tmp <- merge(ffq002, ffq010, all=TRUE, by="water_yr")
    names(tmp) <- c("water_yr", "ffq002", "ffq010")           # see Warning below
    tmp <- merge(tmp,    ffq100, all=TRUE, by="water_yr")
    names(tmp) <- c("water_yr", "ffq002", "ffq010", "ffq100") # see Warning below
    tmp <- merge(tmp,    ffq500, all=TRUE, by="water_yr")     # see Warning below
    names(tmp) <- c("water_yr", "ffq002", "ffq010", "ffq100", "ffq500")
    #Warning in merge.data.frame(tmp, ffq500, all = TRUE, by = "water_yr") :
    #column names 'quantile.x', 'quantile.y' are duplicated in the result
    # Technically, the third merge triggers a warning. Instead of suppressWarnings()
    # wrapper, it is felt better to just continuous rename the columns for better documentation.

    ffqfile <- paste0(path,"/",stations[i],ffq_ext)
    if(! is.na(ffq_ext)) {
      if(! silent) message("writing external ffqfile=",ffqfile)
      write.table(tmp, file=ffqfile, sep=",", quote=FALSE, row.names=FALSE)
    }
  }
  if(! plotem) return(zz)
}


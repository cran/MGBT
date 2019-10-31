"plotPeaks" <-
function(x, codes=TRUE, lot=NULL, site="",
            xlab="", ylab="", xlim=NULL, ylim=NULL,
            xlim.inflate=TRUE, ylim.inflate=TRUE, aux.y=NULL,
            log.ticks=c(1, 2, 3, 5, 8),
            show48=FALSE, showDubNA=FALSE, showGHyrs=TRUE, ...) {
  if(! exists("anyCodes", as.list(x))) x <- splitPeakCodes(x)
  x$peak_va <- as.numeric(x$peak_va) # just incase--expectation is that
  # readNWISpeak is called with convertType=FALSE to more seemingless deal with dates
  x$gage_ht <- as.numeric(x$gage_ht)
  nwis_double_nullyrs <- x$water_yr[is.na(x$peak_va) &   is.na(x$gage_ht)]
  na_peak_gage_avail  <- x$water_yr[is.na(x$peak_va) & ! is.na(x$gage_ht)]
  x <- x[! is.na(x$peak_va),]
  if(! exists("water_yr", as.list(x))) x <- makeWaterYear( x)
  xrng <- range(x$water_yr) # note x-range is not potential xlim until later
  gap <- xrng[1]:xrng[2]; #print(gap)
  gap <- sapply(gap, function(i) ifelse(length(x$water_yr[x$water_yr == i]) == 0,i,NA)); #print(gap)
  gap <- gap[! is.na(gap)]; #print(gap)

  if(is.null(lot)) lot <- MGBT(x$peak_va)$LOThresh
  if(is.null(xlim)) xlim <- xrng
  if(  showGHyrs  ) xlim <- range(c(xlim, na_peak_gage_avail))
  if(is.null(ylim)) ylim <- range(x$peak_va[x$peak_va > 0] )
  # This is a little nudger. What could happen without this is say a maximum
  # being exactly 50,000 cfs, which would place that data point on the top axis.
  # The ylim.inflate does not quite catch this otherwise. So the following 0.01
  # log10 cycle seems to work okay.
  if(ylim.inflate)     ylim <- 10^c(log10(ylim)+c(-0.01,0.01))
  if(! is.null(aux.y)) ylim <- range(c(ylim, aux.y), na.rm=TRUE)

  if(xlim.inflate) {
    xlim <- c(as.integer(xlim[1]/10-1)*10, as.integer(xlim[2]/10+1)*10)
  }
  if(ylim.inflate) {#print(ylim)
    ylim <- 10^(log10(ylim)+6) # complicating sign reversals at log10(1)
    # so let us offset six orders of magnitude and reverse the offset later
    needs <- log10(1:10)
    wants <- log10(ylim) - as.integer(log10(ylim))
    left  <- ifelse(wants[1] == 0, 0, rev(needs[needs <= wants[1]])[1])
    right <- ifelse(wants[2] == 0, 0,     needs[needs >= wants[2]][1])
    ylim <- 10^(as.integer(log10(ylim)) + c(left,right))
    ylim <- 10^(log10(ylim)-6); #print(ylim)
  }
  xx <- xlim; xx <- c(xx, xx[2:1], xx[1])
  yy <- ylim[1]; yy <- c(rep(yy,2), rep(lot,2), yy[1])
  core <- seq(xlim[1], xlim[2], by=10)
  core <- core[core <= xx[2] & core >= xx[1]]

  opar <- par(no.readonly=TRUE)
  par(mgp=c(3,0.5,0), las=0)

  plot(x$water_yr[x$peak_va > 0], x$peak_va[x$peak_va > 0],
       type="n", log="y", xlim=xlim, ylim=ylim, xlab=xlab, ylab="",
       xaxs="i", yaxs="i", axes=FALSE, tcl=0.5, ...)
  if(showDubNA) {
    usry <- par()$usr[3:4]
    for(wy in nwis_double_nullyrs) {
      polygon(   c(   -0.5,    +0.5,    +0.5,    -0.5,    -0.5)+wy,
              10^c(usry[1], usry[1], usry[2], usry[2], usry[1]),
              col="#EDF8B1", lty=0)
    }
  }
  polygon(xx,yy, col=grey(0.95), border=NA)
  lines(xx[1:2], rep(lot,2), lwd=0.5, lty=2)
  lmomco::add.log.axis(side=2,    tcl=+0.8*abs(par()$tcl), two.sided=TRUE)
  lmomco::add.log.axis(logs=c(1), tcl=+1.3*abs(par()$tcl), two.sided=TRUE, side=2)
  axis(1, c(xx[1],core,xx[2]), at=c(xx[1],core,xx[2]), labels=c("",core,""), tcl=0.5)
  htix <- axTicks(1)
  axis(3, c(xx[1],core,xx[2]), labels=FALSE, tcl=0.5)
  axis(1, xlim[1], tcl=0, lwd=0)
  axis(1, xlim[2], tcl=0, lwd=0)
  lmomco::add.log.axis(logs=log.ticks, side=2, make.labs=TRUE, las=1, label="")
  axis(2, ylim, labels=FALSE, tcl=0) # very late additions, did lmomco change?
  axis(4, ylim, labels=FALSE, tcl=0) # to require these to be manually added (WHA 11/27/2018)
  mtext(ylab, side=2, line=3)
  gap.lwd <- 0.76
  for(wy in gap) {
     lines(rep(wy,2), c(ylim[1],ylim[2]), col="#FFAA00", lwd=0.76, lty=5)
  }
  with(x[x$peak_va == 0,], rug(x$water_yr[x$peak_va == 0],
                               side=1, tcl=-0.5, col="#006F41", lwd=2.1))
  if(showGHyrs) {
    # A with() operation does not work here if the na_peak_gage_avail is empty it seems
    if(length(na_peak_gage_avail) >0) rug(na_peak_gage_avail, side=1,
                                          tcl=-0.4, col="#42B3F4", lwd=2.1)
  }
  with(x[x$appearsSystematic & x$isCode7,],
           points(water_yr, peak_va, pch= 1, col=grey(0.1), bg=NULL, cex=0.7, lwd=0.8))
  with(x[! x$anyCodes,],
           points(water_yr, peak_va, pch=21, col=grey(0.1), bg=grey(0.8), cex=0.7, lwd=0.8))
  with(x[x$isCode1,], # circle with + in it, maximum daily average
           points(water_yr, peak_va, pch=10, col=grey(0.1), cex=1.1, lwd=0.8))
  with(x[x$isCode9,], # square with x in it affected by snowmelt, hurricane, ice-jam, or debris-dam breakup
           points(water_yr, peak_va, pch=7,  col=grey(0.1), cex=1.9, lwd=0.8))
  with(x[(x$isCode2 | x$isCodeA | x$isCodeB  | x$isCodeD | x$isCodeE) & # no distinction of an estimated flow
       ! x$isCode1 & ! x$isCode3 & ! x$isCode4 & ! x$isCode5 & # is made or if a date is ambiguous
       ! x$isCode6 & ! x$isCode7 & ! x$isCode8 & ! x$isCode9 & ! x$isCodeC, ],
           points(water_yr, peak_va, pch=21, col=grey(0.1), bg=grey(0.8), cex=0.7, lwd=0.8))
  for(wy in x$water_yr[x$isCode4 == TRUE]) {
    lines(rep(wy,2), c(ylim[1],x$peak_va[x$water_yr == wy]), col="#FF3C00", lwd=0.81)
  }
  for(wy in x$water_yr[x$isCode8 == TRUE]) {
    lines(rep(wy,2), c(ylim[2],x$peak_va[x$water_yr == wy]), col="#FF3C00", lwd=0.81)
  }
  if(codes) {
    with(x[x$isCodeC == TRUE,], points(water_yr, peak_va, pch="C", cex=1.3, col=4))
    with(x[x$isCodeO == TRUE,], points(water_yr, peak_va, pch="O", cex=1.9, col="#FF8C00"))
    with(x[x$isCode3,], # and big letter D for dam
           points(water_yr, peak_va, pch="D", col="#377EB8", cex=2.5, lwd=0.8))
    if(show48) {
      with(x[x$isCode4 == TRUE,], points(water_yr, peak_va, pch="4", cex=0.8, col=2))
      with(x[x$isCode8 == TRUE,], points(water_yr, peak_va, pch="8", cex=0.8, col=2))
    } else {
      with(x[x$isCode4 == TRUE,], points(water_yr, peak_va, pch=16,  cex=0.7, lwd=0.8, col="#FF3C00"))
      with(x[x$isCode8 == TRUE,], points(water_yr, peak_va, pch=16,  cex=0.7, lwd=0.8, col="#FF3C00"))
    }
    with(x[x$isCode5 == TRUE,], points(water_yr, peak_va, pch="5", cex=0.8, col="#4B284B"))
    with(x[x$isCode6 == TRUE,], points(water_yr, peak_va, pch="6", cex=0.8, col="#7D287D"))
    with(x[x$isCode7 == TRUE,], points(water_yr, peak_va, pch="7", cex=1.0, col=6))
  } else {
    with(x[x$isCodeC == TRUE,], points(water_yr, peak_va, pch=3,   cex=1.1, lwd=0.8, col=4))
    with(x[x$isCode4 == TRUE,], points(water_yr, peak_va, pch=16,  cex=0.7, lwd=0.8, col="#FF3C00"))
    with(x[x$isCode5 == TRUE,], points(water_yr, peak_va, pch=2,   cex=0.7, lwd=0.8, col="#4B284B"))
    with(x[x$isCode6 == TRUE,], points(water_yr, peak_va, pch=1,   cex=0.7, lwd=0.8, col="#7D287D"))
    with(x[x$isCode7 == TRUE,], points(water_yr, peak_va, pch=6,   cex=0.7, lwd=0.8, col=6))
    with(x[x$isCode8 == TRUE,], points(water_yr, peak_va, pch=16,  cex=0.7, lwd=0.8, col="#FF3C00"))
  }
  mtext(site)
  on.exit(par(opar))
}


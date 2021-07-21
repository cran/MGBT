readNWISwatstore <-
function(siteNumbers, path=".", dirend="d",
                      tabpk=TRUE, vispk=TRUE, vispdf=TRUE,
                      unlinkpath=FALSE,  citeNWISdoi=TRUE, ...) {
  #https://nwis.waterdata.usgs.gov/nwis/peak?site_no=09416000
  #                                          &agency_cd=USGS&format=hn2

  headit <- "https://nwis.waterdata.usgs.gov/nwis/peak?site_no="
  tailit <- "&agency_cd=USGS&format=hn2"

  for(site in siteNumbers) {

    fullpath <- paste0(path,"/",site,dirend)
    if(unlinkpath) unlink(fullpath, recursive=TRUE)
    if(! dir.exists(fullpath)) dir.create(fullpath)

    FH <- file(paste0(headit,site,tailit))
    pk <- readLines(FH); close(FH)

    cat(pk, file=paste0(fullpath,"/",site,".pkf"), sep="\n")

    if(vispk | tabpk) {
      pkall <- dataRetrieval::readNWISpeak(site, convert=FALSE)
      pkall <- splitPeakCodes(pkall)
      sname <- dataRetrieval::readNWISsite(site)
      sname <- toupper(sname$station_nm[sname$agency_cd == "USGS"])
    }

    if(vispk) {
        file <- paste0(fullpath,"/",site,".pdf")
      if(vispdf) pdf(file, useDingbats=FALSE)
        plotPeaks(pkall, codes=TRUE, showDubNA=TRUE,
                  xlab="Water year", ylab="Peak streamflow, in cubic feet per second",
                  ...)
        mtext(paste0(site," ",sname), line=0.5)
      if(vispdf) dev.off()
    }
    if(tabpk) {
      file <- paste0(fullpath,"/",site,".txt")
      write.table(pkall, file=file, sep="\t", row.names=FALSE, quote=FALSE)
    }

    if(citeNWISdoi) {

      tx <- date() # going to workout the ability to make a contemporaneous citation to NWIS!
      yr <- strsplit(tx, "\\s+")[[1]][5]
      mn <- strsplit(tx, "\\s+")[[1]][2]
      dd <- strsplit(tx, "\\s+")[[1]][3]
      tx <- paste0(mn," ",dd,", ",yr)

      txt <-
       paste0("# CITATION.md---U.S. Geological Survey Peak Streamflow Data\n",
              "\n",
              "***\n",
              "***\n",
              "\n",
              "The peak streamflow data contained in this directory were acquired from\n",
              "U.S. Geological Survey (USGS) (",yr,") using a combination of the **dataRetrieval**\n",
              "and **MGBT** _R_ packages. These packages are referenced below and deliberately\n",
              "left undated because basic **dataRetrieval** operations for reading peaks are used\n",
              "and technically, no features in **MGBT** itself were used for data acquisition,\n",
              "just plotting. However, if a user's system was reasonably up to date, then year\n",
              "and version numbers of the respective packages can be inferred by the access date\n",
              "emphasized here for the USGS National Water Information System database retrieval.\n",
              "\n",
              "# REFERENCES\n\n",
              "Asquith, W.H., England, J.F., and Herrmann, G.R., MGBT---Multiple Grubbs--Beck\n",
              "   low-outlier test: https://cran.r-project.org/package=MGBT.\n\n",
              "DeCicco, L., Hirsch, R., Lorenz, D., Read, J., Walker, J., Carr, L., and Watkins, D.,\n",
              "   dataRetrieval---Retrieval functions for USGS and EPA hydrologic and water quality data:\n",
              "   https://cran.r-project.org/package=dataRetrieval.\n\n",
              "U.S. Geological Survey, ",yr,", USGS water data for the Nation:\n",
              "   U.S. Geological Survey National Water Information System database,\n",
              "   accessed ",tx,", at https://doi.org/10.5066/F7P55KJN.\n",
              "   [ peak streamflows in WATSTORE format by direct URL at\n",
              "     ", headit, site, tailit, " ]")
      cat(txt, file=paste0(fullpath,"/CITATION.md"), sep="\n")
    }
  }
}

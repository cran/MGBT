"peakTMtoHRS" <- function(x, type=c("asis", "musd"), parseHHMM=TRUE) {
  type <- match.arg(type)

  if(is.data.frame(x)) {
    if(exists("peak_tm", x)) tm <- x$peak_tm
  } else{
    tm <- x
  }
  # tm <- paste0(floor(tm), ":", as.integer(60*(tm - floor(tm))))
  if(parseHHMM) {
    tm <- strsplit(tm, ":")
    fd <- as.numeric(sapply(seq_len(length(tm)), function(i) tm[[i]][1])) +
          as.numeric(sapply(seq_len(length(tm)), function(i) tm[[i]][2])) / 60
  } else {
    fd <- tm
  }
  if(type == "asis") return(fd)

  txt <- c("circular.mean", "circular.sd")
  na <- length( fd[is.na(fd)] )
  fd <- fd[! is.na(fd)]
  if(length(fd) == 0) {
    zz <- c(NA, NA, 0, na)
    names(zz) <- c("circular.mean", "circular.sd", "num_peak_tm", "num_NAs")
    return( zz )
  }

  "circ_musd" <- function(hours) {
    # convert hours to radians, to convert from hours to radians
    rads <- (hours / 24 ) * (2 * pi)

    # Refer also to circular:::RhoCircularRad
    # Refer also to circular::sd.circular
    sd <- sqrt( -2*log( sqrt(sum(sin(rads))^2 + sum(cos(rads))^2) / length(rads) ) )

    # Refer also to circular::mean.circular
    sin_mu <- mean( sin(rads) ); cos_mu <- mean( cos(rads) )
    rad_mu <- atan2(sin_mu, cos_mu)
    # print(c(sin_mu, cos_mu, rad_mu))
    if(sin_mu < 0 & cos_mu > 0) {
      rad_mu <- rad_mu + 2*pi
    } else if(sin_mu < 0 & cos_mu <= 0) {
      rad_mu <- rad_mu + 2*pi
    }
    mu <- rad_mu / (2*pi) * 24
    zz <- c(mu, sd)
    names(zz) <- c("circular.mean", "circular.sd")
    return( zz )
  }

  if(type == "musd") {
    zz <- circ_musd(fd)
    zz[3:4] <- c(length(fd), na)
    names(zz) <- c("circular.mean", "circular.sd", "num_peak_tm", "num_NAs")
    return( zz )
  }
}

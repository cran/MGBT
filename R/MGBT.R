"MGBT" <- function(...) MGBT17c(...)

#COHN:P3_089 ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN:P3_089
#COHN:P3_089   Multiple Grubbs-Beck Test (MGBT)
#COHN:P3_089     Identifies up to n/2 low outliers
#COHN:P3_089     Modified 28 Jun 2016 to reflect MGBT [Stedinger, Cohn, etc.]
#COHN:P3_089
#COHN:P3_089 ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN:P3_089
#COHN:P3_089MGBT <- function(Q,Alphaout=0.005,Alphazeroin=0.10,n2=floor(length(Q)/2)){
#COHN:P3_089      zt      <- sort(log10(pmax(1e-8,Q)))
#COHN:P3_089      n       <- length(zt)
#COHN:P3_089      pvalueW <-rep(-99,n2);w<-rep(-99,n2)
#COHN:P3_089      j1=0;j2=0
#COHN:P3_089    for(i in 1:n2) {
#COHN:P3_089       w[i]<-(zt[i]-mean(zt[(i+1):n]))/sqrt(var(zt[(i+1):n]))
#COHN:P3_089       pvalueW[i]<-KthOrderPValueOrthoT(n,i,w[i])$value
#COHN:P3_089       if(pvalueW[i]<Alphaout){j1<-i;j2<-i}
#COHN:P3_089       if( (pvalueW[i]<Alphazeroin) & (j2==i-1)){j2<-i}
#COHN:P3_089       }
#COHN:P3_089    return(list(klow=j2,pvalues=pvalueW,LOThresh=ifelse(j2>0,sort(Q)[j2+1],0)))
#COHN:P3_089}

"MGBTcohn2016" <- # arguments match MGBTcohn2013 but the alphas were renamed
function(x, alphaout=0.005, alphazeroin=0.10, n2=floor(length(x)/2),
            napv.zero=TRUE, offset=0) {
   if(is.data.frame(x)) {
      message("x tests as data.frame, helped out with 'x <- as.numeric(x$peak_va)'")
      x <- as.numeric(x$peak_va)
   }
   if(is.character(x)) {
      message("x tests as character, helped out with 'x <- as.numeric(x)'")
      x <- as.numeric(x)
   } else if(! is.numeric(x)) {
      warning("unable to proceed as x is not a numeric vector")
      return(NULL)
   }
   if(any(is.na(x))) {
      message("NAs detected in x, these were removed") # to suppress.
   }
   x <- x[! is.na(x)]
   # WHA notes that by TAC using 1e-8 as "small", he has assumed that the MGBT
   # will always identify these as low outliers. Safe assumption in flood peaks,
   # but it is a weakness. WHA would have kept them as -Inf (we are in R and not
   # FORTRAN) and dealt with -Inf hitting subcomponents of the MBGT and trapping
   # the issue there.

   # WHA error trapping
   if(length(unique(x)) == 1) return(list(pvalues=NA, klow=0, LOThresh=0))

   zt <- sort( log10( pmax(1e-8,x)) )                                # matches MGBTcohn2013
   n  <- length(zt)  # WHA: n <- length(x) would be bit cleaner # matches MGBTcohn2013
   pvalueW <- w <- rep(NA, n2)                                       # matches MGBTcohn2013
   j1 <- j2 <- 0                                                     # matches MGBTcohn2013
   for(i in 1:n2) {                                                  # matches MGBTcohn2013
      w[i] <-   (zt[i] - mean(zt[(i+1):n])) / sqrt(var(zt[(i+1):n])) # matches MGBTcohn2013
            pvalueW[i] <- RthOrderPValueOrthoT(n, i, w[i])$value     # matches MGBTcohn2013
      if(   is.na(pvalueW[i])) {                         # WHA error trapping
                  pvalueW[i] <- ifelse(napv.zero, 0, NA) # WHA error trapping
         if(is.na(pvalueW[i])) next                      # WHA error trapping
      }                                                  # WHA error trapping
      # Next line as long as the p-value is less than alpha_out, we continue to
      # index up because of the direction of the for() loop. This indexing up
      # has the same effect as "sweeping out" from the median (well if
      # n2=length(x)/2) as discussed in 17C so j1 will always attain the
      # kth-largest low values having a p-value smaller than alpha_out. We can
      # conclude from this that 17C is being followed in this regard and that
      # alphaout is the same as LaTeX \alpha_out in 17C. Notice that alphaout
      # will skip over low values whose p-value might not be small enough and
      # declare them as outliers. This then seems to be a greedy approach. We
      # must conclude that j1 then is the index associated with the outward
      # sweep through in implementation, we are moving up the order statistics.
      # 17C says that the outward sweep "STOPS" but in implementation this is
      # not what happens. TAC is dealing with both alphas in one loop!
      if((pvalueW[i] < alphaout)) { j1 <- i; j2 <- i } # matches MGBTcohn2013
      # TAC seems to break from 17C in the notation that follows. First, this
      # alpha must be LaTeX \alpha_in in 17C. Because j2 gets set to i if the
      # condition is jointly true, then it can be seen that it is more visually
      # sweeping in towards the median. The (j2==(i-1)) is harder to "see" but
      # carefully considering its purpose, it ensures that one has a solid
      # continuity of low outliers from i=1 on up to j2. Hence, it is less
      # greedy than j1 by requiring this. j2 must be associated with sweep in
      # as j1 is dealing with sweep out. 17C says that the inward sweep "STOPS"
      # but in implementation this is not what happens. TAC is dealing with both
      # alphas in one loop!
      if((pvalueW[i] < alphazeroin) & (j2==i-1)) j2 <- i  # matches MGBTcohn2013
   }

   # TAC does not include a recursion back to MGBTcohn2016() for the
   # condition j2 == n2 (see MGBTcohn2013) at this point in the code.

   # ---***--------------***--- TAC CRITICAL BUG ---***--------------***---
   # j2 <- min(c(j1,j2)) # WHA tentative completion of the 17C alogrithm!?!
   # HOWEVER MAJOR WARNING. WHA is using a minimum and not a maximum!!!!!!!
   # See MGBT17C() below. In that if the line a few lines above that reads
   # if((pvalueW[i] < alpha1 )) { j1 <- i; j2 <- i }
   # is replaced with if((pvalueW[i] < alpha1 )) j1 <- i
   # then maximum and not the minimum becomes applicable.
   # ---***--------------***--- TAC CRITICAL BUG ---***--------------***---

   zz <- list(pvalues=pvalueW, klow=j2,                        # matches MGBTcohn2013
              LOThresh=ifelse(j2 > 0, sort(x)[j2+1]+offset, 0))# quasi matches MGBTcohn2013
   # WHA: What bothers me is that j1 does not seem to get involved in the final
   # result. Just by quick inspection, this does not seem symmetrical. 17C says
   # "the number of low outliers identified by [MGBT] is the larger of k and
   # m-1"  The "m" appears to be j2 and j1 appears to be the k. TAC does not
   # compute the maximum (larger there of) and with this conclusion is with
   # evidence of several code bundles available verbatim---
   # IS IT POSSIBLE THAT TAC DID NOT COMPLETE THE 17C ALGORITHM IN HIS MGBT-R
   # "port"? Looking closely, without yet proof, m-1 does not translate to j2-1
   # given the structure of TACs algorithm.
   zz$LOThresh[zz$LOThresh < 0] <- 0 # WHA 02/04/2019
   return(zz)   # the offset is by WHA                               # matches MGBTcohn2013
}

#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN   Multiple Grubbs-Beck Test (MGBT)
#COHN     Identifies up to n/2 low outliers
#COHN
#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN MGBT <- function(Q,alpha1=0.005,alpha10=0.10,n2=floor(length(Q)/2)){
#COHN       zt      <- sort(log10(pmax(1e-8,Q)))
#COHN       n       <- length(zt)
#COHN       pvalueW <-rep(-99,n2);w<-rep(-99,n2)
#COHN       j1=0;j2=0
#COHN     for(i in 1:n2) {
#COHN        w[i]<-(zt[i]-mean(zt[(i+1):n]))/sqrt(var(zt[(i+1):n]))
#COHN        pvalueW[i]<-KthOrderPValueOrthoT(n,i,w[i])$value
#COHN        if(pvalueW[i]<alpha1){j1<-i;j2<-i}
#COHN        if( (pvalueW[i]<alpha10) & (j2==i-1)){j2<-i}
#COHN        }
#COHN     if(j2==n2){
#COHN       if(n2<length(Q)-5) {   # set a limit of at least 5 retained observations
#COHN         print(paste(" Number of low outliers equals or exceeds limit of ",n2));
#COHN         return(MGBT(Q,alpha1=0.01,alpha10=0.10,n2=length(Q)-5))} # try this
#COHN       else
#COHN         print("MGBT identifies too many low outliers; use caution and judgment")
#COHN     }
#COHN      return(list(klow=j2,pvalues=pvalueW,LOThresh=ifelse(j2>0,sort(Q)[j2+1],0)))
#COHN }

"MGBTcohn2013" <-
function(x, alphaout=0.005, alphazeroin=0.10, n2=floor(length(x)/2),
            napv.zero=TRUE, offset=0) {
   if(is.data.frame(x)) {
      message("x tests as data.frame, helped out with 'x <- as.numeric(x$peak_va)'")
      x <- as.numeric(x$peak_va)
   }
   if(is.character(x)) {
      message("x tests as character, helped out with 'x <- as.numeric(x)'")
      x <- as.numeric(x)
   } else if(! is.numeric(x)) {
      warning("unable to proceed as x is not a numeric vector")
      return(NULL)
   }
   if(any(is.na(x))) {
      message("NAs detected in x, these were removed") # to suppress.
   }
   x <- x[! is.na(x)]
   if(length(unique(x)) == 1) {                           # WHA error trapping
      return(list(pvalues=NA, klow=0, LOThresh=0))        # WHA error trapping
   }                                                      # WHA error trapping
   zt <- sort( log10( pmax(1e-8,x)) )
   n  <- length(zt) # WHA: n <- length(x) could be used instead and cleaner
   pvalueW <- w <- rep(NA, n2)
   j1 <- j2 <- 0
   for(i in 1:n2) {
      w[i] <-  (zt[i] - mean(zt[(i+1):n])) / sqrt(var(zt[(i+1):n]))
           pvalueW[i] <- RthOrderPValueOrthoT(n, i, w[i])$value
      if(   is.na(pvalueW[i])) {                          # WHA error trapping
                  pvalueW[i] <- ifelse(napv.zero, 0, NA)  # WHA error trapping
         if(is.na(pvalueW[i])) next                       # WHA error trapping
      }                                                   # WHA error trapping
      if((pvalueW[i] < alphaout ))   { j1 <- i; j2 <- i }
      if((pvalueW[i] < alphazeroin) & (j2==i-1) ) j2 <- i
   }

   # ---***--------------***--- TAC CRITICAL BUG ---***--------------***---
   # j2 <- min(c(j1,j2)) # WHA tentative completion of the 17C alogrithm!?!
   # HOWEVER MAJOR WARNING. WHA is using a minimum and not a maximum!!!!!!!
   # See MGBT17C() below. In that if the line a few lines above that reads
   # if((pvalueW[i] < alpha1 )) { j1 <- i; j2 <- i }
   # is replaced with if((pvalueW[i] < alpha1 )) j1 <- i
   # then maximum and not the minimum becomes applicable.
   # ---***--------------***--- TAC CRITICAL BUG ---***--------------***---

   # Curious that TAC does two length(x) below, but the length is already known.
   if(j2 == n2) { # TODO, does TAC change his mind on 5 retained observations?
                  # Also alphaout changes.
      if( n2 < n-5 ) { # set limit of at least 5 retained below 1/2 the sample
         message( paste0(" Number of low outliers equals or exceeds limit of ",n2) )
         zz <- MGBTcohn2013(x, alphaout=0.01, alphazeroin=0.10, n2=n-5,
                               napv.zero=napv.zero) # recursion
         return(zz)
      } else {
         message("MGBT identifies too many low outliers, use caution and judgment")
      }
   }
   zz <- list(pvalues=pvalueW, klow=j2,
              LOThresh=ifelse(j2 > 0, sort(x)[j2+1]+offset, 0))# quasi matches MGBTcohn2016
   zz$LOThresh[zz$LOThresh < 0] <- 0 # WHA 02/04/2019
   return(zz)  # the offset is by WHA
}

#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN   Multiple Grubbs Beck test (Cohn et al., 2011)
#COHN      p. 169
#COHN MGB <- function(x,k,n=length(x)){
#COHN            y=sort(x);(y[k]-mean(y[(k+1):n]))/sqrt(var(y[(k+1):n]))}

"MGBTcohn2011" <- function(x, r=NULL, n=length(x)) {
   if(is.null(r)) {
      warning("the 'r'th-order truncation statistic requires definition")
      return(NULL)
   }
   # It is deliberate to not go through the data.frame/as.numeric etc here as
   # done for the other MGBT*() functions herein.
   x <- x[! is.na(x)]
   y <- sort(x)
   ( y[r] - mean(y[(r+1):n]) ) / sqrt( var(y[(r+1):n]) )
}

#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN   Multiple Grubbs-Beck Test (MGBTnb)
#COHN     This eliminates the backup procedure
#COHN     Added for JL/JRS study 24 Aug 2012 (TAC)
#COHN
#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN MGBTnb <- function(Q,alpha1=0.005,alpha10=0.10,n2=floor(length(Q)/2)){
#COHN       zt      <- sort(log10(pmax(1e-8,Q)))
#COHN       n       <- length(zt)
#COHN       pvalueW <-rep(-99,n2);w<-rep(-99,n2)
#COHN       j1=0;j2=0
#COHN     for(i in 1:n2) {
#COHN        w[i]<-(zt[i]-mean(zt[(i+1):n]))/sqrt(var(zt[(i+1):n]))
#COHN        pvalueW[i]<-KthOrderPValueOrthoT(n,i,w[i])$value
#COHN        if(pvalueW[i]<alpha1){j1<-i;j2<-i}
#COHN     }
#COHN     if( (pvalueW[1]<alpha10) & (j2==0)){j2<-1}
#COHN     if(j2==n2){
#COHN       if(n2<length(Q)-5) {   # set a limit of at least 5 retained observations
#COHN         print(paste(" Number of low outliers equals or exceeds limit of ",n2));
#COHN         return(MGBT(Q,alpha1=0.01,alpha10=0.10,n2=length(Q)-5))} # try this
#COHN       else
#COHN         print("MGBT identifies too many low outliers; use caution and judgment")
#COHN     }
#COHN     return(list(klow=j2,pvalues=pvalueW,LOThresh=ifelse(j2>0,sort(Q)[j2+1],0)))
#COHN }

# In MGBTcohn2013, TAC uses recursion back to that same function. The MGBTnb()
# recurses to MGBTcohn2013 and not back to itself, which appears the only real
# difference between the two.
"MGBTnb" <-
function(x, alphaout=0.005, alphazeroin=0.10, n2=floor(length(x)/2),
            napv.zero=TRUE, offset=0) {
   if(is.data.frame(x)) {
      message("x tests as data.frame, helped out with 'x <- as.numeric(x$peak_va)'")
      x <- as.numeric(x$peak_va)
   }
   if(is.character(x)) {
      message("x tests as character, helped out with 'x <- as.numeric(x)'")
      x <- as.numeric(x)
   } else if(! is.numeric(x)) {
      warning("unable to proceed as x is not a numeric vector")
      return(NULL)
   }
   if(any(is.na(x))) {
      message("NAs detected in x, these were removed") # to suppress.
   }
   x <- x[! is.na(x)]
   # WHA error trapping
   if(length(unique(x)) == 1) return(list(pvalues=NA, klow=0, LOThresh=0))
   zt <- sort( log10(pmax(1e-8,x)) )
   n  <- length(zt)
   pvalueW <- w <- rep(NA, n2) # rep(-99,n2)
   j1 <- j2 <- 0
   for(i in 1:n2) {
      w[i] <- (zt[i]-mean(zt[(i+1):n]))/sqrt(var(zt[(i+1):n]))
         pvalueW[i] <- RthOrderPValueOrthoT(n,i,w[i])$value
      if(   is.na(pvalueW[i])) {                          # WHA error trapping
                  pvalueW[i] <- ifelse(napv.zero, 0, NA)  # WHA error trapping
         if(is.na(pvalueW[i])) next                       # WHA error trapping
      }                                                   # WHA error trapping
      if(pvalueW[i] < alphaout) {  j1 <- i; j2 <- i }
   }
   if(   pvalueW[1] < alphazeroin & j2 == 0 ) j2 <- 1 # Notice this is outside the loop and
   # also is slightly different logic than seen in MGBTcohn2013 and MGBTcohn2016.
   # It is curious that TAC does two length(x) in logic below, but that length is already known.
   if(j2 == n2) {
      if(n2 < n-5) {   # set a limit of at least 5 retained observations
         message(" Number of low outliers equals or exceeds limit of ",n2)
         return(MGBTcohn2013(x, alphaout=0.01, alphazeroin=0.10, n2=n-5),
                                napv.zero=napv.zero) # recursion
      } else {
         message("MGBT identifies too many low outliers; use caution and judgment")
      }
   }
   zz <- list(pvalues=pvalueW, klow=j2,
              LOThresh=ifelse(j2 > 0, sort(x)[j2+1]+offset, 0))# matches MGBTcohn2013
   zz$LOThresh[zz$LOThresh < 0] <- 0 # WHA 02/04/2019
   return(zz)  # the offset is by WHA
}

# This is a slower function in general practice between some omegas (w) get
# doubly computed. This function was developed by WHA explicitly following
# verbatim the language inside the B17C publication:
# https://pubs.er.usgs.gov/publication/tm4B5
# This function serves as a backstop against TAC having a systemtic and
# undetected fault in his R idiom implementation of the MGBT in the various
# incarations credited to him within this source code file.
"MGBT17c.verb" <- # arguments match MGBT17C
function(x, alphaout=0.005, alphain=0, alphazeroin=0.10, n2=floor(length(x)/2),
            napv.zero=TRUE, offset=0, min.obs=0) {
   if(is.data.frame(x)) {
      message("x tests as data.frame, helped out with 'x <- as.numeric(x$peak_va)'")
      x <- as.numeric(x$peak_va)
   }
   if(is.character(x)) {
      message("x tests as character, helped out with 'x <- as.numeric(x)'")
      x <- as.numeric(x)
   } else if(! is.numeric(x)) {
      warning("unable to proceed as x is not a numeric vector")
      return(NULL)
   }
   if(any(is.na(x))) {
      message("NAs detected in x, these were removed") # to suppress.
   }
   x <- x[! is.na(x)]
   n <- length(x); idx <- c(n,n2,rep(NA, 3))
   txt <- c("n", "n2", "ix_alphaout", "ix_alphain", "ix_alphazeroin")
   names(idx) <- txt
   if(length(unique(x)) == 1) return(list(index=idx, klow=0, LOThresh=0))
   if(n < min.obs)            return(list(index=idx, klow=0, LOThresh=0))
   if(n2 > n) {
      warning("n2 > n, which does not make not logical sense")
      return(NULL)
   }
   small <- sqrt(.Machine$double.eps); j1 <- j2 <- 0
   x <- sort(x); zt <- log10( pmax(small, x) )

   for(i in seq(n2,1,by=-1)) { # NOTE LITERALLY SWEEP OUT as per 17C
      w  <- (zt[i]-mean(zt[(i+1):n])) / sqrt(var(zt[(i+1):n]))
      pv <- RthOrderPValueOrthoT(n, i, w)$value
      if(napv.zero & is.na(pv)) pv <- 0
      if(is.na(pv)) next
      if( pv >= alphaout) { j1 <- i; next } # effect of sweep out, NOTE GREATER THAN EQUAL
      break
   }

   j1 <- j1 - 1 # this is critical!
   for(i in seq(1,n2,by=+1)) { # NOTE LITERALLY SWEEP IN as per 17C
      w  <- (zt[i]-mean(zt[(i+1):n])) / sqrt(var(zt[(i+1):n]))
      pv <- RthOrderPValueOrthoT(n, i, w)$value
      if(napv.zero & is.na(pv)) pv <- 0 # NA handling
      if(is.na(pv)) next # protection from a NA leaking into these
      if((pv <= alphazeroin ) & (j2==i-1)) { j2 <- i; next } # effect of sweep in,
      # NOTE LESS THAN EQUAL and NOT LESS THAN
      break
   }
   k <- max(j1,j2) # as per B17C
   if(k >= n) return(list(index=idx, klow=0, LOThresh=0))
   idx <- c(n,n2,j1,NA,j2); names(idx) <- txt
   zz <- list(index=idx, klow=k,
              LOThresh=ifelse(k > 0, x[k+1]+offset, 0))
   zz$LOThresh[zz$LOThresh < 0] <- 0 # WHA 02/04/2019
   return(zz)
}


# This code base follows more along lines of WHA idioms implementing B17C in
# the spirit of TAC's coding and faster than the verbatim implementation mentioned
# above. This function is to be the user-level version of the MGBT.
"MGBT17c" <- # arguments match MGBTcohn2013 but the alphas were renamed to match 17C
function(x, alphaout=0.005, alphain=0, alphazeroin=0.10, n2=floor(length(x)/2),
            napv.zero=TRUE, offset=0, min.obs=0) {
   if(is.data.frame(x)) {
      message("x tests as data.frame, helped out with 'x <- as.numeric(x$peak_va)'")
      x <- as.numeric(x$peak_va)
   }
   if(is.character(x)) {
      message("x tests as character, helped out with 'x <- as.numeric(x)'")
      x <- as.numeric(x)
   } else if(! is.numeric(x)) {
      warning("unable to proceed as x is not a numeric vector")
      return(NULL)
   }
   if(any(is.na(x))) {
      message("NAs detected in x, these were removed") # to suppress.
   }
   x <- x[! is.na(x)]
   n <- length(x); idx <- c(n,n2,rep(NA, 3))
   txt <- c("n", "n2", "ix_alphaout", "ix_alphain", "ix_alphazeroin");
   if(length(unique(x)) == 1) return(list(index=idx, omegas=NA, x=NA, pvalues=NA,
                               klow=0, LOThresh=0, message="all data are equal"))
   if(n < min.obs) return(list(index=idx, omegas=NA, x=NA, pvalues=NA,
                               klow=0, LOThresh=0, message="too few data"))
   if(n2 > n) {
      warning("n2 > n, which does not make not logical sense")
      return(NULL)
   }
   small <- sqrt(.Machine$double.eps); j1 <- j2 <- 0
   x <- sort(x); zt <- log10( pmax(small, x) )

   # 17C says sweeps out and in and STOP each time, which is the effect we seek, but for
   # simplicity motivated by desire to return p-values for all 1:n2 values  we must
   # expend CPU time computing all omegas and pvalues. This was TAC motivation (at least
   # concluded) for a single loop. Below are R idioms by WHA.
   w  <- sapply(1:n2, function(i) (zt[i]-mean(zt[(i+1):n])) /
                                     sqrt(var(zt[(i+1):n])) )
   pv <- sapply(1:n2, function(i) RthOrderPValueOrthoT(n, i, w[i])$value) # p-values
   sw <- mgbt.sweeper(pv, alphaout=alphaout, alphain=alphain, alphazeroin=alphazeroin)
   k <- sw$index
   # don't know of this is next line is legitimately accessible but trap anyway
   if(k >= n) return(list(index=idx, omegas=NA, x=NA, pvalues=NA, klow=0, LOThresh=0))
   idx <- c(n,n2,sw$sweeps[1],sw$sweeps[2],sw$sweeps[3]); names(idx) <- txt
   zz <- list(index=idx, omegas=w[1:n2], x=x[1:n2], pvalues=pv, klow=k,
              LOThresh=ifelse(k > 0, x[k+1]+offset, 0))
   zz$LOThresh[zz$LOThresh < 0] <- 0 # WHA 02/04/2019
   return(zz)
}


"mgbt.sweeper" <-
function(pvs, alphaout=0.005, alphain=0, alphazeroin=0.1) {
   j1 <- j2 <- j3 <- 0 # outward, inward from 1+ outward, inward sweep
   n2 <- length(pvs)   # the js are numer of low outliers and not the index of the threshold
   if(any(is.na(pvs))) stop("NA p-value, wrapper code should make decision")
   for(i in seq(n2,1,-1)) if(pvs[i] <    alphaout) { j1 <- i;  break }
   if(j1 < n2) { # Late addition station 08020500 causes j1 to be n2 [A WHA fix to TAC.]
      # with Q=c(19500, 17400, 9350, 3760, 8410, 15200, 5370, 22300, 21500, 6590, 17300)
      for(i in j1+1:n2) if(pvs[i] >=     alphain) { j2 <- i-1; break }
   } else if(j1 == n2) {
      # This means that the last pv is < alphaout
   } else {
      stop("should not be here in logic")
   }
   for(i in         1:n2) if(pvs[i] >= alphazeroin) { j3 <- i-1; break }
   js <- c(j1,j2,j3); names(js) <- c("ix_alphaout", "ix_alphain", "ix_alphazeroin")
   return(list(index=max(js), sweeps=js))
}

# outward sweep number of low outliers
# inward sweep number of low outliers
# other inward sweep number of low outliers

# Peer Review Notes: Chandramauli Awasthi, North Carolina State University (07/23/2019)
# I tested the package for some of the sites using MGBTcohn2016 and MGBTnb
# functions. How are these two functions different, as they are found to be
# giving slightly different results in few cases?

# WHA (07/23/2019): Mauli, note that my comments in MGBT.R indicate that MGBTnb
# has some conditional
#  if(   pvalueW[1] < alphazeroin & j2 == 0 ) j2 <- 1
# that resides outside a loop compared to MGBTcohn2016 (or MGBTcohn2013). The
# MGBTnb() uses sometype of recursion. I do not understand what TAC meant to
# do with MGBTnb() or what type of experiment is was pursuing. Please note that
# MGBTcohn2016/2013 and MGBTnb() are retained for historical reference and are
# not intended for deployment. As to differences that you have seen in the
# performance between the two, this is related to subtle differences at the time
# in TAC's thoughts. The differences are restricted to whether the
# X_k or X_(k+1) order statistics are chosen as the result of the MGBT.
# Finally, by design the "official" implementation of this package is
# "MGBT" <- function(...) MGBT17c(...) # which is the first line of this file.

#COHN****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN  Alternative test: N3 from Barnett & Lewis (adjusted to look for Low Outliers
#COHN     p. 169
#COHN BL_N3 <- function(x,k,n=length(x)){
#COHN           y=sort(x);(sum(y[1:k])-k*mean(y))/sqrt(var(y))}

#Barnett, Vic, and Lewis, Toby, 1994, Outliers in statistical data, 3rd ed.: New York, John Wiley,
#ISBN 978-0471930945 ?

"BLlo" <- function(x, r, n=length(x)) {
   y <- sort(x)
   ( sum(y[1:r]) - r * mean(y) ) / sqrt( var(y) )
}

#"BL_N3" <- function(...) BLlo(...)

# BL_N3 in COHN_MGBT_LowOutliers(R).txt is only used by testMGBvsN3()

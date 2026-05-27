#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN   Moments of observations above the threshold, xsi
#COHN
#COHN gtmoms <- function(xsi,k){
#COHN    H  <- function(xsi) dnorm(xsi)/(1-pnorm(xsi))
#COHN    p  <- pnorm(xsi);
#COHN    if(k == 0) return(1);
#COHN    if(k == 1) return(H(xsi));
#COHN    if(k > 1) return((k-1)*gtmoms(xsi,k-2)+H(xsi)*xsi^(k-1))
#COHN }

"gtmoms" <- function(xsi, r) {
   H  <- function(xsi) dnorm(xsi) / (1-pnorm(xsi))
   if(r == 0) return(1)
   if(r == 1) return(H(xsi))
   if(r >  1) return((r-1) * gtmoms(xsi,r-2) + H(xsi)*xsi^(r-1))
}

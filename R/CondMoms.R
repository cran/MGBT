#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN   Conditional moments
#COHN     N.B. Moments employ only obs. above xsi
#COHN
#COHN "CondMomsZ" <- function(n,r,xsi){
#COHN   cbind( gtmoms(xsi,1) , (gtmoms(xsi,2)-gtmoms(xsi,1)^2)/(n-r) )
#COHN }
#COHN "CondMomsChi2" <- function(n,r,xsi){
#COHN   mu <- gtmoms(xsi,1);
#COHN   cbind(gtmoms(xsi,2) - mu^2 ,
#COHN   V(n,r,pnorm(xsi))[2,2])
#COHN }

"CondMomsZ"    <- function(n, r, xsi) {
   cbind( gtmoms(xsi,1), ( gtmoms(xsi,2) - gtmoms(xsi,1)^2) / (n-r) )
}

"CondMomsChi2" <- function(n, r, xsi) {
   cbind( gtmoms(xsi,2) - gtmoms(xsi,1)^2, V(n,r,pnorm(xsi))[2,2] )
}

# CondMomsZ does not appear used in COHN_MGBT_LowOutliers(R).txt
# CondMomsChi2 is used in COHN_MGBT_LowOutliers(R).txt
# Does N.B. stand for nonbiased? (WHA 05/23/2017)
# Does N.B. stand for NoBackup procedure, which seems some type of note for MGBTnb().
#   (WHA 06/06/2017)

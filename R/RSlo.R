#COHN****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN  Alternative test: RST from Rosner (1975a, TNMX 18(2) pp. 221-227)
#COHN  (adjusted to look only for Low Outliers
#COHN     p. 169
#COHN RST <- function(x,k,n=length(x)){
#COHN           y=sort(x);(y[k]-mean(y[(k+1):(n-k)]))/sqrt(var(y[(k+1):(n-k)]))}

"RSlo" <- function(x, r, n=length(x)) {
   y <- sort(x)
   ( y[r] - mean(y[(r+1):(n-r)]) ) / sqrt( var(y[(r+1):(n-r)]) )
}

# RST in COHN_MGBT_LowOutliers(R).txt is only used by testMGBvsN3()

# rstdat <-
# c(-1.056, -1.008, -0.340, +0.533, +0.109, +0.661, +1.638, -0.413, -0.667, -0.576,
#   +1.207, -0.550, +2.290, +0.504, -2.215, +2.139, -0.048, -0.909, +0.967, -0.143)
# mean(rstdat) # 0.10615 matches Rosner (1975, table 11)
# sd(rstdat)   # 1.14496 matches Rosner (1975, table 11)

#RST <- function(x, r=0) {
#   n <- length(x); x <- sort(x)
#   rsta <- function(k) sum(sapply((k+1):(n-k), function(i) x[i]/(n-2*k)))
#   rstb <- function(k) sum(sapply((k+1):(n-k), function(i) (x[i]-rsta(k))^2/(n-2*k-1)))
#   max(abs(x[r]-rsta(r))/rstb(r))
#}

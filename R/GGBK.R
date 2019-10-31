#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN   New Generalized Grubbs-Beck critical values for 10% test; TAC Approximation
#COHN
#COHN  fw  <- function(n,w){n*w/((n-1)*sqrt((1/(n-2))*(n-1-(n/(n-1))*w^2)))}
#COHN  fw1 <- function(n,w1){-sqrt(((n-1)^3*w1^2)/(n^2*(n-2)+w1^2*n*(n-1)))}
#COHN GGBK  <-function(n) fw1(n,CritK(n,1,.1,-fw(n,critK10(n))))

"GGBK" <- function(n)   {
   fw  <- function(n, w ) {
             n*w / ((n-1) * sqrt((1/(n-2)) * (n-1 - (n/(n-1))*w^2) )) }
   fw1 <- function(n, w1) {
             -sqrt( ((n-1)^3*w1^2) / (n^2*(n-2) + w1^2*n*(n-1)) ) }
   #return( fw1(n, CritK(n, 1, 0.1, -fw(n, critK10(n)) )) )
   stop("GGBK function is incomplete, its CritK() call is the issue, and ",
        "that function requires three arguments, but this code in ",
        "Timothy A. Cohn's sources passes four!")
}

# GGBK is the consumer of fw() and fw1(). These should be defined within it
# to keep the scope local.

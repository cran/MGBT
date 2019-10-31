#COHN:P3_089****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN:P3_089
#COHN:P3_089  Function to compute p-value corresponding to k-th order statistic
#COHN:P3_089     n    sample size
#COHN:P3_089     r    number of truncated observations
#COHN:P3_089     eta  "pseudo-studentized" magnitude of r-th smallest observation
#COHN:P3_089           eta = (X[r]-mean(X[(r+1):n])/sqrt(var(X[(r+1):n]))
#COHN:P3_089
#COHN:P3_089KthOrderPValueOrthoT <- function(n,r,eta){
#COHN:P3_089  integrateV(peta,lower=1e-7,upper=1-1e-7,n=n,r=r,eta=eta)
#COHN:P3_089   }


#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN   Improved "integrate" function that de-vectorizes f(x)
#COHN
# Below is a cleanup of Cohn's coding style. Note that Cohn did not pass
# the actual arguments via their names into the integrate() call. This
# Asquith has done. However, the logic can be shortened to avoid a separate
# function for vectorizing TAC's original peta().
#"integrateV" <-
#function(f, lower, upper, ..., subdivisions=100,
#         rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol,
#         stop.on.error = TRUE, keep.xy = FALSE, aux = NULL) {
#   f2 <- function(x,...){ sapply(x, f, ...) }
#   integrate(f2, lower, upper, ..., subdivisions=100,
#             rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol,
#             stop.on.error = TRUE, keep.xy = FALSE, aux = NULL)
#}
# This is an updated port via WHA, though function is not needed notice how TAC
# did not actually pass the argument for control the integration on down the
# chain to intergrate().
#"integrateV" <-
#function(f, lower, upper, ..., subdivisions=100,
#         rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol,
#         stop.on.error = TRUE, keep.xy = FALSE, aux = NULL) {
#   f2 <- function(x, ...) sapply(x, f, ...)
#   integrate(f2, lower, upper, ..., subdivisions=subdivisions,
#             rel.tol = rel.tol, abs.tol = abs.tol,
#             stop.on.error = stop.on.error, keep.xy = keep.xy, aux = aux)
#}

# integrateV in COHN_MGBT_LowOutliers(R).txt is only used by KthOrderPValueOrthoT()
# It would be safer to define integrateV within that function to keep scope local.

# What Cohn is doing is vectorizing a function that he has defined elsewhere with
# only scalar operation. Perhaps too complicated to put an sapply() inside that other
# function. Asquith understands the reasoning and has many cases of his own where
# vectorization needed to be created. Asquith designs though have historically put
# the vectorization within the said function.


"RthOrderPValueOrthoT" <- function(n, r, eta, n.sim=10000, silent=TRUE) {
   if(length(eta) != 1) {
      warning("eta is not intended for vectorization in this function")
      return(NULL)
   }
   eps <- sqrt(.Machine$double.eps)
   A <- eps; B <- 1 - A
   intn <- NULL # WHA addition for trapping numerical integration failure
   try(intn <- integrate(peta, lower=A, upper=B, n=n, r=r, eta=eta), silent=silent)
   if(is.null(intn)) { # WHA addition
      if(! silent) message("Monte Carlo integration fall back")
      petas <- peta(runif(n.sim), n=n,r=r,eta=eta)
      try(intn <- (B-A)*sum(petas)/n.sim, silent=silent)
      if(is.null(intn)) return(list(value=NA, monte.carlo.error=NA, n.sim=n.sim))
      err <- sqrt((B-A)^2 * var(petas) / n.sim)
      intn <- list(value=intn, monte.carlo.error=err, n.sim=n.sim)
   }
   return(intn) # it is critical that this function return at least list(value=###)
}


#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN   This routine employs simple Gaussian quadrature to compute the integral
#COHN   (above) much more quickly. However, it is slightly less accurate for
#COHN   tail probabilities
#COHN
#COHN KthOrderPValueOrthoTb <- function(n,r,eta,nterms=50){
#COHN                    x    <-  gauss.quad.prob(nterms)
#COHN                    sapply(x$nodes,peta,n=n,r=r,eta=eta) %*% x$weights
#COHN      }

#"KthOrderPValueOrthoTb" <- function(n, r, eta, nterms=50) {
#   x    <-  gauss.quad.prob(nterms)
#   peta(x$nodes, n=n, r=r, eta=eta) %*% x$weights
#}

# gauss.quad.prob  WHA(05/20/2017): I don't know source of this function

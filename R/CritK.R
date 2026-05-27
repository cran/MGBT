#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN   Inverse function:  Computing critical point given probability
#COHN     Note that initial guess is based on approximating integral by
#COHN          function value at 50%-ile.
#COHN
#CritK<-function(n,r,p){
#  guess1 = uniroot(function(eta)(peta(.5,n,r,eta)-p),
#           interval=c(-10,10))$root
#  uniroot(function(x,n,r) {KthOrderPValueOrthoT(n,r,x)$value-p},
#           interval=guess1+c(-1,1),n=n,r=r)$root
#  }

"CritK" <- function(n, r, p) {

  "gfunc" <- function(eta) (peta(0.5, n, r, eta) - p)
  guess1 <- NULL
  try(guess1  <- uniroot(gfunc, interval=c(-10,10))$root)
  if(is.null(guess1)) {
     warning("could not uniroot guess1, do not know what to do, need TAC, NA returned")
     return(NA)
  }

  "kfunc" <- function(x,n,r) { RthOrderPValueOrthoT(n, r, x)$value - p }
  critk <- NULL
  try(critk <- uniroot(kfunc, interval=guess1+c(-1,1), n=n, r=r)$root)
  if(is.null(critk)) {
     warning("could not uniroot kfunc, do not know what to do, need TAC, NA returned")
     return(NA)
  }
  return(critk)
}



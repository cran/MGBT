#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN   Expected values of M and S
#COHN
"EMS" <- function(n, r, qmin) {
         if(length(qmin) != 1) {
            warning("not expecting a vector for qmin, using only the first value")
            qmin <- qmin[1]
         }
         zr       <- qnorm(qmin)
         Em       <- gtmoms(zr,1)
         momS2    <- CondMomsChi2(n, r, zr)
           alpha  <- momS2[1]^2 / momS2[2]
           beta   <- momS2[2]   / momS2[1]
         suppressWarnings(Es <- sqrt(beta)*exp( lgamma(alpha+0.5) - lgamma(alpha)))
         c(Em[1], Es)
}


#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN   Covariance matrix of M and S
#COHN
#COHN "VMS" <- function(n,r,qmin){
#COHN          E    <- sapply(1:2,function(i){gtmoms(qnorm(qmin),i)});
#COHN          Es   <- EMS(n,r,qmin)[2]
#COHN          Es2  <- E[2]-E[1]^2
#COHN          V2   <- V(n,r,qmin)
#COHN          array(c(
#COHN                  V2[1,1],
#COHN                  rep(V2[1,2]/(2*Es),2),
#COHN                  Es2-(Es)^2
#COHN                 ),dim=c(2,2))
#COHN }

"VMS" <-
function(n, r, qmin) {
   if(length(qmin) != 1) {
      warning("not expecting a vector for qmin, using only the first value")
      qmin <- qmin[1]
   }
   E   <- sapply(1:2, function(i) gtmoms(qnorm(qmin), i))
   Es  <- EMS(n, r, qmin)[2]
   Es2 <- E[2] - E[1]^2
   V2  <- V(n,r,qmin)
   array(c(V2[1,1], rep(V2[1,2]/(2*Es),2), Es2-(Es)^2),
         dim=c(2,2))
}

#COHN ****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
#COHN
#COHN   Covariance matrix of M and S^2
#COHN     Note that E is vector of expected values of X^k given X>qmin
#COHN
#COHN "V" <- function(n,r,qmin){
#COHN         n2 <- n-r
#COHN       E<-sapply(1:4,function(i){gtmoms(qnorm(qmin),i)});
#COHN 	   cm      <- c(E[1],
#COHN 					E[2]-E[1]^2,
#COHN 					E[3]-3*E[2]*E[1]+2*E[1]^3,
#COHN 					E[4]-4*E[3]*E[1]+6*E[2]*E[1]^2-3*E[1]^4
#COHN 				   )
#COHN       array(
#COHN         c((E[2] - E[1]^2)/n2,
#COHN         rep(
#COHN           (E[3] - 3*E[1]*E[2] + 2*E[1]^3)/sqrt(n2*(n2-1))
#COHN           ,2),
#COHN           (cm[4]-cm[2]^2)/n2 + 2/((n2-1)*n2)*cm[2]^2),
#COHN        dim=c(2,2))
#COHN }

"V" <- function(n, r, qmin) {
   if(length(qmin) != 1) {
      warning("not expecting a vector for qmin, using only the first value")
      qmin <- qmin[1]
   }
   k  <- n-r
   E  <- sapply(1:4, function(i) gtmoms(qnorm(qmin),i) )
	  cm <- c(E[1],
					      E[2] -   E[1]^2,
					      E[3] - 3*E[2]*E[1] + 2*E[1]^3,
					      E[4] - 4*E[3]*E[1] + 6*E[2]*E[1]^2 - 3*E[1]^4)
   array(  c( cm[2]/k, # streamline because TAC has this without the k already computed
         rep( cm[3]/sqrt(k*(k-1)), 2),  # streamline because TAC has this without the k already computed
             (cm[4] -  cm[2]^2)/k + 2/((k-1)*k)*cm[2]^2),
         dim=c(2,2))
}

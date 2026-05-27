#peta <- function(pzr,n,r,eta){
#                   zr       <- qnorm(qbeta(pzr,shape1=r,shape2=n+1-r))
#                   CV       <- VMS(n,r,qmin=pnorm(zr))
#                   lambda   <- CV[1,2]/CV[2,2]
#                   etap     <- eta + lambda
#                   EMp      <- EMS(n,r,qmin=pnorm(zr))
#                   muMp     <- EMp[1]-lambda*EMp[2]
#                   SigmaMp  <- sqrt(CV[1,1] - CV[1,2]^2/CV[2,2]);
#                   momS2    <- CondMomsChi2(n,r,zr)
#                   shape    <- momS2[1]^2/momS2[2]
#                   scale    <- momS2[2]/momS2[1]
#                   sqrtS2   <- sqrt(momS2[1])
#                   df       <- 2*shape
#                   ncp      <- (muMp-zr)/SigmaMp
#                   q        <- -(sqrtS2/SigmaMp)*etap
#               (1 - pt(q,df=df, ncp=ncp, lower.tail = TRUE))
#     }

"peta" <- function(pzr, n, r, eta) {
   sapply(pzr, function(the.pzr) {
      br     <- qbeta(the.pzr, shape1=r, shape2=n+1-r) # isolated by WHA
      zr     <- qnorm(br) # so that br can be used twice without need for pnorm().
      COV    <- VMS(n, r, qmin=br)
      EMp    <- EMS(n, r, qmin=br)
      lambda <- COV[1,2] / COV[2,2] # renamed to be more COVariance like
      etap   <- eta    + lambda
      muMp   <- EMp[1] - lambda*EMp[2]
      suppressWarnings(SigmaMp  <- sqrt(COV[1,1] - COV[1,2]^2/COV[2,2]))  # WHA error trapping
      if(! is.finite(SigmaMp)) return(1) # TODO, Is unity okay? Seems so  # WHA error trapping
      momS2    <- CondMomsChi2(n,r,zr)
                       # shape <- momS2[1]^2/momS2[2]  # WHA: only needed for df
                       # scale <- momS2[2]  / momS2[1] # WHA: not used by TAC
      q   <- -(sqrt(momS2[1])  / SigmaMp)*etap # a quantile for pt()
      df  <-     2*(momS2[1]^2 / momS2[2])     # degrees of freedom
      ncp <-         (muMp-zr) / SigmaMp       # non-centrality parameter
      #if(abs(ncp) > 37.62) { # WHA limit exploring of pt(), 37.62 from R documentation
      #   message("Exploring limits of peta:",q, ", df=",df," and ncp"=ncp)
      #}
      suppressWarnings(the.pt <- pt(q, df=df, ncp=ncp, lower.tail=TRUE))
      return(1 - the.pt)
   })
}

# Peaks <- c(53, 88, 55, 45, 40); MGBT(Peaks)
# if(! is.finite(SigmaMp)) return(1)
#$pvalues for 40 and 45
#[1] 0.8139058 0.8011052

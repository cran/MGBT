"ASlo" <- function(mu, sigma, gamma) {
   isInside <- TRUE
   if(mu    <  1.9   | mu    > 4.842) isInside <- FALSE
   if(sigma <  0.125 | sigma > 1.814) isInside <- FALSE
   if(gamma < -2.714 | gamma > 0.698) isInside <- FALSE
   if(! isInside) {
      warning("input data outside of the potential applicable range of the regression")
   }
   10^(1.09*mu - 0.584*sigma + 0.14*gamma - 0.799)
}

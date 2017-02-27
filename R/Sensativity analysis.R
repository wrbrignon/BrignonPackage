#' Sensativity analysis function
#' 
#' Outputs a parameter value of the beta or gamma distribution as a function of a mean value, distribution, quantile, and coeficent of variation
#' @param vary a character string of the parameter name to vary
#' @param pct a quantile location (e.g., 0.05,0.5,0.95)
#' @param dist "beta" or "gamma"
#' @param cv coefficent of variation
#' 
#' @return Returns the new value of the vary parm
#' 
#' @export

sensy <- function(vary = NULL,  
                  pct = 0.5,  
                  dist = "beta", 
                  cv = 0.5){
  
  value <- get(vary)
  sd <- value * cv
  mom <- mom(dist,value,sd)  
  if (dist == "beta") out = qbeta(pct,mom[1],mom[2])
  if (dist == "gamma") out = round(qgamma(pct,mom[1],1/mom[2]),0)
   return(out)
}



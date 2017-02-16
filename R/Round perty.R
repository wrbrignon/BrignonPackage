#' Round Perty
#' 
#' Rounds a number of vector of numbers to a perty number for plot axis or
#' whatever else.
#' @param x a number or vector
#' @param lower_bound logical to round down(default) or round up 
#' @param nice_small a vector scalar for rounding small numbers (<100)
#' @param nice_big a vector scalar for rounding big numbers (>100)
#' 
#' @return Returns rounded number or vector
#' @examples
#' round.perty(c(4,12,233,49488), lower_bound = T)
#' round.perty(c(4,12,233,49488), lower_bound = F)

round.perty <- Vectorize(function(x, lower_bound = TRUE, nice_small=c(0,5,10), nice_big=c(1,2,3,4,5,6,7,8,9,10)) {
  if (abs(x) > 100) {
    nice = nice_big
  } else {
    nice = nice_small
  }
  if (lower_bound == TRUE) {
    if (x > 0) {
      return(10^floor(log10(x)) * nice[[max(which(x >= 10^floor(log10(x)) * nice))[[1]]]])
    } else if (x < 0) {
      return(- 10^floor(log10(-x)) * nice[[min(which(-x <= 10^floor(log10(-x)) * nice))[[1]]]])
    } else {
      return(0)
    }
  } else {
    if (x > 0) {
      return(10^floor(log10(x)) * nice[[min(which(x <= 10^floor(log10(x)) * nice))[[1]]]])
    } else if (x < 0) {
      return(- 10^floor(log10(-x)) * nice[[max(which(-x >= 10^floor(log10(-x)) * nice))[[1]]]])
    } else {
      return(0)
    }
  }
})






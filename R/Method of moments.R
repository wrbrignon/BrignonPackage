#' Method of Moments
#'
#' Method of Moments to determine parameters for gamma or beta distributions.
#' @param distribution enter "beta" or "gamma"
#' @param mean  mean value of samples
#' @param sd  standard deviation of samples
#' @return Returns parameters \code{a} (i.e., shape 1) and \code{b} (i.e., shape
#'   2) of the \strong{beta} distribution.
#'
#'   Returns parameters \code{c} (i.e., shape) and \code{b} (i.e., rate) of the
#'   \strong{gamma} distribution.  Note that the scale parameter = 1/b (See example).
#'
#'   A warning will show if negative values are obtained. distribution.
#'
#' @examples
#' ##Beta Distribution
#' ##assign beta parms from mean = 0.5 and sd = 0.2 to outz
#' outz = mom("beta",0.5, 0.2)
#'
#' ##use outz to generate 10 random beta observations from mean = 0.5 and sd = 0.2
#' rbeta(10,outz[1],outz[2])
#'
#' ##Example of negative parameter estimate function
#' mom("beta",50, 0.2)
#'
#' ##Gamma Distribution
#' ##Generate some probability levels
#' prob_levels<-c(0.001,.05,.25,.5,.75,.95,0.999)
#'
#' ##Generate gamma parms with man of 10 and sd of 5
#' outz <- mom("gamma",10, 5)
#'
#' ##Use the gamma parms (outz) and the probability with qgamma to find the
#' ##quantile values of a gamma dist. Note the 1/b component to calculate the scale parameter
#' qgamma(prob_levels,outz[1],1/outz[2])
#' @export

mom<-function(distribution, mean, sd){
  if (distribution != "beta" && distribution != "gamma") stop("Warning wrong distribution")
  v<-sd**2
  x<-mean
  if (distribution == "gamma"){
    c<-(x/sd)^2
    b<-v/x
    cb <- c(c=c,b=b)
    ifelse(cb < 0 ,stop("Warning: Negative parameter values obtained"),outz <- cb)
  }
  if (distribution == "beta"){
    a<-x*(x*(1-x)/v-1)
    b<-(1-x)*(x*(1-x)/v-1)
    ab <- c(a=a,b=b)
    ifelse(ab < 0 ,stop("Warning: Negative parameter values obtained"),outz <- ab)
  }
  return(outz)
}





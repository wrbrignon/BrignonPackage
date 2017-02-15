#' Data Summary
#'
#' Gives count, sum, max, min, mean, standard deviation, standard error of the
#' mean, and confidence interval.  I altered this from the
#' Rmisc::summarySE func.
#' @param data a data frame to summarize
#' @param measurevar the name of a column that contains the variable to be
#'   summariezed
#' @param groupvars a vector containing names of columns that contain grouping
#'   variables
#' @param na.rm a boolean that indicates whether to ignore NA's
#' @param conf.interval the percent range of the confidence interval (default is 95 percent)
#' @param .drop should combinations of variables that do not appear in the input
#'   data be preserved (FALSE) or dropped (TRUE, default)
#'
#' @return Returns a dataframe with the summary metrics.  Usefull in data
#'   exploration, plotting whisker plots, and other good ish.
#'
#' @examples
#' ## make sample data
#' a = seq(0,2000,length.out = 40)
#' b = seq(2000,4000,length.out = 40)
#' c = c(rep("blue",20),rep("red",20))
#' d = c(rep("north",10),rep("west",10),rep("south",10),rep("east",10))
#'
#' ##join above obejcts to dataframe
#' tmp = cbind.data.frame(a,b,c,d)
#'
#' ##Look at data fame
#' tmp
#'
#' ##Single grouping variables
#' sumdater(tmp,measurevar = "a",groupvars = "c")
#' sumdater(tmp,measurevar = "a",groupvars = "d")
#'
#' ##multiple grouping variables
#' sumdater(tmp,measurevar = "a",groupvars = c("c","d"))
#' @export


sumdater <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  require(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      sum = sum   (xx[,col], na.rm=na.rm),
                      max = max   (xx[,col], na.rm=na.rm),
                      min = min   (xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}

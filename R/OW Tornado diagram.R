#' Tornado Diagram for One-Way Sensitivity Analysis
#' 
#' Output is a tornado diagram for SDM sensativity analysis.
#' @param data dater frame or matrix containing decision, utility, parm
#'   (parameter), pct (increment) as column names.
#' @param best.outcome either max or min.  Represents whether the utility is
#'   optimized at minimum or maximun values.
#' @param x.axis.title self explanitory
#' @param xlim.big optional input to fine tine the upper limit of the x axais in
#'   it gets weird
#' @param xlim.small similar to xlim.big
#'   
#' @return Returns a barplot in the form of a tornado diagram 
#' I tested this for the most part but I bet it will throwup on me at
#'   some point.  I'll keep updating the codez until it holds them down.  Uses
#'   the nicenumbs funciton in the BrignonPackage for sexy axis plotting. 
#'   Requires installing the gplots and Hmisc packages.
#'   
#' @examples 
#' ##Lets make some fake dater called infile
#' decision = seq(1,2,1)
#' parm = paste("parm",seq(1,25,1))
#' pct = seq(0.05,0.95,length.out = 10)
#' utility = sample(rnorm(1,0,50):runif(1,50,500),500,replace = T)
#' infile = cbind(expand.grid(decision=decision,parm = parm,pct = pct),utility = utility)
#' 
#' ##Open seperate plotting window and make da plot
#' x11()
#' OW.tornado(infile,best.outcome = max,x.axis.title = "Coach Ditka's rating on Madden 1985")
#' 
#' ##Save plot to directory
#' png(file="~/one way sensativity output.png",width = 1000,height = 800,res = 100)
#' OW.tornado(infile,best.outcome = max,x.axis.title = "Coach Ditka's rating on Madden 1985")
#' dev.off()
#' @export

OW.tornado <- function(data = NULL,best.outcome = max, x.axis.title = "Utility", xlim.big = NULL, xlim.small = NULL){
  
  ### load packages for creating tornado diagram
  require(gplots)
  require(Hmisc)
  
  ## Find optimal outcome/greatest utility (i.e.,  max or min) for each combination of
  ## parm and increment (pct)
  bests <- aggregate(data["utility"], by= data[c("parm","pct")], best.outcome)
  
  ## Find and cbind the minimum and maximum utilities of the optimal outcome/greatest
  ## utility for each parameter,  These will define the ends of the bars in the
  ## tornado diagram
  min.ut = aggregate(bests[c(3)],by=bests[c(1)],min)
  max.ut = aggregate(bests[c(3)],by=bests[c(1)],max)
  sens = cbind(min.ut,max.ut)[,-3]
  
  ## rename columns
  colnames(sens) = c("Component","Min","Max")
  
  ## calculate the difference between the min and max
  ## this will be the width of the bars in the diagram
  sens$Dif = sens$Max - sens$Min
  
  ################# sort from least to most sensitive
  sens = sens[order(sens$Dif),]
  
  ## these will be the names of the components on the y-axis
  parm.names = sens$Component
  
  ## transpose the matrix to have each column for a component with the first row
  ## as the minimum utility and the second row as the difference between min and
  ## max.
  one.way <- as.data.frame(t(sens[,c(2,4)]))
  colnames(one.way) <- parm.names 
  
  ## create tornado diagram
  plot.data <- as.matrix(one.way)
  
  ## setup pars
  par(oma=c(0,0,0,0),mar=c(5,8,2,2))   
  
  ## setup xaxis limits
  xlim.big <- ifelse(is.null(xlim.big),nicenumbs(max(plot.data[1,] + plot.data[2,]),lower_bound = F),xlim.big)
  xlim.small <- ifelse(is.null(xlim.small),nicenumbs(min(plot.data[1,]),lower_bound = T),xlim.small)
  xticks <- pretty(xlim.small:xlim.big,n = 5)
  xs <- c(min(xticks),max(xticks))
  ## Its pretty much just a fancy barplot
  bp <- barplot2(plot.data,beside=F,las=1,horiz=TRUE, 
                xlim = xs,xaxt = "n",
                col=c("white","black","white"),border=c("white","white","white"),xlab = x.axis.title,ylab = NULL,yaxt = "n")
  
  axis(side = 1,at = (xticks))
  mtext(text = parm.names,side = 2,at = bp,las = 1,line = 0.5)
  abline(v= min(xticks))
  
}




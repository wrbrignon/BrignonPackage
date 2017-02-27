#################################################
#################################################
#### Script creates one-way response profile
#### sensitivity analysis plots
############# file containing decision, utility, parameter, increment
#infile <- sens.resultz
#infile = read.csv("one way sensativity output.csv")[,2:6]
## grab parameters to be evaluated
#require(lattice)

#for(i in 1:length(parms)){
#  working<- subset(infile,parm == parms[i])
#  png(file=paste("./sens plots/Indiff plots/",parms[i]," basics.png",sep = ""),
#      width = 8,height = 8, res = 300, units = "in")
#  print(xyplot(quasi.ext~pct,groups = decision, type = c("a"), data = working,
#               par.settings=list(superpose.line=list(lwd = 2)),
#               xlab="Range of values", ylab="Expected utility",ylim = c(-5,200),
#               main=paste("Response profile for ", parms[i], sep =""), 
#               auto.key = list(space = "top",columns = max(infile$decision), lines = T, points = F)))
#  dev.off()
#}
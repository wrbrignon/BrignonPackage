#' Install and load packages
#'
#' This will check to see if packages are installed. Install them if they are
#' not, then load them into the R session.
#' @param pkg a vector of packages
#' @return Packages will be installed and loaded.
#' @examples
#' ## usage
#' packages <-  c("curl", "httr","plyr","devtools")
#' ipak(packages)
#' @export


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,verbose = T)
  sapply(pkg, require, character.only = TRUE)
}



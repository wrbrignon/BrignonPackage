##install packages/libraries required to load BrignonPackage
install.packages(c("devtools","httr"))
library(devtools)
library(httr)

##work around for certificate error
httr::set_config( config( ssl_verifypeer = 0L ) )

##install and load BrignonPackage
install_github("wrbrignon/BrignonPackage")
library(BrignonPackage)

##test if it worked
mom("beta",0.5,0.2)   ##get beta dist parms
?ipak  ##show ipak help document
?sensy

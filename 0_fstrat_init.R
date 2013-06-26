############################ PACKAGES ###################################################
#########################################################################################
pkgL 	<- function(pckg){sapply(pckg, require, character.only=TRUE, quietly=TRUE)}
pkgL(c(
	"quantmod",
	"lubridate",
	"sendmailR",
	"PerformanceAnalytics",
	"timeDate"
	))


############################ FUNS #######################################################
#########################################################################################
db_fol 	<- paste0(dirname(sys.frame(1)$ofile),"/1_dbase/")
fun_fol <- paste0(dirname(sys.frame(1)$ofile),"/0_funs/")
funL 	<- function(files, dir=fun_fol){sapply(paste0(dir,files), source)}

funL(c(
	"1_basic.R", "1_port_obj.R",
	"2_indicators.R",
	"5_charting.R"
	))




############################ OTHER ######################################################
#########################################################################################
setwd(dirname(sys.frame(1)$ofile))
Sys.setenv(TZ='GMT')


############################ PACKAGES ###################################################
#########################################################################################
pkgL 	<- function(pckg){sapply(pckg, require, character.only=TRUE, quietly=TRUE)}
pkgL(c(
	"quantmod",
	"lubridate",
	"sendmailR",
	"PerformanceAnalytics"
	))


############################ FUNS #######################################################
#########################################################################################
db_fol 	<- paste0(dirname(sys.frame(1)$ofile),"/1_dbase/")
fun_fol <- paste0(dirname(sys.frame(1)$ofile),"/0_funs/")
funL 	<- function(files, dir=fun_fol){sapply(paste0(dir,files), source)}

funL(c(
	"1_basic.R", "1_core.R",
	"2_candles.R", "2_indicators.R", "2_oscilators.R", 
	"5_charting.R",
	"9_fstrat_main.R"
	))




############################ OTHER ######################################################
#########################################################################################
setwd(dirname(sys.frame(1)$ofile))


########################## SIGNALS ##############################################
#################################################################################
# ge      <- function(a, b){ as.numeric(a)  > as.numeric(b) }
# le      <- function(a, b){ as.numeric(a)  < as.numeric(b) }
# geq     <- function(a, b){ as.numeric(a) >= as.numeric(b) }
# leq     <- function(a, b){ as.numeric(a) <= as.numeric(b) }

# seriesIncr(x, thresh=0, diff.=1L)
# seriesDecr(x, thresh=0, diff.=1L)

#### TIME #################################
###########################################
# trading_day <- function(x, days=c(2,3,4,5,6)){
# 	# 1-sunday
# 	x 		<- try.xts(x, error = as.matrix)
# 	time 	<- index(x)
# 	weekd 	<- lubridate::wday(time)
# 	tday 	<- as.numeric(sapply(weekd,function(x){any(x==days)}))
# 	reclass(tday, x)}

# trading_time <- function(x, range="0800/1400"){
# 	is.within 	<- function(x){
# 		value 	<- hour(x)*60+minute(x)
# 		(value > range[1] && value < range[2])}
# 	split 	<- as.numeric(strsplit(range,"/")[[1]])
# 	range 	<- (split%/%100)*60+(split%%100)
# 	x 		<- try.xts(x, error = as.matrix)
# 	time 	<- index(x)
# 	ttime 	<- as.numeric(sapply(time,is.within))
# 	reclass(tday, x)}


########################## INDICATORS ###########################################
#################################################################################
# runSum, runMin, runMax, runMean, runMedian, runCov, runVar, runSD, runMAD
# system.time(replicate(100,ROC(x)))
roc <- function (x, n = 1, na = TRUE){
    x 	<- try.xts(x, error = as.matrix)
    roc <- diff(log(x), n, na.pad = na)
    reclass(roc, x)}

RS <- function(x, n=10){
	x <- OHLC(x)
	ind <- ifelse(Cl(x)>Op(x),1,2)
	RS<-numeric()
	for (i in n:nrow(x)){
		means <- tapply(Cl(x[(i-n+1):i, ]), ind[(i-n+1):i],mean)
		up=ifelse(is.na(means["1"]),1,means["1"])
		down=ifelse(is.na(means["2"]),1,means["2"])
		RS[i] <- up/down}
	return(RS)}

ddown <- function(x){
	ret.cum 	<- cumprod(1 + x)
	max.cum.ret <- cummax(c(1, ret.cum))[-1]
	drawdown 	<- ret.cum/max.cum.ret - 1
	min(drawdown)}

########################## FILTERS ##############################################
#################################################################################
SMOOTHE <- function(x, level=0.1){
	SMOO <- function(x, level=0.1){return(lowess(x,f=level)$y)}
	x <- as.numeric(na_zero(inf_zero(x)))
	smoothed<-numeric()
	smoothed[1]<-x[1]
	if (x[1]!=0) start<-1
	for (i in 2:length(x)){
		if(i==length(x)){
			if(x[i]!=0 && x[i-1]!=0){
				smoothed[start:i]<-SMOO(x[start:i],level=level)
				next}}

		if(x[i]==0 && x[i-1]==0){
			smoothed[i] <-0
		}else if(x[i]!=0 && x[i-1]==0){
			start <-i
		}else if(x[i]!=0 && x[i-1]!=0){
		}else if(x[i]==0 && x[i-1]!=0){
			smoothed[start:(i-1)]<-SMOO(x[start:(i-1)],level=level)
			smoothed[i]<-0
		}
	}
	return(smoothed)
}
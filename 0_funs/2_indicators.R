########################## ESSENTIAL INDICATORS #################################
#################################################################################
# ge      <- function(a, b){ as.numeric(a)  > as.numeric(b) }
# le      <- function(a, b){ as.numeric(a)  < as.numeric(b) }
# geq     <- function(a, b){ as.numeric(a) >= as.numeric(b) }
# leq     <- function(a, b){ as.numeric(a) <= as.numeric(b) }

lt_than <- function(x, cl){
	x 	<- try.xts(x, error = as.matrix)
	res <- as.numeric(x)
	res <- sapply(res,function(x,const=cl){(const>x)})
	reclass(res, x)}

gt_than <- function(x, cg){
	x 	<- try.xts(x, error = as.matrix)
	res <- as.numeric(x)
	res <- sapply(res,function(x, const=cg){(const<x)})
	reclass(res, x)}

in_range <- function(x, l, h){
	x 	<- try.xts(x, error = as.matrix)
	res <- as.numeric(x)
	res <- sapply(res,function(x,low=l,high=h){(low<x && x<high)})
	reclass(res, x)}

cross_point <- function(a, b){	# 1 if a crosses b from below, -1 if from above
	cont_signal <- (a > b)
	cont_signal[is.na(cont_signal)] = FALSE
	funia 	<- function(x){
			if(as.numeric(x[2]) & as.numeric(!x[1])) 1 else if(as.numeric(!x[2]) & as.numeric(x[1])) -1 else 0 }	
	rollapply(cont_signal, 2, funia)}

########################## INDICATORS ###########################################
#################################################################################
trading_day <- function(x, days=c(2,3,4,5,6)){
	# 1-sunday
	x 		<- try.xts(x, error = as.matrix)
	time 	<- index(x)
	weekd 	<- lubridate::wday(time)
	tday 	<- as.numeric(sapply(weekd,function(x){any(x==days)}))
	reclass(tday, x)}

trading_time <- function(x, range="0800/1400"){
	is.within 	<- function(x){
		value 	<- hour(x)*60+minute(x)
		(value > range[1] && value < range[2])}
	split 	<- as.numeric(strsplit(range,"/")[[1]])
	range 	<- (split%/%100)*60+(split%%100)
	x 		<- try.xts(x, error = as.matrix)
	time 	<- index(x)
	ttime 	<- as.numeric(sapply(time,is.within))}

########################## INDICATORS ###########################################
#################################################################################
roc <- function (x, nn = 1, na = TRUE){
    x <- try.xts(x, error = as.matrix)
    roc <- diff(log(x), nn, na.pad = na)
    reclass(roc, x)}

rocn <- function (x, nnn = 1){
    rocn <- lag(roc(x),nnn)
    reclass(rocn, x)}

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
	Return.cumulative = cumprod(1 + x)
	maxCumulativeReturn = cummax(c(1, Return.cumulative))[-1]
	drawdown = Return.cumulative/maxCumulativeReturn - 1
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
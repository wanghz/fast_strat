


################# ONE OFFS ###########################################################################################################################
######################################################################################################################################################
cndl.gravestone <- function(x, down=-0.5, up=-0.3, frac=0.01){
	gfun <- function(x, down, up, frac){
		tmp 	<- as.numeric(x)
		oz 		<- -0.5+(x - min(x))/(max(x)-min(x))
		if(any(is.nan(oz))){FALSE
		}else{
			if( all(oz[c(1,4)] %inside% c(down,up)) && (max(x)-min(x)) > frac*max(x)){
				TRUE
			}else{FALSE}}}
	temp <- apply(x, 1, gfun, down=down, up=up, frac=frac)
	reclass(temp,x)}

cndl.hammer <- function(x, down=0.3, up=0.5, frac=0.01){
	gfun <- function(x, down, up, frac){
		tmp 	<- as.numeric(x)
		oz 		<- -0.5+(x - min(x))/(max(x)-min(x))
		if(any(is.nan(oz))){
			FALSE
		}else{
			if( all(oz[c(1,4)] %inside% c(down,up)) && (max(x)-min(x)) > frac*max(x)){
				TRUE
			}else{FALSE}}}
	temp <- apply(x, 1, gfun, down=down, up=up, frac=frac)
	reclass(temp,x)}

################# PATTERNS ###########################################################################################################################
######################################################################################################################################################





# X[which(cndl.hammer(X,down=0.2,frac=0.001)), ]
# chartSeries(X)

# stock <- OHLC(getSymbols("BARC.L", auto.assign=FALSE))
# gr <- which(cndl.gravestone(stock))
# hm <- which(cndl.hammer(stock))
# chartSeries(stock[hm,])

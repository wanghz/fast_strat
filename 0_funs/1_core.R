########################## FAST FRAMEWORK #######################################
#################################################################################
port_new <- function(data){
	pos 			<<- 0
	temp 			<-  xts(matrix(0, nrow(data), 2), index(data))
	colnames(temp) 	<-  c("orders", "posqty")
	assign("mkdata", temp, envir=.GlobalEnv)}

order_add <- function(qty, time, price){
	new_qty <- get("mkdata",envir=.GlobalEnv)[(time-1),"posqty"] + qty
	if (new_qty!=0) {pos <<- as.numeric(new_qty)} else{pos <<- 0}
	mkdata[time,"orders"] <<- qty
	mkdata[time,"posqty"] <<- new_qty
	long <- sign(qty)==1
	cat(time, ifelse(long,"long","short"),price,"\n")}

aggreg <- function(time){
	if(pos!=0 && as.numeric(mkdata[time,"posqty"])==0){
		mkdata[time,"posqty"] <<- mkdata[(time-1),"posqty"]}}

finalize <- function(mkdata, X, pricefun = c("open","close"), init=1e4, spread=2){
	pricefun = match.arg(pricefun)
	n <- nrow(mkdata)
	if(pricefun=="open"){
		mkdata=lag(mkdata)
		pricecol <- Op(X)
	} else if(pricefun=="close"){
		pricecol <- Cl(X)}
	mkdata$strat_pl <- (pricecol[2:n,]-as.numeric(pricecol[1:(n-1),]))*as.numeric(mkdata[1:(n-1),"posqty"])
	pl_tax			<- (mkdata$strat_pl-abs(as.numeric(mkdata$orders * spread*0.0001/2)))
	mkdata$strat_pl_tax 	<- pl_tax
	mkdata[1:3,] 	<- 0
	mkdata$strat_balance 	<- cumsum(mkdata$strat_pl_tax)+init
	if(pricefun=="open"){mkdata	<- lag(mkdata)}
	mkdata$strat_ret <- ROC(mkdata$strat_balance,type="discrete")
	augmented 		<- merge(X,mkdata)
	return(augmented)}



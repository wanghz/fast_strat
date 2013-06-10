########################## FAST CHARTING ########################################
#################################################################################
fast_theme <- chartTheme(up.col='#065a85',dn.col='#b9571c', 
							up.border='#065a85', down.border='#b9571c',
							area='#eaeaea',		# pagrindine spalva uz candles!
							bg.col='#000000',  	# remu spalva
							fg.col='#b9571c', 	
							grid.col='#1a1a1a',	# grid
							main.col="green",
							fill='#9f9f9f')
# chartTheme('white')
POS 	<- function(X){return(X[,"posqty"])}
BAL 	<- function(X){return(X[,"strat_balance"])}
CUM 	<- function(X){return(cumprod(1+X[,"strat_ret"]))}
Line 	<- function(x, l=1){return(reclass(rep(l,nrow(x)),x))}
Orders 	<- function(X, price=Cl){return(abs(sign(zero_na(X[,"orders"]))*price(X)))}
addPOS 	<- newTA(POS, lwd=2, legend='position Q')
addBAL 	<- newTA(BAL, lwd=2, col='#86b825',legend='balance',yrange=c(9950,12000))
addCUM 	<- newTA(CUM, lwd=2, col='#00b134',legend='performance')
addLine <- newTA(Line, l=1, lwd=1, col='#ff0019',legend.name='',on=NA)
addVolatility <- newTA(volatility, preFUN=Cl, col=4, lwd=2)
addOrders <- newTA(Orders, type='b', col="red", pch=9, on=1)

fast_plot <- function(X, TA=list("addOrders()", "addPOS()", "addBAL()"), theme=fast_theme, ...){
	chartSeries(x=X, TA=TA, theme=theme, ...)}

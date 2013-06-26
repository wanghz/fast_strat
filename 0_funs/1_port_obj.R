Strat <- function(data, init=1e4, qty=5*init, spread=2){
	data 			<- OHLC(data)
	colnames(data) 	<- c("open", "high", "low", "close")
	ttime 			<- xts(rep(1,nrow(data)), index(data))
	data 			<- merge(data,ttime)
	StratObj(data=data, start=1L, init=init, qty=qty, spread=spread)
}

StratObj <- setRefClass("tstrat",
	fields = list(
		data 		= "xts",
		start 		= "integer",
		init		= "numeric",
		qty 		= "numeric",
		spread 		= "numeric"
	 	),
	methods = list(
		names = function(){
			print(colnames(data))
		},
		time = function(from=0, to=23, days=2:6){
			time 	<- index(data)
			tday 	<- as.numeric(sapply(wday(time),function(x){any(x==days)}))
			ttime 	<- as.numeric(sapply(hour(time), function(x){x >= from && x <= to}))
			data[,"ttime"] <<- reclass((tday & ttime), data)
		},
		ind = function(fun, name="ind", ..., prefun=Cl){
			temp 	<- fun(prefun(data), ...)
			colnames(temp) <- name
			start 	<<- as.integer(sum(is.na(temp))+1)
			data 	<<- merge(data, temp)
		},
		############################################################################################
		signal = function(name="sig", type=c("le","ge","in","cross"), col1, col2, range=c(0,1)){
			type = match.arg(type)
			if(type=="le"){
				# col1 < col2
				temp <- data[ ,col1] < data[ ,col2]
				colnames(temp) <- name
				data <<- merge(data, temp)
			}else if(type=="ge"){
				# col1 > col2
				temp <- data[ ,col1] > data[ ,col2]
				colnames(temp) <- name
				data <<- merge(data, temp)
			}else if(type=="in"){
				# dn < col1 < up
				temp <- range[1] < data[ ,col1] & data[ ,col1] < range[2]
				colnames(temp) <- name
				data <<- merge(data, temp)
			}else if(type=="cross"){
				# col1 crosses col2 from below
				cont_signal <- as.numeric(data[ ,col1] > data[ ,col2])
				cont_signal[is.na(cont_signal)] = FALSE
				funia 	<- function(x){
					if(as.numeric(x[2]) & as.numeric(!x[1])) 1 else 0 }	
				temp <- reclass(rollapplyr(cont_signal, 2, funia, fill=0),data)
				colnames(temp) <- name
				data <<- merge(data, temp)
			}
		},
		l.entry = function(columns, true){
			fincol <- do.call(merge, lapply(columns,function(x){data[,x]}))
			fincol[is.na(fincol)] <- 0
			temp <- reclass(apply(fincol, 1, function(x){all(x==true)}),data)
			colnames(temp) <- "l.entry"
			data <<- merge(data, temp)
		},
		l.exit = function(columns, true){
			fincol <- do.call(merge, lapply(columns,function(x){data[,x]}))
			fincol[is.na(fincol)] <- 0
			temp <- reclass(apply(fincol, 1, function(x){all(x==true)}),data)
			colnames(temp) <- "l.exit"
			data <<- merge(data, temp)
		},
		s.entry = function(columns, true){
			fincol <- do.call(merge, lapply(columns,function(x){data[,x]}))
			fincol[is.na(fincol)] <- 0
			temp <- reclass(apply(fincol, 1, function(x){all(x==true)}),data)
			colnames(temp) <- "s.entry"
			data <<- merge(data, temp)
		},
		s.exit = function(columns, true){
			fincol <- do.call(merge, lapply(columns,function(x){data[,x]}))
			fincol[is.na(fincol)] <- 0
			temp <- reclass(apply(fincol, 1, function(x){all(x==true)}),data)
			colnames(temp) <- "s.exit"
			data <<- merge(data, temp)
		},
		run = function(){
			#######################################################################
			ttime <- data[,c("ttime")]
			if(!is.null(data$l.entry)){
				if(is.null(data$l.exit)) stop("Long exit not defined")
				l.sig <- data[,c("l.entry", "l.exit")]
				l.temp <- xts(matrix(0, nrow(data), 2), index(data))
				colnames(l.temp) 	<- c("l.orders", "l.qty")
				long <- 1
			}else{
				l.temp <- 0; long <- 0}
			if(!is.null(data$s.entry)){
				if(is.null(data$s.exit)) stop("Short exit not defined")
				s.sig <- data[,c("s.entry", "s.exit")]
				s.temp <- xts(matrix(0, nrow(data), 2), index(data))
				colnames(s.temp) 	<- c("s.orders", "s.qty")
				short <- 1
			}else{s.temp <- 0; short <- 0}
			#######################################################################
			for( t in start:(nrow(data)-1)){
				#######################################################################
				if ( !ttime[t] ){
					if( long ){
						if( l.temp[t,"l.qty"]==0 && l.temp[t,"l.orders"]==0){
							l.temp[t,"l.qty"] <- l.temp[(t-1),"l.qty"] } }
					if( short ){
						if( s.temp[t,"s.qty"]==0 && s.temp[t,"s.orders"]==0){
							s.temp[t,"s.qty"] <- s.temp[(t-1),"s.qty"] } }
					cat("X"); next}
				#######################################################################
				if( long ){
					if( l.temp$l.qty[(t-1)] == 0 ){
				        if( l.sig[t,"l.entry"] ){				        	
				            l.temp[t,"l.orders"] <- qty
							l.temp[t,"l.qty"] <- l.temp[ (t-1), "l.qty"] + qty
							cat("\n", as.character(index(ttime[ t ])), as.numeric(qty), "\n")
				        }else{ l.temp[t,"l.qty"] <- l.temp[(t-1),"l.qty"]}
				    }else if( l.temp[(t-1),"l.qty"] > 0 ){
			            if( l.sig[t,"l.exit"] ){
							l.temp[t,"l.orders"] <- -l.temp[ (t-1), "l.qty"]
							l.temp[t,"l.qty"] <- 0
							cat("\n", as.character(index(ttime[ t ])), as.numeric(-l.temp[ (t-1), "l.qty"]), "\n")
			            }else{ l.temp[t,"l.qty"] <- l.temp[(t-1),"l.qty"]; cat("L") }
			        }else{ cat("wdf?") } }
				#######################################################################
				if( short ){
					if( s.temp[(t-1),"s.qty"] == 0 ){
				        if( s.sig[t,"s.entry"] ){
				            s.temp[t,"s.orders"] <- -qty
							s.temp[t,"s.qty"] <- s.temp[ (t-1), "s.qty"] - qty
							cat("\n", as.character(index(ttime[ t ])), as.numeric(qty), "\n")
				        }else{ s.temp[t,"s.qty"] <- s.temp[(t-1),"s.qty"]}
				    }else if( s.temp[(t-1),"s.qty"] < 0 ){
			            if( s.sig[t,"s.exit"] ){
							s.temp[t,"s.orders"] <- -s.temp[ (t-1), "s.qty"]
							s.temp[t,"s.qty"] <- 0
							cat("\n", as.character(index(ttime[ t ])), as.numeric(-s.temp[ (t-1), "s.qty"]), "\n")
			            }else{ s.temp[t,"s.qty"] <- s.temp[(t-1),"s.qty"]; cat("S") }
			        }else{ cat("wdf?") } }
				#######################################################################
			}; cat("\n")
			#######################################################################
			temp 	<- l.temp + s.temp
			colnames(temp) <- c("orders", "qty")
			data 	<<- merge(data,temp)
			n 		<- nrow(data)
			Pcol 	<- Op(data)
			odata 	<- lag(temp)
			data$pl 	<<- (lag(Pcol,-1)-as.numeric(Pcol))*as.numeric(odata$qty)
			data$pl_tax <<- (data$pl-abs(as.numeric(odata$orders * spread*0.0001/2)))
			data$pl_tax[is.na(data$pl_tax)]<<-0
			data$wealth <<- cumsum(data$pl_tax)+init
			ret 	<- ROC(data$wealth,type="discrete")
			ret[is.na(ret)] <- 0
			data$ret 	<<- ret
			data$windex <<- cumprod(1+ret)
			invisible(data)
		},
		plot = function(...){
			fast_plot(X=data, ...)
		},
		show = function(){
				str(.self)
		} ) )
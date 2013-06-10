strat <- function(params, output = c("table","returns","chart") ){
					for (i in seq_along(params)){ assign(paste('var',i,sep=''),params[i]) }
                    #####################################################################################################################################################
					#####################################################################################################################################################


################# PARAMETERS ########################################################################################################################
#####################################################################################################################################################
init            <- 1e4
norm            <- init*5
wdays           <- c(3,4,5)
dtime           <- "0600/1900"

n_vol           <- var1
tp_vol          <- var2
step            <- var3#0.0005
n_smi           <- var4#21
n_signal        <- var5#30


################# INDICATORS ########################################################################################################################
#####################################################################################################################################################
X$rec       <- rep(0,nrow(X))
X$tday      <- trading_day(X,wdays)
X$ttime     <- trading_time(X,range=dtime)

X$roc       <- ROC(Cl(X),type="discrete")
X$vol       <- volatility(X, n=n_vol)


X$SMI      <- SMI(HLC(X),n=n_smi,nSig=n_signal)$SMI
X$signal    <- SMI(HLC(X),n=n_smi,nSig=n_signal)$signal

################# FRAMEWORK ##########################################################################################################################
######################################################################################################################################################
port_new(X)
helper_1 <- 0
helper_sl <- 0
for( t in 100:(nrow(X)-1)){
    #------------------------------------------------------------------------
    current_date    <- time(X[ t ])
    char_date       <- as.character(current_date)
    tday            <- as.numeric( X[   t  ,"tday"])
    ttime           <- as.numeric( X[   t  ,"ttime"])

    close           <- as.numeric( Cl(X[ t, ]) )
    open            <- as.numeric( Op(X[ (t+1), ]) )
    
    roc             <- as.numeric( X[   t  ,"roc"])    
    vol             <- as.numeric( X[   t  ,"vol"])
    
    SMI            <- as.numeric( X[   t  ,"SMI"])
    signal          <- as.numeric( X[   t  ,"signal"])
    bad_conditions  <- (!tday || !ttime || vol < tp_vol)
    #------------------------------------------------------------------------
    if (helper_1==1 && SMI < signal){
        helper_1 <- 0; 
    }else if (helper_1==2 && SMI > signal){
        helper_1 <- 0; 
    } else if (helper_1!=0){aggreg(t);next}

    if( pos == 0 ) {
        if(bad_conditions){ # conditions not to open
            cat("X")
            helper_sl<-0
        }else{
            if(SMI>signal){
                # long
                order_add(price=close, qty=norm, time=t)
                helper_sl <- close - step
            } else if (SMI<signal){
                # short
                order_add(price=close, qty=-norm, time=t)
                helper_sl <- close + step
            } else {cat("0")}
        }
    } else{
        if (pos > 0){
            if(!bad_conditions && SMI < signal){
                order_add(price=close, qty=-2*pos, time=t)
                helper_sl <- close + step
            }else if(bad_conditions && SMI < signal){
                order_add(price=close, qty=-pos, time=t)
            }else{
                helper_sl <- max(helper_sl, close - step)
                cat("L")
            }

            if((close < helper_sl) && pos >0){
                order_add(price=close, qty=-pos, time=t)
                helper_1 <- 1
            } #1-long(laukia kol kirs is virsaus y apacia)

        } else if(pos <0){
            if (!bad_conditions && SMI > signal){
                order_add(price=close, qty=-2*pos, time=t)
                helper_sl <- close - step
            }else if(bad_conditions && SMI > signal){
                order_add(price=close, qty=-pos, time=t)
            } else{
                helper_sl <- min(helper_sl, close + step)
                cat("L")
            } 

            if ((close > helper_sl) && pos <0){
                order_add(price=close, qty=-pos, time=t)
                helper_1 <- 2
            } #2-short(laukia kol kirs is apacios y virsu)
        }else{cat(char_date, "error0")}
    }
    #------------------------------------------------------------------------
    aggreg(t)
    X[t,"rec"]<-helper_sl
}
augmented   <- finalize(mkdata=mkdata, X=X, pricefun="close", init=init)



					#####################################################################################################################################################
					#####################################################################################################################################################
					output 			<- match.arg(output)
					rets 			<- na.omit(augmented$strat_ret)
					switch(output, table={
						r=prod((1+rets))
						return(c(r, r/apply(rets,2,sd),ddown(rets),params))
					}, returns={
						return(rets)
					}, chart={
						path 	<- '/Users/Edu/Dropbox/public/ttrading/charts/' 
						name 	<- paste("performance_",gsub(" ","_",as.character(Sys.time())),".pdf",sep="")
						full 	<- paste(path,name,sep="")
						pdf(file=full,width = 10, height = 6, family="sans")
						fast_plot(augmented)
						dev.off()
						return(name)
					})
}
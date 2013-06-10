source('http://dl.dropbox.com/u/42748692/ttrading/0_funs_basic.R')
source('http://dl.dropbox.com/u/42748692/ttrading/0_funs_fast.R')


################# DATA ##############################################################################################################################
#####################################################################################################################################################
# data        <- dbase('fx','EURUSD_2012_min.csv')
# ind         <- as.POSIXct(data[ ,1], format="%d.%m.%Y %H:%M:%S")
# data2       <- xts(data[ ,2:5],ind)
final_data  <- data2["20120807/20120809"]
X           <- to.minutes(final_data, OHLC=FALSE)

################# PARAMETERS ########################################################################################################################
#####################################################################################################################################################
vmin            <- .0035
vmax            <- 1
tp1             <- 0.0004
tp2             <- 0.0006
en              <- 0.00036
length_ema      <- 34
length_vol      <- 20

################# INDICATORS ########################################################################################################################
#####################################################################################################################################################
X$ret       <- ROC(Cl(X))
X$sd        <- volatility(X, n=length_vol)
X$rocsd     <- ROC(volatility(X, n=length_vol))
X$ema       <- EMA(Cl(X), length_ema)
X$tday      <- trading_day(X,c(3,4,5))

################# FRAMEWORK ##########################################################################################################################
######################################################################################################################################################
port_new(X)
for( t in 40:(nrow(X)-1)){
    #------------------------------------------------------------------------
    current_date    <- time(X[ t ])
    char_date       <- as.character(current_date)
    close           <- as.numeric( Cl(X[ t, ]) )
    open            <- as.numeric( Op(X[ (t+1), ]) )
    ret             <- as.numeric( X[   t  ,"ret"])
    ret1            <- as.numeric( X[ (t-1),"ret"])
    vol             <- as.numeric( X[   t  , "sd"])
    ema             <- as.numeric( X[   t  ,"ema"])
    tday            <- as.numeric( X[   t  ,"tday"])
    #------------------------------------------------------------------------
    if( pos == 0 ) {
        if ( !tday ){
            cat(char_date, "not rlly a trading day\n")
        }else if(vol > vmax | vol < vmin){
            cat(char_date, "volatility...\n")
        }else{
            if(          ret>en && ret1<0 && close>ema){
                order_add(price=close, qty=+500000, time=t)
            } else if (  ret<en && ret1>0 && close<ema){
                order_add(price=close, qty=-500000, time=t)
            } else {cat(char_date, "entry cond not satisfied\n")}
        }
    } else{
        if (pos > 0){
            if ( !tday | vol > vmax | vol < vmin){
                order_add(price=close, qty=-pos, time=t)
            } else{
                if( pos==250000 && close<ema ){
                    if(       ret>tp2 ){
                        order_add(price=close, qty=-250000, time=t)
                    }else if( ret<-en   ){
                        order_add(price=close, qty=-7500000, time=t)
                    }else{}
                } else if( pos==500000 && close<ema ){
                    if(       ret>tp1 ){
                        order_add(price=close, qty=-250000, time=t)
                    }else if( ret>tp2 ){
                        order_add(price=close, qty=-500000, time=t)
                    }else if( ret<-en   ){
                        order_add(price=close, qty=-1000000, time=t)
                    }else{}
                } else{cat(char_date, "LONG\n")}   
            } 
        } else if(pos <0 ){
            if ( !tday | vol > vmax | vol < vmin){
                order_add(price=close, qty=-pos, time=t)
            } else{
                if( pos==-250000 && close>ema ){
                    if(       ret<-tp2 ){
                        order_add(price=close, qty=+250000, time=t)
                    }else if( ret>en   ){
                        order_add(price=close, qty=+750000, time=t)
                    }else{}
                } else if( pos==-500000 && close>ema ) {
                    if(       ret<-tp1 ){
                        order_add(price=close, qty=+250000, time=t)
                    }else if( ret<-tp2 ){
                order_add(price=close, qty=+500000, time=t)
                    }else if( ret>en   ){
                order_add(price=close, qty=+1000000, time=t)
                    }else{}
                }else{cat(char_date, "SHORT\n")}   
            } 
        }else{cat(char_date, "wdf?")}
    }
    #------------------------------------------------------------------------
    aggreg(t)
}
augmented   <- finalize(mkdata=mkdata, X=X, pricefun="open")

################# ANALYSIS ##########################################################################################################################
#####################################################################################################################################################
fast_plot(augmented) #subset="20120806 04"
addEMA(24)
addVolatility(length_vol)


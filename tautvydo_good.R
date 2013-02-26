source('http://dl.dropbox.com/u/42748692/ttrading/0_funs_basic.R')
source('http://dl.dropbox.com/u/42748692/ttrading/0_funs_fast.R')

################# PARAMETERS ########################################################################################################################
#####################################################################################################################################################
tp1             <- 0.0005
tp2             <- 0.001
length_vol      <- 20

################# DATA ##############################################################################################################################
#####################################################################################################################################################
# data        <- dbase('fx','EURUSD_2012_min.csv')
# ind         <- as.POSIXct(data[ ,1], format="%d.%m.%Y %H:%M:%S")
# data2       <- xts(data[ ,2:5],ind)
final_data  <- data2["20120807/20120809"]
X           <- to.minutes5(final_data, OHLC=TRUE)

################# INDICATORS ########################################################################################################################
#####################################################################################################################################################
X$tday      <- trading_day(X,c(3,4,5))
X$ttime     <- trading_time(X,range="0800/1900")

X$roc       <- ROC(Cl(X))
X$droc      <- ROC(ROC(Cl(X)))

X$vol       <- volatility(X, n=length_vol)
X$ema       <- EMA(Cl(X), length_ema)

################# FRAMEWORK ##########################################################################################################################
######################################################################################################################################################
port_new(X)
for( t in 40:(nrow(X)-1)){
    #------------------------------------------------------------------------
    current_date    <- time(X[ t ])
    char_date       <- as.character(current_date)
    close           <- as.numeric( Cl(X[ t, ]) )
    open            <- as.numeric( Op(X[ (t+1), ]) )
    roc             <- as.numeric( X[   t  ,"roc"])
    roc1            <- as.numeric( X[ (t-1),"roc"])
    vol             <- as.numeric( X[   t  ,"vol"])
    ema             <- as.numeric( X[   t  ,"ema"])
    tday            <- as.numeric( X[   t  ,"tday"])
    ttime           <- as.numeric( X[   t  ,"ttime"])
    #------------------------------------------------------------------------
    if (!tday || !ttime){
        if(pos != 0){
            order_add(price=close, qty=-pos, time=t)
        }else{cat("T")}
    } else{
        if( pos == 0 ) {
                if(roc>0 && roc1<0){
                    order_add(price=close, qty=+5000, time=t)
                } else if (roc<0 && roc1>0){
                    order_add(price=close, qty=-5000, time=t)
                } else {cat("0")}
        } else{
            if (pos > 0){
                if (pos == 5000){
                    if(roc < -tp1 || roc > tp2){
                        order_add(price=close, qty=-pos, time=t)
                    } else if(roc > tp1){
                        order_add(price=close, qty=-pos/2, time=t)
                    } else{cat("L")}   
                } else if(pos == 2500){
                    if(roc < -tp1 || roc > tp2){
                        order_add(price=close, qty=-pos, time=t)
                    } else{cat("L")}   
                } else{cat("error1")}
            } else if(pos <0 ){
                if (pos == -5000){
                    if(roc > tp1 || roc < -tp2){
                        order_add(price=close, qty=-pos, time=t)
                    } else if(roc < tp1){
                        order_add(price=close, qty=-pos/2, time=t)
                    } else{cat("L")}   
                } else if(pos == -2500){
                    if(roc > tp1 || roc < -tp2){
                        order_add(price=close, qty=-pos, time=t)
                    } else{cat("L")}   
                } else{cat("error2")}
            }else{cat(char_date, "error0")}
        }
    }
    #------------------------------------------------------------------------
    aggreg(t)
}
augmented   <- finalize(mkdata=mkdata, X=X, pricefun="close")

################# ANALYSIS ##########################################################################################################################
#####################################################################################################################################################
fast_plot(augmented) #subset="20120806 04"
zoomChart("20120808")

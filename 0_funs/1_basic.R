csv_to_xts <- function(csv, format='%Y-%m-%d'){
    n       <- ncol(csv)
    ind     <- as.POSIXct(csv[ ,1], format=format)
    xts::xts( csv[ ,2:n], ind)}

dbase <- function(dir=0, which, type=c("RDS","xts","csv"), main=db_fol, sep=",", format='%Y-%m-%d'){
    if( dir!=0 ){
        if(is(dir,"numeric")){
            main <- paste0(main,list.files(main)[dir],"/")
        }else if( is(dir,"character") ){
            main <- paste0(main,dir,"/")}}
        listof <- list.files(main)
    if( missing(which) ){
        print( as.matrix(listof) )
    }else{
        type <- match.arg(type)
        if(is(which,"numeric")) which <- listof[which]
        if (type=="RDS"){
            readRDS(paste0(main,which))
        }else if (type=="csv") {
            utils::read.csv(paste0(main,which), stringsAsFactors=FALSE, sep=sep)
        }else if(type=="xts"){
            temp <- read.csv(paste0(main,which), stringsAsFactors=FALSE, sep=sep)
            xts::xts(temp[,2:ncol(temp)], as.POSIXct(data[,1], format=format)) } } }



########################## BASIC ################################################
#################################################################################
na_zero <- function(x){
	x[is.na(x)]<-0;
	return(x)}

inf_zero <- function(x){
	x[x== Inf]<-0;
	x[x==-Inf]<-0
	return(x)}
	
zero_na <- function(x){
	x[x==0]<-NA;
	return(x)}

"%inside%" <- function(a, range){
    if(length(range)!=2){stop("not a range")}
    lower = min(range); 
    upper = max(range);
    (lower<=a & a<=upper)}


########################## TAX ##################################################
#################################################################################

tax_pip <- function(qty, pips=2) {return(abs(qty) * -0.0001 * pips/2)}






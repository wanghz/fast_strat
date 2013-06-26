source("/Users/Edu/core/Projects/fast_strat/0_fstrat_init.R")
data       <- dbase(0, 1)
X          <- to.minutes(data)["20120105"]

#### TEST ########################
stra <- Strat(data=X)
args(stra$ind)
stra$ind(fun=SMA, name="smaf", n=20)
stra$ind(fun=SMA, name="smas", n=50)
stra$signal(name="crossup", type="cross", col1="smaf", col2="smas")
stra$signal(name="crossdown", type="cross", col1="smas", col2="smaf")
stra$l.entry("crossup",1)
stra$l.exit("crossdown",1)
stra$s.entry("crossdown",1)
stra$s.exit("crossup",1)
stra$run()

stra$plot()
addSMA(20)
addSMA(50)

#####################################################################################################################################################
#####################################################################################################################################################
opt.fun <- function(param, chart=FALSE){
	stra <- Strat(data=X)
	stra$sma(param[1], name="smaf")
	stra$sma(param[2], name="smas")
	stra$signal(name="crossup", type="cross", col1="smaf", col2="smas")
	stra$signal(name="crossdown", type="cross", col1="smas", col2="smaf")
	stra$l.entry("crossup",1)
	stra$l.exit("crossdown",1)
	stra$s.entry("crossdown",1)
	stra$s.exit("crossup",1)
	stra$run()

	if(chart){
		path <- '/Users/Edu/Dropbox/public/'; name <- 'perf.pdf'
		pdf(file=paste0(path,name),width = 10, height = 6, family="sans")
		stra$plot(); addPOS(); dev.off()
		return(name)
	}else{ 
		c(as.numeric(stra$data$windex[nrow(out)]),param)
	}
}

#### MAIL #########################
par 			<- expand.grid(seq(5,50,10), seq(50,200,10))
out 			<- t(apply(par, 1, opt.fun))
ord 			<- order(out[ ,1], decreasing = TRUE)
ord_out			<- out[ord,  ]
chart_name 		<- opt.fun(as.numeric(par[ord[1],  ]), chart=TRUE)

#### MAIL #########################
who_get	<- c('<dom.grigonis@gmail.com>')#, '<vjgeko@gmail.com>')
sender 	<- '<ben.bernanke@fed.com>'
title	<- 'Greetings from fed.'
text 	<- paste0('Chart of best performance: ','http://dl.dropbox.com/u/42748692/',chart_name)
attach1	<- mime_part(out)
attach2	<- mime_part(ord_out)
contr 	<- list(smtpServer='ALT2.ASPMX.L.GOOGLE.COM')
# ASPMX.L.GOOGLE.COM, ALT1.ASPMX.L.GOOGLE.COM, ALT2.ASPMX.L.GOOGLE.COM, ASPMX2.GOOGLEMAIL.COM, ASPMX3.GOOGLEMAIL.COM
for (mail in who_get){ sendmail(sender, mail, title, control=contr, body=list(text,attach1,attach2)) }

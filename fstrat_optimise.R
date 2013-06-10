# /usr/bin/Rscript --vanilla -e 'source("http://dl.dropbox.com/u/42748692/projects/TradingStrat/9_executable.R")'& > output.txt
source("/Users/Edu/core/Projects/music_production_R/0_fstrat_init.R")

################# DATA ##############################################################################################################################
#####################################################################################################################################################
data		<- dbase("hf","EURUSD_2012_min.xts.rds","RDS")
X           <- to.minutes5(data, OHLC=TRUE)

################# PARAMETERS ########################################################################################################################
#####################################################################################################################################################
par1 <- 29
par2 <- 0.005
par3 <- 0.0003
par4 <- 14
par5 <- 15

params 	<- as.matrix(expand.grid(par1,par2,par3,par4,par5))

# sharp=ordered_output[,2]>quantile(ordered_output[,2],0.8)
# ret = ordered_output[,1]>quantile(ordered_output[,1],0.8)
# dat1=ordered_output[ sharp & ret,]
# params <- (ordered_output[ ordered_output[,1]>quantile(ordered_output[,1],0.5),])[,4:8]
# params <- ordered_output[1:8,4:8]

################# EXECUTION #########################################################################################################################
#####################################################################################################################################################
output 				<- t(apply(params, 1, strat))
# colnames(output) 	<- c("ret","sharp","dd","vol")
ord 				<- order(output[ ,1], decreasing = TRUE)
ordered_output		<- as.matrix(output[ord,  ])
high 				<- params[ord[1],  ]
chart_path 			<- strat(as.numeric(high), output="chart")
a=microbenchmark(strat(as.numeric(high), output="chart"),strat1(as.numeric(high), output="chart"),times=30)


################# POST #########################################################################################################################
#####################################################################################################################################################
sender 	<- "<ben.bernanke@fed.com>"
who_get	<- c("<dom.grigonis@gmail.com>")#, "<vjgeko@gmail.com>")
title	<- "Greetings from fed."
text 	<- paste('Chart of best performance: ','http://dl.dropbox.com/u/42748692/ttrading/charts/',chart_path,sep='')
attach1	<- mime_part(output)
attach2	<- mime_part(ordered_output)
contr 	<- list(smtpServer="ALT2.ASPMX.L.GOOGLE.COM")

for (mail in who_get){
	sendmailR::sendmail(sender, mail, title, control=contr, 
		body<-list(text,attach1,attach2))}

source("/Users/Edu/core/Projects/fast_strat/0_fstrat_init.R")
data       <- dbase(0, 1)
X          <- to.minutes(data)["201201"]

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
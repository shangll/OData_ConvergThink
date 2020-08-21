library(sft)
# setwd("D:/SyncDocs/MyJob/converg/results")
setwd("//VUW/Personal$/Homes/S/shangl/My Documents/NCKU/converg/results")
dat <- read.table(file = "ORsd3.txt", head = T)
attach(dat)

rt1 <- dat[Condition=="OR" & Channel1==1 & Channel2==0,"RT"]
rt2 <- dat[Condition=="OR" & Channel1==0 & Channel2==1,"RT"]
rt12 <- dat[Condition=="OR" & Channel1==1 & Channel2==1,"RT"]
#RT.and <- pmax(rt1,rt2,rt12)
tvec <- sort(unique(c(rt1,rt2,rt12)))

cr1 <- dat[Condition=="OR" & Channel1==1 & Channel2==0,"Correct"]
cr2 <- dat[Condition=="OR" & Channel1==0 & Channel2==1,"Correct"]
cr12 <- dat[Condition=="OR" & Channel1==1 & Channel2==1,"Correct"]
capand <- capacity.or(list(rt12,rt1,rt2),list(cr12,cr1,cr2),ratio=TRUE)
or.bounds <- estimate.bounds(list(rt1,rt2),list(cr1,cr2),
                              stopping.rule="OR",unified.space=FALSE)

xy=par("usr")
windows()
par(oma=c(0,0,0,0), mar=c(8,4.1,3,2.1))
plot(tvec,ecdf(rt12)(tvec),bty="l",type="l",col="red",lty=1,lwd=3,
     xlim=c(1000,65000),ylim=c(0,1),
     xlab="Time(ms)",ylab="P(T<t)")
#lines(tvec,ecdf(rt2)(tvec),type="l",col="green",lty=1,lwd=2)
#lines(tvec,ecdf(rt1)(tvec),type="l",col="red",lty=1,lwd=3)

lines(tvec,or.bounds$Upper.Bound(tvec),col="blue",lty=3,lwd=2)
lines(tvec,or.bounds$Lower.Bound(tvec),col="blue",lty=4,lwd=2)

legend(x=xy[1]+xinch(0),y=xy[1]-yinch(1.2),
       c("C-V Upper Bound","C-V Lower Bound","Redundant-Cue"),
       lty=c(3,4,1),col=c("blue","blue","red"),
       lwd=2,x.intersp=0.5,cex=0.9,bty="o",xpd=TRUE,horiz=TRUE)

detach(dat)
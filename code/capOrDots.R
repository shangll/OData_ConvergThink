library(sft)
setwd("D:/SyncDocs/MyJob/Converg/manuscript_JEPG/data")
#setwd("C:/Users/U759254/SyncDocs/MyJob/converg/manuscript_JEPG/data")
dat <- read.table("ORsd3.txt", header=T)
attach(dat)

totaldat.OR <- subset(dat, dat$Condition == "OR" & dat$Correct == 1)
totalHH.OR <- subset(totaldat.OR,
                     totaldat.OR$Channel1 == 1 & totaldat.OR$Channel2 == 1)
totalHX.OR <- subset(totaldat.OR,
                     totaldat.OR$Channel1 == 1 & totaldat.OR$Channel2 == 0)
totalXH.OR <- subset(totaldat.OR,
                     totaldat.OR$Channel1 == 0 & totaldat.OR$Channel2 == 1)
RT.HH.totalOR <- totalHH.OR$RT
RT.HX.totalOR <- totalHX.OR$RT
RT.XH.totalOR <- totalXH.OR$RT
capAll.res <- capacity.or(RT=list(RT.HH.totalOR,RT.HX.totalOR,RT.XH.totalOR))
tvecAll <- sort(unique(c(RT.HX.totalOR, RT.XH.totalOR, RT.HH.totalOR)))

#
cr1 <- dat[Condition=="OR" & Channel1==1 & Channel2==0,"Correct"]
cr2 <- dat[Condition=="OR" & Channel1==0 & Channel2==1,"Correct"]
bounds <- estimate.bounds(list(RT.HX.totalOR, RT.XH.totalOR),list(cr1,cr2),
                          stopping.rule="OR", unified.space=TRUE)
#

xy=par("usr")
windows()
par(oma=c(0,0,0,0), mar=c(9,4.1,3,2.1))
plot(tvecAll[tvecAll>=350],capAll.res$Ct(tvecAll[tvecAll>=350]),bty="l",type="l",family="Arial",
     xlim=c(350,75000),ylim=c(0,3.5),xlab="Time(ms)",ylab="C(t)")

for (n in 1:45){
  dat.OR <- subset(totaldat.OR,Subject==n)
  HH.OR <- subset(dat.OR, dat.OR$Channel1 == 1 & dat.OR$Channel2 == 1)
  HX.OR <- subset(dat.OR, dat.OR$Channel1 == 1 & dat.OR$Channel2 == 0)
  XH.OR <- subset(dat.OR, dat.OR$Channel1 == 0 & dat.OR$Channel2 == 1)
  RT.HH.OR <- HH.OR$RT
  RT.HX.OR <- HX.OR$RT
  RT.XH.OR <- XH.OR$RT
  capor.res <- capacity.or(RT=list(RT.HH.OR,RT.HX.OR,RT.XH.OR))
  tvec <- sort(unique(c(RT.HX.OR, RT.XH.OR, RT.HH.OR)))
  points(tvec[tvecAll>=350],capor.res$Ct(tvec[tvecAll>=350]),pch=20,cex=0.75,col="ivory4")
  #lines(tvec,capor.res$Ct(tvec),lwd=1.75,lty=1,col="#33CC66")
}

lines(tvecAll[tvecAll>=350], bounds$Lower.Bound(tvecAll[tvecAll>=350]),col="blue",lty=4,lwd=3)
lines(tvecAll[tvecAll>=350],capAll.res$Ct(tvecAll[tvecAll>=350]),lwd=3,col="black")
abline(h=1,col="red",lwd=2.5,lty=2)

lines(tvecAll[tvecAll>=350], bounds$Upper.Bound(tvecAll[tvecAll>=350]),col="blue",lty=3,lwd=3)

legend(x=xy[1]+xinch(0.5),y=xy[1]-yinch(0.9),
       c("Race-Model Bound","Grice Bound",
         "Capacity Coefficient","Reference Line"),
       lty=c(3,4,1,2),col=c("blue","blue","black","red"),
       lwd=2,x.intersp=0.5,cex=1,bty="o",ncol=2,xpd=TRUE,horiz=F)

detach(dat)



library(sft)
setwd("D:/SyncDocs/MyJob/converg/results")
dat <- read.table(file = "ANDsd3.txt", head = T)
attach(dat)
#cap <- read.table(file = "captype.txt", head = T)

totalAnd <- subset(dat, dat$Condition == "AND" & dat$Correct == 1)
totalAnd.HH <- subset(totalAnd,
                      totalAnd$Channel1 == 1 & totalAnd$Channel2 == 1)
totalAnd.HX <- subset(totalAnd,
                      totalAnd$Channel1 == 1 & totalAnd$Channel2 == 0)
totalAnd.XH <- subset(totalAnd,
                      totalAnd$Channel1 == 0 & totalAnd$Channel2 == 1)
RT.HH.totalAnd <- totalAnd.HH$RT
RT.HX.totalAnd <- totalAnd.HX$RT
RT.XH.totalAnd <- totalAnd.XH$RT
capandAll.res <- capacity.and(
  RT=list(RT.HH.totalAnd,RT.HX.totalAnd,RT.XH.totalAnd))
tvecAll <- sort(unique(c(RT.HX.totalAnd, RT.XH.totalAnd, RT.HH.totalAnd)))

#
cr1 <- dat[Condition=="AND" & Channel1==1 & Channel2==0,"Correct"]
cr2 <- dat[Condition=="AND" & Channel1==0 & Channel2==1,"Correct"]
cr12 <- dat[Condition=="AND" & Channel1==1 & Channel2==1,"Correct"]

bounds <- estimate.bounds(list(RT.HX.totalAnd, RT.XH.totalAnd),list(cr1,cr2),
                          stopping.rule="AND", unified.space=TRUE)
CRlist <- list(cr12, cr1, cr2)
#
z.super <- ucip.test(RT=list(RT.HH.totalAnd,RT.HX.totalAnd,RT.XH.totalAnd),
                    CR=CRlist,stopping.rule="AND")

###################

xy=par("usr")
windows()
par(oma=c(0,0,0,0), mar=c(9,4.1,3,2.1))
plot(tvecAll,capandAll.res$Ct(tvecAll),bty="l",type="l",family="Arial",
     xlim=c(500,60000),ylim=c(0,2.5),xlab="Time(ms)",ylab="C(t)")

ord <-c(1:3,5:39,41:46)
for (n in ord){
  dat.AND <- subset(totalAnd,Subject==n)
  HH.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 1)
  HX.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 0)
  XH.AND <- subset(dat.AND, dat.AND$Channel1 == 0 & dat.AND$Channel2 == 1)
  RT.HH.AND <- HH.AND$RT
  RT.HX.AND <- HX.AND$RT
  RT.XH.AND <- XH.AND$RT
  tvec <- sort(unique(c(RT.HX.AND, RT.XH.AND, RT.HH.AND)))
  capand.res <- capacity.and(RT=list(RT.HH.AND,RT.HX.AND,RT.XH.AND))

  points(tvec,capand.res$Ct(tvec),pch=20,cex=0.75,col="ivory4")
  #lines(tvec,capand.res$Ct(tvec),lwd=2.75,lty=1,col=clr)
}

lines(tvecAll,capandAll.res$Ct(tvecAll),lwd=3,col="black")
abline(h=1,col="red",lwd=2.5,lty=2)

lines(tvecAll, bounds$Upper.Bound(tvecAll), lty=3, col="blue", lwd=3)
lines(tvecAll, bounds$Lower.Bound(tvecAll), lty=4, col="blue", lwd=3)
#

legend(x=xy[2]+xinch(0.5),y=xy[4]-yinch(2.75),
       c("C-V Upper Bound","C-V Lower Bound",
         "Capacity Coefficient","Reference Line"),
       lty=c(3,4,1,2),col=c("blue","blue","black","red"),
       lwd=2,x.intersp=0.5,cex=1,bty="o",ncol=2,xpd=TRUE,horiz=F)

detach(dat)


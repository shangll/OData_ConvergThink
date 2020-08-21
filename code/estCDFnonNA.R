library(sft)
setwd("D:/SyncDocs/MyJob/converg/results")
dat.AND <- read.table("ANDsd3.txt", header=T)
dat <- subset(dat.AND, dat.AND$Correct == 1)
attach(dat)

HX <- dat[Condition=="AND" & Channel1==1 & Channel2==0,"RT"]
XH <- dat[Condition=="AND" & Channel1==0 & Channel2==1,"RT"]
HH <- dat[Condition=="AND" & Channel1==1 & Channel2==1,"RT"]
HX <- sort(unique(HX))
XH <- sort(unique(XH))
HH <- sort(unique(HH))
len1 <- length(HX)
len2 <- length(XH)
len12 <- length(HH)
shortLen <- sort(c(len1,len2,len12))[1]

rt1 <- log(ecdf(HX)(HX))[1:shortLen]
rt2 <- log(ecdf(XH)(XH))[1:shortLen]
rt12 <- log(ecdf(HH)(HH))[1:shortLen]

tvec <- sort(unique(c(HX,XH,HH)))

cr1 <- dat[Condition=="AND" & Channel1==1 & Channel2==0,"Correct"]
cr2 <- dat[Condition=="AND" & Channel1==0 & Channel2==1,"Correct"]
cr12 <- dat[Condition=="AND" & Channel1==1 & Channel2==1,"Correct"]
cr1 <- cr1[1:shortLen]
cr2 <- cr2[1:shortLen]

and.bounds <- estimate.bounds(list(HX,XH),list(cr1,cr2),
                              stopping.rule="AND",unified.space=FALSE)
xy=par("usr")
windows()
par(oma=c(0,0,0,0), mar=c(8,4.1,3,2.1))
plot(tvec,ecdf(HH)(tvec),bty="l",type="l",lty=1,lwd=2.5,col="red",
     xlim=c(1000,65000),ylim=c(0,1),
     xlab="Time(ms)",ylab="P(T<t)")

lines(tvec,and.bounds$Upper.Bound(tvec),col="blue",lty=3,lwd=2)
lines(tvec,and.bounds$Lower.Bound(tvec),col="blue",lty=4,lwd=2)

legend(x=xy[2]+xinch(0),y=xy[4]-yinch(5.3),
       c("C-V Upper Bound","C-V Lower Bound","Redundant-Cue"),
       lty=c(3,4,1),col=c("blue","blue","red"),
       lwd=2,x.intersp=0.5,cex=0.9,bty="o",xpd=TRUE,horiz=TRUE)


detach(dat)



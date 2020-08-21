library(sft)
setwd("D:/SyncDocs/MyJob/converg/results")
dat.AND <- read.table("ANDsd3.txt", header=T)

dat <- subset(dat.AND, dat.AND$Condition == "AND" & dat.AND$Correct == 1)
attach(dat)

HX <- dat[Condition=="AND" & Channel1==1 & Channel2==0,"RT"]
XH <- dat[Condition=="AND" & Channel1==0 & Channel2==1,"RT"]
HH <- dat[Condition=="AND" & Channel1==1 & Channel2==1,"RT"]
rt <- list(HX,XH,HH)
tvec <- sort(unique(c(HX,XH,HH)))
tm <- sort(unique(c(rt,recursive = TRUE)))
nc <- length(rt) - 1
cr <- vector("list", length(rt))
for (i in 1:length(rt)) {cr[[i]] <- rep(1, length(rt[[i]]))}
denomand <- estimateNAK(rt[[1]], cr[[1]])
numerand <- estimateUCIPand(RT = rt[1+(1:nc)], CR = cr[1+(1:nc)])
andct <- numerand$K(tm)/denomand$K(tm)
andct[is.nan(andct)] <- NA
andct[is.infinite(andct)] <- NA
andct[andct==0] <- NA
naset <- which(is.na(andct))
andctdel <- andct[-naset]
tm <- tm[-naset]
ctfinal <- approxfun(tm, andctdel)
rt <- rt[-naset]
cr <- cr[-naset]

k <- floor(length(rt)/3)
rt1 <- rt[1:k]
rt2 <- rt[k+1:k*2]
rt12 <- rt[k*2+1:k*3]
tvec <- sort(unique(c(rt1,rt2,rt12)))
and.bounds <- estimate.bounds(list(rt1,rt2),list(cr1[1:length(rt1)],cr2[1:length(rt2)]),
                              stopping.rule="AND",unified.space=FALSE)
xy=par("usr")
windows()
par(oma=c(0,0,0,0), mar=c(8,4.1,3,2.1))
plot(tvec,ecdf(rt12)(tvec),bty="l",type="l",lty=1,lwd=2.5,col="red",
     xlim=c(1000,65000),ylim=c(0,1),
     xlab="Time(ms)",ylab="P(T<t)")

lines(tvec,and.bounds$Upper.Bound(tvec),col="blue",lty=3,lwd=2)
lines(tvec,and.bounds$Lower.Bound(tvec),col="blue",lty=4,lwd=2)

legend(x=xy[2]+xinch(0),y=xy[4]-yinch(5.3),
       c("C-V Upper Bound","C-V Lower Bound","Redundant-Cue"),
       lty=c(3,4,1),col=c("blue","blue","red"),
       lwd=2,x.intersp=0.5,cex=0.9,bty="o",xpd=TRUE,horiz=TRUE)


detach(dat)
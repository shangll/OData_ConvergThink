library(sft)
# setwd("D:/SyncDocs/MyJob/converg/results")
setwd("//VUW/Personal$/Homes/S/shangl/My Documents/NCKU/converg/results")
dat <- read.table("ANDsd3.txt", header=T)
#dat <- read.table(file = "ANDsd3rep.txt", head = T)
attach(dat)

cr1 <- dat[Condition=="AND" & Channel1==1 & Channel2==0,"Correct"]
cr2 <- dat[Condition=="AND" & Channel1==0 & Channel2==1,"Correct"]
cr12 <- dat[Condition=="AND" & Channel1==1 & Channel2==1,"Correct"]
rt1 <- dat[Condition=="AND" & Channel1==1 & Channel2==0,"RT"]
rt2 <- dat[Condition=="AND" & Channel1==0 & Channel2==1,"RT"]
rt12 <- dat[Condition=="AND" & Channel1==1 & Channel2==1,"RT"]
#RT.and <- pmax(rt1,rt2,rt12)
tvec <- sort(unique(c(rt1[rt1<=60000],rt2[rt2<=60000],rt12[rt12<=60000])))

######
rt <- list(rt12,rt1,rt2)
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
andct <- andct[-naset]
tm <- tm[-naset]
ctfinal <- approxfun(tm, andct)

rt <- rt[-naset]
cr <- cr[-naset]

tvec <- tvec[-naset]
k <- floor(length(tvec)/3)
rt1 <- tvec[1:k]
rt2 <- tvec[k+1:k*2]
rt12 <- tvec[k*2+1:k*3]
and.bounds <- estimate.bounds(list(rt1,rt2),list(cr1[1:length(rt1)],cr2[1:length(rt2)]),
                              stopping.rule="AND",unified.space=FALSE)
xy=par("usr")
#windows()
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



######
capand <- capacity.and(list(rt12,rt1,rt2),list(cr12,cr1,cr2),ratio=TRUE)
and.bounds <- estimate.bounds(list(rt1,rt2),list(cr1,cr2),
                              stopping.rule="AND",unified.space=FALSE)

windows()
#par(mfrow=c(1,2))
plot(tvec[tvec<=60000],ecdf(rt1)(tvec[tvec<=60000]),type="l",lty=1,lwd=3,
     xlim=c(1000,65000),ylim=c(0,1),
     xlab="Time(ms)",ylab="P(T<t)",
     main="Cumulative Distribution Functions (AND)")
lines(tvec[tvec<=60000],ecdf(rt2)(tvec[tvec<=60000]),type="l",col="green",lty=1,lwd=2)
lines(tvec[tvec<=60000],ecdf(rt12)(tvec[tvec<=60000]),type="l",col="red",lty=1,lwd=3)

lines(tvec[tvec<=60000],and.bounds$Upper.Bound(tvec[tvec<=60000]),col="blue",lty=3,lwd=2)
lines(tvec[tvec<=60000],and.bounds$Lower.Bound(tvec[tvec<=60000]),col="blue",lty=4,lwd=2)

legend('bottomright',c("C-V Upper Bound","C-V Lower Bound","Left-Cue","Right-Cue","Redundant-Cue"),
       lty=c(4,4,1,1,1),col=c("blue","blue","black","green","red"),
       lwd=2,bty="n")

#plot(tvec,capand$Ct(tvec), type="l",lty=1,lwd=3,
#     xlim=c(500,65000),
#     xlab="Time(ms)",ylab="C(t)",main="AND Capacity")
#lines(tvec,and.bounds$Upper.Bound(tvec),lty=3,col="blue",lwd=3)
#lines(tvec,and.bounds$Lower.Bound(tvec),lty=4,col="blue",lwd=3)
#abline(h=1,col="red",lty=2)
#legend('topleft',c("C-V Upper Bound","C-V Lower Bound",
#                   "Capacity Coefficient","Reference Line"),
#       lty=c(3,4,1,2),col=c("blue","blue","black","red"),lwd=2,bty="n")

detach(dat)
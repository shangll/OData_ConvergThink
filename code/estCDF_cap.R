library(sft)
# setwd("D:/SyncDocs/MyJob/converg/results")
setwd("//VUW/Personal$/Homes/S/shangl/My Documents/NCKU/converg/results")
dat <- read.table("ANDsd3.txt", header=T)
#dat <- read.table(file = "ANDsd3rep.txt", head = T)
attach(dat)

ord <-c(1:3,5:39,41:46)

for (n in ord)
{
rt1 <- dat[Subject==n & Condition=="AND" & Channel1==1 & Channel2==0,"RT"]
rt2 <- dat[Subject==n & Condition=="AND" & Channel1==0 & Channel2==1,"RT"]
rt12 <- dat[Subject==n & Condition=="AND" & Channel1==1 & Channel2==1,"RT"]
#RT.and <- pmax(rt1,rt2,rt12)
tvec <- sort(unique(c(rt1,rt2,rt12)))

cr1 <- dat[Subject==n & Condition=="AND" & Channel1==1 & Channel2==0,"Correct"]
cr2 <- dat[Subject==n & Condition=="AND" & Channel1==0 & Channel2==1,"Correct"]
cr12 <- dat[Subject==n & Condition=="AND" & Channel1==1 & Channel2==1,"Correct"]
capand <- capacity.and(list(rt12,rt1,rt2),list(cr12,cr1,cr2),ratio=TRUE)
and.bounds <- estimate.bounds(list(rt1,rt2),list(cr1,cr2),
                              stopping.rule="AND",unified.space=FALSE)

subj <- as.character(n)
#windows()
paraFig <- paste("D:\\Usr\\Documents\\NCKU\\estiFigCDF\\",subj,".png",sep = "",collapse="")
png(paraFig,width=1366,height=685)

par(mfrow=c(1,2))
plot(tvec,ecdf(rt1)(tvec),type="l",lty=1,lwd=3,
     xlim=c(1000,65000),ylim=c(0,1),
     xlab="Time(ms)",ylab="P(T<t)",
     main=paste("CDF subject ",subj))
lines(tvec,ecdf(rt2)(tvec),type="l",col="green",lty=1,lwd=2)
lines(tvec,ecdf(rt12)(tvec),type="l",col="red",lty=1,lwd=3)

#lines(tvec,and.bounds$Upper.Bound(tvec),type="l",col="blue",lty=3,lwd=2)
lines(tvec,and.bounds$Lower.Bound(tvec),type="l",col="blue",lty=4,lwd=2)

legend('bottomright',c("C-V Lower Bound","Left-Cue","Right-Cue","Redundant-Cue"),
       lty=c(4,1,1,1),col=c("blue","black","green","red"),
       lwd=2,bty="n")

plot(tvec,capand$Ct(tvec), type="l",lty=1,lwd=3,
     xlim=c(500,65000),
     xlab="Time(ms)",ylab="C(t)",main="AND Capacity")
lines(tvec,and.bounds$Upper.Bound(tvec),lty=3,col="blue",lwd=3)
lines(tvec,and.bounds$Lower.Bound(tvec),lty=4,col="blue",lwd=3)
abline(h=1,col="red")
legend('topleft',c("C-V Upper Bound","C-V Lower Bound",
                   "Capacity Coefficient","Reference Line"),
       lty=c(3,4,1,2),col=c("blue","blue","black","red"),lwd=2,bty="n")
dev.off()
}
detach(dat)
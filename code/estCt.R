library(sft)
setwd("D:\\Usr\\Documents\\NCKU\\new")
dat <- read.table("AndDelFinal.txt", header=T)
dat.AND <- subset(dat, dat$Condition == "AND" & dat$Correct == 1)
data(dat.AND)
attach(dat.AND)

ord <- c(1:3,5:39,41:46)

for (n in ord){
RT.A <- dat.AND[Subject==n & Condition=='AND' &
               Channel1==1 & Channel2==0, 'RT']
RT.B <- dat.AND[Subject==n & Condition=='AND' &
               Channel1==0 & Channel2==1, 'RT']
RT.AB <- dat.AND[Subject==n & Condition=='AND' &
                Channel1==1 & Channel2==1, 'RT']
tvec <- sort(unique(c(RT.A, RT.B, RT.AB)))
Cand.A <- dat.AND[Subject==n & Condition=='AND' &
                 Channel1==1 & Channel2==0, 'Correct']
Cand.B <- dat.AND[Subject==n & Condition=='AND' &
                 Channel1==0 & Channel2==1, 'Correct']
Cand.AB <- dat.AND[Subject==n & Condition=='AND' &
                  Channel1==1 & Channel2==1, 'Correct']
capacity <- capacity.and(list(RT.AB,RT.A,RT.B), 
                         list(Cand.AB,Cand.A,Cand.B), ratio=TRUE)
bounds <- estimate.bounds(list(RT.A,RT.B), list(Cand.A,Cand.B), 
                          stopping.rule="AND",
                          unified.space=TRUE)
#plot unified capacity coefficient space
subj <- as.character(n)
windows()
paraFig <- paste("D:\\Usr\\Documents\\NCKU\\estiFigCap\\",subj,".png",sep = "",collapse="")
png(paraFig)
plot(tvec, capacity$Ct(tvec),type="l",lty=1,lwd=2,main=paste("subject ",subj),xlab = "Time",ylab="C(t)")
lines(tvec, bounds$Upper.Bound(tvec), lty=2, col="grey", lwd=2)
lines(tvec, bounds$Lower.Bound(tvec), lty=4, col="grey", lwd=2)
abline(h=0, col="red", lty=1)

dev.off()
}
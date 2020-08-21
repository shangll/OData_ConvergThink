library(sft)
setwd("D:/Usr/Documents/NCKU/new")
dat <- read.table(file = "AndDel005.txt", head = T)
cap <- read.table(file = "captype.txt", head = F)

totalAnd <- subset(dat, dat$Condition == "AND" & dat$Correct == 1)

totalAnd.HH <- subset(totalAnd, totalAnd$Channel1 == 1 & totalAnd$Channel2 == 1)
totalAnd.HX <- subset(totalAnd, totalAnd$Channel1 == 1 & totalAnd$Channel2 == 0)
totalAnd.XH <- subset(totalAnd, totalAnd$Channel1 == 0 & totalAnd$Channel2 == 1)
RT.HH.totalAnd <- totalAnd.HH$RT
RT.HX.totalAnd <- totalAnd.HX$RT
RT.XH.totalAnd <- totalAnd.XH$RT
capandAll.res <- capacity.and(RT=list(RT.HH.totalAnd,RT.HX.totalAnd,RT.XH.totalAnd))
tvecAll <- sort(unique(c(RT.HX.totalAnd, RT.XH.totalAnd, RT.HH.totalAnd)))

windows()
#ggplot2()
plot(tvecAll,capandAll.res$Ct(tvecAll),type="l",xlim=c(0,225000),ylim=c(0,4),
     xlab="Time(ms)",ylab="C(t)")
ord <-c(1:6,8:39,41:46)
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
  
  capclr <- cap[n,3]
  if (capclr=="Limited"){
    clr <- "green"
  }
  if (capclr=="Nonsignificant"){
    clr <- "blue"
  }
  if (capclr=="Super"){
    clr <- "orange"
  }
  
  points(tvec,capand.res$Ct(tvec),pch=20,cex=0.75,col=clr)
}
lines(tvecAll,capandAll.res$Ct(tvecAll),lwd=3)
abline(1,0)


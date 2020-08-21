library(sft)
# setwd("D:/SyncDocs/MyJob/converg/results")
setwd("//VUW/Personal$/Homes/S/shangl/My Documents/NCKU/converg/results")
datAll <- read.table(file = "ANDsd3.txt", head = T)

datCRAT <- read.table(file = "ANDresults.txt", head = T)
crat1 <- quantile(datCRAT$CRAT,.25)
crat3 <- quantile(datCRAT$CRAT,.75)
datlow <- subset(datCRAT, datCRAT$CRAT <= crat1)
dathigh <- subset(datCRAT, datCRAT$CRAT >= crat3)
lowSubj <-datlow$Subject
higSubj <- dathigh$Subject

dathi <- subset(datAll, datAll$Subject %in% higSubj)
datlo <- subset(datAll, datAll$Subject %in% lowSubj)

totalAndhi <- subset(dathi,dathi$Condition=="AND" & dathi$Correct==1)
totalAndhi.HH <- subset(totalAndhi, totalAndhi$Channel1 == 1 & totalAndhi$Channel2 == 1)
totalAndhi.HX <- subset(totalAndhi, totalAndhi$Channel1 == 1 & totalAndhi$Channel2 == 0)
totalAndhi.XH <- subset(totalAndhi, totalAndhi$Channel1 ==0 & totalAndhi$Channel2 == 1)
RT.HH.totalAndhi <- totalAndhi.HH$RT
RT.HX.totalAndhi <- totalAndhi.HX$RT
RT.XH.totalAndhi <- totalAndhi.XH$RT
capandAllhi.res <- capacity.and(RT=list(RT.HH.totalAndhi,RT.HX.totalAndhi,RT.XH.totalAndhi))
tvecAllhi <- sort(unique(c(RT.HX.totalAndhi, RT.XH.totalAndhi, RT.HH.totalAndhi)))

windows()
par(mfrow=c(1,2))

# high-CRAT
plot(tvecAllhi,capandAllhi.res$Ct(tvecAllhi),type="l",xlim=c(500,120000),ylim=c(0,2.5),
     xlab="Time(ms)",ylab="C(t)",main="High-CRAT Group Capacity")
for (n in higSubj){
  dat.ANDhi <- subset(totalAndhi,Subject==n)
  HH.ANDhi <- subset(dat.ANDhi, dat.ANDhi$Channel1 == 1 & dat.ANDhi$Channel2 == 1)
  HX.ANDhi <- subset(dat.ANDhi, dat.ANDhi$Channel1 == 1 & dat.ANDhi$Channel2 == 0)
  XH.ANDhi <- subset(dat.ANDhi, dat.ANDhi$Channel1 == 0 & dat.ANDhi$Channel2 == 1)
  RT.HH.ANDhi <- HH.ANDhi$RT
  RT.HX.ANDhi <- HX.ANDhi$RT
  RT.XH.ANDhi <- XH.ANDhi$RT
  tvechi <- sort(unique(c(RT.HX.ANDhi, RT.XH.ANDhi, RT.HH.ANDhi)))
  capandhi.res <- capacity.and(RT=list(RT.HH.ANDhi,RT.HX.ANDhi,RT.XH.ANDhi))
  
  points(tvechi,capandhi.res$Ct(tvechi),pch=20,cex=1)
}
lines(tvecAllhi,capandAllhi.res$Ct(tvecAllhi),lwd=3,col="blue")
abline(h=1,col="red",lwd=3,lty=2)

# low-CRAT
totalAndlo <- subset(datlo,datlo$Condition=="AND"&datlo$Correct==1)
totalAndlo.HH <- subset(totalAndlo, totalAndlo$Channel1 == 1 & totalAndlo$Channel2 == 1)
totalAndlo.HX <- subset(totalAndlo, totalAndlo$Channel1 == 1 & totalAndlo$Channel2 == 0)
totalAndlo.XH <- subset(totalAndlo, totalAndlo$Channel1 == 0 & totalAndlo$Channel2 == 1)
RT.HH.totalAndlo <- totalAndlo.HH$RT
RT.HX.totalAndlo <- totalAndlo.HX$RT
RT.XH.totalAndlo <- totalAndlo.XH$RT
capandAlllo.res <- capacity.and(RT=list(RT.HH.totalAndlo,RT.HX.totalAndlo,RT.XH.totalAndlo))
tvecAlllo <- sort(unique(c(RT.HX.totalAndlo, RT.XH.totalAndlo, RT.HH.totalAndlo)))

plot(tvecAlllo,capandAlllo.res$Ct(tvecAlllo),type="l",xlim=c(500,120000),ylim=c(0,2.5),
     xlab="Time(ms)",ylab="C(t)",main="Low-CRAT Group Capacity")
for (k in lowSubj){
  dat.ANDlo <- subset(totalAndlo,Subject==k)
  HH.ANDlo <- subset(dat.ANDlo, dat.ANDlo$Channel1 == 1 & dat.ANDlo$Channel2 == 1)
  HX.ANDlo <- subset(dat.ANDlo, dat.ANDlo$Channel1 == 1 & dat.ANDlo$Channel2 == 0)
  XH.ANDlo <- subset(dat.ANDlo, dat.ANDlo$Channel1 == 0 & dat.ANDlo$Channel2 == 1)
  RT.HH.ANDlo <- HH.ANDlo$RT
  RT.HX.ANDlo <- HX.ANDlo$RT
  RT.XH.ANDlo <- XH.ANDlo$RT
  tveclo <- sort(unique(c(RT.HX.ANDlo, RT.XH.ANDlo, RT.HH.ANDlo)))
  capandlo.res <- capacity.and(RT=list(RT.HH.ANDlo,RT.HX.ANDlo,RT.XH.ANDlo))
  
  points(tveclo,capandlo.res$Ct(tveclo),pch=20,cex=1)
}
lines(tvecAlllo,capandAlllo.res$Ct(tvecAlllo),lwd=3,col="blue")
abline(h=1,col="red",lwd=3,lty=2)



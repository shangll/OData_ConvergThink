library(sft)

setwd("D:\\Usr\\Documents\\NCKU\\new")
datAll <- read.table("Sig2.5ANDdel.txt", header=T)
dat.AND <- subset(datAll, datAll$Condition == "AND" & datAll$Correct == 1)

ord <-c(1:3,5:32,34:35,37:39,42:46)

for (n in ord){
  dat <- subset(dat.AND, dat.AND$Subject == n)
  
  HH <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  HX <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 0)
  XH <- subset(dat, dat$Channel1 == 0 & dat$Channel2 == 1)
  
  RT.HH <- HH$RT
  RT.HX <- HX$RT
  RT.XH <- XH$RT
  
  # PDF
  RT.HH.PDF <- density(RT.HH)
  RT.HX.PDF <- density(RT.HX)
  RT.XH.PDF <- density(RT.XH)
  # CDF
  RT.HH.CDF <- ecdf(RT.HH)
  RT.HX.CDF <- ecdf(RT.HX)
  RT.XH.CDF <- ecdf(RT.XH)
  
}

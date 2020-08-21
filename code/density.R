setwd("D:\\SL_NCKU\\new")
dat <- read.table(file = "andDelOrder.txt", head = T)

windows()
dat.AND = subset(dat, dat$Subject == 1 & dat$Condition == "AND" & dat$Correct == 1)

HH.AND = subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 1)
HX.AND = subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 0)
XH.AND = subset(dat.AND, dat.AND$Channel1 == 0 & dat.AND$Channel2 == 1)

RT.HX.AND = HX.AND$RT
RT.XH.AND = XH.AND$RT

RT.X = c(RT.HX.AND,RT.XH.AND)
RT.H = HH.AND$RT
#if (i == 1 || i == 10 || i == 19 || i == 28 || i == 37)
#{
  #windows()
  #par(mfrow=c(3,6))
#}
s = seq(0, 50000, by = 5000)
#hist(RT.X, breaks = s, freq = FALSE)
plot(density(RT.X), main = " ", xlim=c(0,50000),ylim=c(0,0.001), ylab="Density", xlab="RT", col="blue", lwd=1.5)
lines(density(RT.H), col="red", lwd=1.5)
#plot(density(RT.H), main = " ", xlim=c(0,50000),ylim=c(0,0.001), ylab="Density", xlab="RT", col="red", lwd=1)
for (i in 2:39)
{
  dat.AND = subset(dat, dat$Subject == i & dat$Condition == "AND" & dat$Correct == 1)
  
  HH.AND = subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 1)
  HX.AND = subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 0)
  XH.AND = subset(dat.AND, dat.AND$Channel1 == 0 & dat.AND$Channel2 == 1)
  
  RT.HX.AND = HX.AND$RT
  RT.XH.AND = XH.AND$RT
  
  RT.X = c(RT.HX.AND,RT.XH.AND)
  RT.H = HH.AND$RT
  #if (i == 1 || i == 10 || i == 19 || i == 28 || i == 37)
  #{
    #windows()
    #par(mfrow=c(3,6))
  #}
  s = seq(0, 50000, by = 5000)
  #hist(RT.X, breaks = s, freq = FALSE)
  #plot(density(RT.X), col="blue", lwd=3)
  lines(density(RT.X), col="blue", lwd=1.5)
  #d = seq(0, 125000, by = 12500)
  #hist(RT.H, breaks = d, freq = FALSE)
  lines(density(RT.H), col="red", lwd=1.5)
}

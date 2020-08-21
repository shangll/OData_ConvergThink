setwd("D:\\SL_NCKU\\new")
dat <- read.table(file = "andDelOrder.txt", head = T)

for (i in 1:39){
dat.AND = subset(dat, dat$Subject == i & dat$Condition == "AND" & dat$Correct == 1)

if (i == 1 || i == 9 || i == 17 || i == 25 || i == 33)
{
windows()
par(mfrow=c(2,4))
}

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
plot(density(RT.X), main = " ", xlim=c(0,50000),ylim=c(0,0.0004), ylab="Density", xlab="RT", col="blue", lwd=1)
lines(density(RT.H), col="red", lwd=1)}
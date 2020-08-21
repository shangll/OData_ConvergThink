setwd("C:\\Users\\shangll\\Documents\\NCKU\\sub\\data_spl")
dat = read.table(file='accOrSig2.txt', head=T)

for (i in 1:46)
{
  dat.OR = subset(dat, dat$Subject == i & dat$Condition == "OR" & dat$Correct == 1)
  
  HH.OR = subset(dat.OR, dat.OR$Channel1 == 1 & dat.OR$Channel2 == 1)
  HX.OR = subset(dat.OR, dat.OR$Channel1 == 1 & dat.OR$Channel2 == 0)
  XH.OR = subset(dat.OR, dat.OR$Channel1 == 0 & dat.OR$Channel2 == 1)
  
  RT.HX.OR = HX.OR$RT
  RT.XH.OR = XH.OR$RT
  
  RT.X = c(RT.HX.OR,RT.XH.OR)
  RT.H = HH.OR$RT
  
  if (i == 1 || i == 10 || i == 19 || i == 28 || i == 37)
  {
    windows()
    par(mfrow=c(3,6))
  }
  s = seq(0, 60000, by = 6000)
  hist(RT.X, breaks = s, freq = FALSE)
  lines(density(RT.X), col="blue", lwd=3)
  d = seq(0, 60000, by = 6000)
  hist(RT.H, breaks = d, freq = FALSE)
  lines(density(RT.H), col="blue", lwd=3)
}
setwd("D:\\Usr\\Documents\\NCKU\\new")
ord <- c(1:3, 5:32, 34:39, 41:46)

for (i in ord){
  dat <- read.table("Sig2ANDCRATdel.txt", header=T)
  
  dat.AND <- subset(dat, dat$Condition == "AND" & dat$Correct == 1 & dat$Subject == i)
  
  HH.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 1)
  HX.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 0)
  XH.AND <- subset(dat.AND, dat.AND$Channel1 == 0 & dat.AND$Channel2 == 1)
  
  RT.HH.AND <- HH.AND$RT
  RT.HX.AND <- HX.AND$RT
  RT.XH.AND <- XH.AND$RT
  
}
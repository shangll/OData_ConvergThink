setwd("C:\\Users\\shangll\\Documents\\NCKU\\sub\\data")
datHMP = read.table(file='ACCans.txt', head=T)
for (i in 1:60)
{
  dat = subset(datHMP, datHMP$Subject == i & datHMP$Correct == 1)
  
  HH = subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  XX = subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | (dat$Channel1 == 1 & dat$Channel2 == 0))
  
  XRT = XX$RT
  HRT = HH$RT  
  
  XM = vector()
  HM = vector()
  
  for (j in 1:length(XRT))
  {
    XM[j] = abs(XRT[j] - median(XRT))
  }
  for (n in 1:length(HRT))
  {
    HM[n] = abs(HRT[n] - median(HRT))
  }

  XD = vector()
  HD = vector()
  
  for (h in 1:length(XRT))
  {
    XD[h] = XM[h]/(median(XM)/0.6745)
    if (XD[h] > 2.24)
    {
      XX$Correct[h] = 0
    }
  }
  write.table(XX, file = "C:\\Users\\shangll\\Documents\\NCKU\\sub\\data\\ACChmp.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  for (k in 1:length(HRT))
  {
    HD[k] = HM[k]/(median(HM)/0.6745)
    if (HD[k] > 2.24)
    {
      HH$Correct[k] = 0
    }
  }
  write.table(HH, file = "C:\\Users\\shangll\\Documents\\NCKU\\sub\\data\\ACChmp.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
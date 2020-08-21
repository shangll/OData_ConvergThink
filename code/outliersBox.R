setwd("C:\\Users\\shangll\\Documents\\NCKU\\sub\\data")
datBOX = read.table(file='ACCans.txt', head=T)
for (i in 1:60)
{
  dat = subset(datBOX, datBOX$Subject == i & datBOX$Correct == 1)
  
  HH = subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  XX = subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | (dat$Channel1 == 1 & dat$Channel2 == 0))
  
  XRT = XX$RT
  HRT = HH$RT

  SUF = quantile(XRT, 0.75) + 1.5*IQR(XRT)
  SLF = quantile(XRT, 0.25) - 1.5*IQR(XRT)
  RUF = quantile(HRT, 0.75) + 1.5*IQR(HRT)
  RLF = quantile(HRT, 0.25) - 1.5*IQR(HRT)  
  for (j in 1:length(XRT))
  {
    if (XRT[j] > SUF | XRT[j] < SLF)
    {
      XX$Correct[j] = 0
    }
  }
  write.table(XX, file = "C:\\Users\\shangll\\Documents\\NCKU\\sub\\data\\ACCbox.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  
  for (n in 1:length(HRT))
  {
    if (HRT[n] > RUF | HRT[n] < RLF)
    {
      HH$Correct[n] = 0
    }
  }
  write.table(HH, file = "C:\\Users\\shangll\\Documents\\NCKU\\sub\\data\\ACCbox.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
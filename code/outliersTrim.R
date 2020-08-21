setwd("C:\\Users\\shangll\\Documents\\NCKU\\sub\\data")
datTRM = read.table(file='ACCans.txt', head=T)
for (i in 1:60)
{
  dat = subset(datTRM, datTRM$Subject == i & datTRM$Correct == 1)
  HH = subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  XX = subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | (dat$Channel1 == 1 & dat$Channel2 == 0))
  
  XRT = XX$RT
  HRT = HH$RT
  XN = ceiling(length(XRT) * 0.05)
  HN = ceiling(length(HRT) * 0.05)

  datX = XX[order(XX[,3]),]
  datH = HH[order(HH[,3]),]

  for (j in 1:XN)
  {
    datX$RT[j] = datX$RT[XN+1]
    datX$RT[length(XRT)-(j-1)] = datX$RT[length(XRT)-XN]
  }
  write.table(datX, file = "C:\\Users\\shangll\\Documents\\NCKU\\sub\\data\\ACCtrm.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  for (k in 1:HN)
  {
    datH$RT[k] = datH$RT[HN+1]
    datH$RT[length(HRT)-(j-1)] = datH$RT[length(HRT)-HN]
  }
  write.table(datH, file = "C:\\Users\\shangll\\Documents\\NCKU\\sub\\data\\ACCtrm.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
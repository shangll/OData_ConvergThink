#setwd("D:/SyncDocs/MyJob/converg/results")
setwd("//VUW/Personal$/Homes/S/shangl/My Documents/NCKU/converg/results")
datSIG <- read.table(file='OrACC.txt', head=T, sep="\t")
til <- cbind("Subject","Condition","RT","Correct","Channel1","Channel2")
write.table(til, file = "ORsd3.txt",append = TRUE, sep="\t",
            row.names = FALSE, col.names = FALSE)

ord = c(1:45)

for (i in ord)
{
  dat = subset(datSIG, datSIG$Subject == i & datSIG$Correct == 1)
  
  HH = subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  XX = subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | (dat$Channel1 == 1 & dat$Channel2 == 0))
  
  XRT = XX$RT
  HRT = HH$RT
   
  X.avg = mean(XRT)
  H.avg = mean(HRT)
  X.std = sd(XRT)
  H.std = sd(HRT)
  
  for (j in 1:length(XRT))
  {
    if (XRT[j] > (X.avg+3*X.std) | XRT[j] < (X.avg-3*X.std))
    {
      XX$Correct[j] = 0
    }
  }
  write.table(XX, file = "ORsd3.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  for (n in 1:length(HRT))
  {
    if (HRT[n] > (H.avg+3*H.std) | HRT[n] < (H.avg-3*H.std))
    {
      HH$Correct[n] = 0
    }
  }
  write.table(HH, file = "ORsd3.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
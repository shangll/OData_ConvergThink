setwd("D:/Usr/Documents/NCKU/new")
datAnd <- read.table(file = "AccANDdel.txt", head = T)
ord = c(1:3,5:46)

for (i in ord){
  dat <- subset(datAnd, datAnd$Subject == i & datAnd$Correct == 1)
  hh <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  xx <- subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | (dat$Channel1 == 1 & dat$Channel2 == 0))
  
  HH <- hh[order(hh$RT.both),]
  XX <- xx[order(xx$RT.single),]
  
  criterSlo <- quantile(HH$RT,.05)
  criterSup <- quantile(HH$RT,.95)
  criterBlo <- quantile(XX$RT,.05)
  criterBup <- quantile(XX$RT,.95)
  
  XXnew <- subset(XX, XX$RT>criterSlo & XX$RT<criterSup)
  HHnew <- subset(HH, HH$RT>criterBlo & HH$RT<criterBup)
  
  write.table(XXnew, file = "D:\\Usr\\Documents\\NCKU\\new\\AndDel005.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  write.table(HHnew, file = "D:\\Usr\\Documents\\NCKU\\new\\AndDel005.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
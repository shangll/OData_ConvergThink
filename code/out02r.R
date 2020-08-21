setwd("D:/Usr/Documents/NCKU/new")
datAnd <- read.table(file = "AccAND.txt", head = T)
ord = c(1:46)

for (i in ord){
  dat <- subset(datAnd, datAnd$Subject == i & datAnd$Correct == 1)
  HH <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  XX <- subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | (dat$Channel1 == 1 & dat$Channel2 == 0))
  criterBlo <- quantile(XX$RT,.02)
  criterBup <- quantile(XX$RT,.98)

  HHnew <- subset(HH, HH$RT>criterBlo & HH$RT<criterBup)
  
  write.table(XX, file = "D:\\Usr\\Documents\\NCKU\\new\\AndDel02r.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  write.table(HHnew, file = "D:\\Usr\\Documents\\NCKU\\new\\AndDel02r.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
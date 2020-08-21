setwd("D:/SyncDocs/new")
datOr <- read.table(file = "AccOR.txt", head = T)
ord = c(1:45)

for (i in ord){
  dat <- subset(datOr, datOr$Subject == i & datOr$Correct == 1)
  HH <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  XX <- subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | (dat$Channel1 == 1 & dat$Channel2 == 0))
  criterSlo <- quantile(HH$RT,.03)
  criterSup <- quantile(HH$RT,.97)
  criterBlo <- quantile(XX$RT,.03)
  criterBup <- quantile(XX$RT,.97)
  
  XXnew <- subset(XX, XX$RT>criterSlo & XX$RT<criterSup)
  HHnew <- subset(HH, HH$RT>criterBlo & HH$RT<criterBup)
  
  write.table(XXnew, file = "D:/SyncDocs/new/ORDel03.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  write.table(HHnew, file = "D:/SyncDocs/new/ORDel03.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
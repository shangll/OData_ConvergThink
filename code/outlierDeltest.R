setwd("D:/SyncDocs/MyJob/converg/results")
datAnd <- read.table(file = "test.txt",head = T)
titles <- c("Subject","Condition","RT","Correct","Channel1","Channel2")
titles <- t(titles)
write.table(titles, file = "testdel.txt",
            append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)

ord = c(1:46)

for (i in ord){
  dat <- subset(datAnd, datAnd$Subject == i & datAnd$Correct == 1)
  HH <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  XX <- subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | 
                 (dat$Channel1 == 1 & dat$Channel2 == 0))
  criterSlo <- quantile(HH$RT,.001)
  criterSup <- quantile(HH$RT,.999)
  criterBlo <- quantile(XX$RT,.001)
  criterBup <- quantile(XX$RT,.999)
  
  XXnew <- subset(XX, XX$RT>criterSlo & XX$RT<criterSup)
  HHnew <- subset(HH, HH$RT>criterBlo & HH$RT<criterBup)

  write.table(XXnew, file = "testdel.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  write.table(HHnew, file = "testdel.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
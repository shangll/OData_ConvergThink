setwd("D:/SyncDocs/MyJob/converg/results")
datAnd <- read.table(file = "ANDsd3rep.txt",head = T)
titles <- c("Subject","Condition","RT","Correct","Channel1","Channel2")
titles <- t(titles)
write.table(titles, file = "AND015rep.txt",
            append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)

ord = c(1:46)

for (i in ord){
  dat <- subset(datAnd, datAnd$Subject == i & datAnd$Correct == 1)
  HH <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  XX <- subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | 
                 (dat$Channel1 == 1 & dat$Channel2 == 0))
  criterSlo <- quantile(HH$RT,.015)
  criterSup <- quantile(HH$RT,.985)
  criterBlo <- quantile(XX$RT,.015)
  criterBup <- quantile(XX$RT,.985)
  
  XXnew <- subset(XX, XX$RT>criterSlo & XX$RT<criterSup)
  HHnew <- subset(HH, HH$RT>criterBlo & HH$RT<criterBup)

  write.table(XXnew, file = "AND015rep.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  write.table(HHnew, file = "AND015rep.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
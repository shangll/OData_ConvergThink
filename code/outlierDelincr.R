setwd("D:/Usr/Documents/NCKU/new")
datAnd <- read.table(file = "AccAND.txt",head = T,na.strings="NA",sep="\t")
titles <- c("Subject","Condition","RT","Correct","Channel1","Channel2")
titles <- t(titles)
write.table(titles, file = "D:\\Usr\\Documents\\NCKU\\new\\AndallC.txt",
            append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)

ord = c(1:3,5:39,41:46)

for (i in ord){
  datInc <- subset(datAnd, datAnd$Subject == i & datAnd$Correct == 0)
  HHInc <- subset(datInc, datInc$Channel1 == 1 & datInc$Channel2 == 1)
  XXInc <- subset(datInc, (datInc$Channel1 == 0 & datInc$Channel2 == 1) | (datInc$Channel1 == 1 & datInc$Channel2 == 0))
  
  dat <- subset(datAnd, datAnd$Subject == i & datAnd$Correct == 1)
  HH <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  XX <- subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | (dat$Channel1 == 1 & dat$Channel2 == 0))
  criterSlo <- quantile(HH$RT,.02)
  criterSup <- quantile(HH$RT,.98)
  criterBlo <- quantile(XX$RT,.02)
  criterBup <- quantile(XX$RT,.98)
  
  XXnew <- subset(XX, XX$RT>criterSlo & XX$RT<criterSup)
  HHnew <- subset(HH, HH$RT>criterBlo & HH$RT<criterBup)

  XX$Correct[XX$RT<criterSlo & XX$RT>criterSup] <- 0
  HH$Correct[HH$RT<criterBlo & HH$RT>criterBup] <- 0
  XXnewInc <- subset(XX, XX$Correct == 0)
  HHnewInc <- subset(XX, XX$Correct == 0)
  
  XXall <- rbind(XXnew,XXnewInc,XXInc)
  HHall <- rbind(HHnew,HHnewInc,HHInc)
  
  write.table(XXall, file = "D:\\Usr\\Documents\\NCKU\\new\\AndallC.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  write.table(HHall, file = "D:\\Usr\\Documents\\NCKU\\new\\AndallC.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
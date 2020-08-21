setwd("D:/Usr/Documents/NCKU/new")
datAnd <- read.table(file = "AccAND.txt", head = T)
total <- nrow(datAnd)
ord <- c(1:46)
XXnewNum <- 0
HHnewNum <- 0

for (i in ord){
  dat <- subset(datAnd, datAnd$Subject == i & datAnd$Correct == 1)
  HH <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  XX <- subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | (dat$Channel1 == 1 & dat$Channel2 == 0))
  XXcount <- nrow(XX)
  HHcount <- nrow(HH)
  Xnum <- XXcount/400
  Hnum <- HHcount/200
  
  criterSlo <- quantile(HH$RT,.05)
  criterSup <- quantile(HH$RT,.95)
  criterBlo <- quantile(XX$RT,.05)
  criterBup <- quantile(XX$RT,.95)
  
  XXnew <- subset(XX, XX$RT>criterSlo | XX$RT<criterSup)
  HHnew <- subset(HH, HH$RT>criterBlo | HH$RT<criterBup)
  
  XXnewNum <- XXnewNum+nrow(XXnew)
  HHnewNum <- HHnewNum+nrow(HHnew)
  
  write.table(Xnum, file = "D:\\Usr\\Documents\\NCKU\\new\\ACCnumAND.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  write.table(Hnum, file = "D:\\Usr\\Documents\\NCKU\\new\\ACCnumAND.txt",
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
  
}

(total-XXnewNum-HHnewNum)/total

setwd("D:\\Usr\\Documents\\NCKU\\new")
datAll <- read.table(file='Sig2.5ANDdel.txt', head=T)
dat <- subset(datAll, datAll$Correct == 1)
write.table(dat, file = "D:\\Usr\\Documents\\NCKU\\new\\Sig2.5ANDdelMat.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = TRUE)
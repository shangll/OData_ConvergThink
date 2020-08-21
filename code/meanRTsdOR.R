datOr <- read.table("OrDel.txt", header=T)
ord <- c(1:45)

for (i in ord){
  dat <- subset(datOr, datOr$Subject == i & datOr$Correct == 1)
  HH <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)
  XX <- subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1) | (dat$Channel1 == 1 & dat$Channel2 == 0))
  XH <- subset(dat, (dat$Channel1 == 0 & dat$Channel2 == 1))
  HX <- subset(dat, (dat$Channel1 == 1 & dat$Channel2 == 0))
  accri <- length(XH$RT)/200
  accl<- length(HX$RT)/200
  accs <- length(XX$RT)/400
  accr <- length(HH$RT)/200
  ml <- mean(HX$RT)
  mri <- mean(XH$RT)
  ms <- mean(XX$RT)
  mb <- mean(HH$RT)
  m <- cbind(ms,mb,ml,mri,accs,accr,accl,accri)
  write.table(m, file = "D:\\Usr\\Documents\\NCKU\\new\\rtACCorSB.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
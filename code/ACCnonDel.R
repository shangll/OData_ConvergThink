setwd("D:\\Usr\\Documents\\NCKU\\new")
dat <- read.table("AndRep.txt",header=T,na.strings="NA",sep="\t")
ord <- c(1:3,5:46)
for (s in ord){
  subjDat <- subset(dat,dat$Subject==s)
  left <- subset(subjDat,subjDat$Correct==1&subjDat$Channel1==1&subjDat$Channel2==0)
  right <- subset(subjDat,subjDat$Correct==1&subjDat$Channel1==0&subjDat$Channel2==1)
  l <- nrow(left)/200
  r <- nrow(right)/200
  strl <- nrow(rbind(left,right))/400
  m <- cbind(l,r,strl)
  
  write.table(m, file = "rtACCrep.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
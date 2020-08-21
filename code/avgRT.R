#setwd("D:/SyncDocs/MyJob/converg/results")
setwd("//VUW/Personal$/Homes/S/shangl/My Documents/NCKU/converg/results")
dat <- read.table(file='ORsd3.txt', head=T, sep="\t")
til <- cbind("left","right","single","both")
write.table(til, file = "ORavgRT.txt",append = TRUE, sep="\t",
            row.names = FALSE, col.names = FALSE)
ord <- c(1:45)

for (i in ord)
{
  dat.RT <- subset(dat, dat$Correct == 1 & dat$Subject == i)
  HH <- subset(dat.RT, dat.RT$Channel1 == 1 & dat.RT$Channel2 == 1)
  HX <- subset(dat.RT, dat.RT$Channel1 == 1 & dat.RT$Channel2 == 0)
  XH <- subset(dat.RT, dat.RT$Channel1 == 0 & dat.RT$Channel2 == 1)
  XX = subset(dat.RT, (dat.RT$Channel1 == 0 & dat.RT$Channel2 == 1) | 
                (dat.RT$Channel1 == 1 & dat.RT$Channel2 == 0))
  
  LRT <- mean(HX$RT)
  RRT <- mean(XH$RT)
  SRT <- mean(XX$RT)
  BRT <- mean(HH$RT)
  
  tm <- c(LRT, RRT, SRT, BRT)
  tmMTR <- matrix(tm, nrow = 1, ncol = 4)
  write.table(tmMTR, file = "ORavgRT.txt", 
              append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
}
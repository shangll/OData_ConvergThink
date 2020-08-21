setwd("D:\\Usr\\Documents\\NCKU\\new")
datAll <- read.table(file='Sig2.5ANDdel.txt', head=T)

ord = c(1:3,5:32,34:35,37:39,42:46)
k <- 1
avgL <- vector()
stdL <- vector()
avgR <- vector()
stdR <- vector()

for(n in ord){
  dat <- subset(datAll, datAll$Subject == n & datAll$Correct == 1)
  right <- subset(dat, dat$Channel1 == 0 & dat$Channel2 == 1)
  left <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 0)
  avgL[k] <- mean(left$RT)/1000
  stdL[k] <- sd(left$RT)/1000
  avgR[k] <- mean(right$RT)/1000
  stdR[k] <- sd(right$RT)/1000
  
  k = k + 1

}

print(avgL)
print(stdL)
print(avgR)
print(stdR)

setwd("D:/Usr/Documents/NCKU/new")
datAll <- read.table(file = "Sig2.5ANDdel.txt", head = T)
dat <- subset(datAll, datAll$Correct == 1)

limiSubj <- c(25,24,46,8,26,21,45,22,34,17,14,35,12)
unlimiSubj <- c(9,15,10,6,16,28,23,11,1,13,42,27,38,7,18,20,32,5,30)
superSubj <- c(2,39,37,3,29,43,31,44,19)

unlimiS <- subset(dat, dat$Subject in limiSubj)
limiS
superS

unlimiR
limiR
superR

single <- subset(dat, (dat$Channel1 == 1 & dat$Channel2 == 0)|
                   (dat$Channel1 == 0 & dat$Channel2 == 1))
redundant <- subset(dat, dat$Channel1 == 1 & dat$Channel2 == 1)

library(sft)
setwd("D:/SyncDocs/MyJob/converg/results")
dat <- read.table("ANDsd3.txt", header=T)
ord <- c(1:46)

for (n in ord)
{

dat.AND <- subset(dat, dat$Condition == "AND" & 
                    dat$Correct == 1 & dat$Subject == n)

HH.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 1)
HX.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 0)
XH.AND <- subset(dat.AND, dat.AND$Channel1 == 0 & dat.AND$Channel2 == 1)

RT.HH.AND <- HH.AND$RT
RT.HX.AND <- HX.AND$RT
RT.XH.AND <- XH.AND$RT

#check
#llist <- sort(unique(RT.HX.AND))
#rlist <- sort(unique(RT.XH.AND))
#blist <- sort(unique(RT.HH.AND))

capand.res <- capacity.and(RT=list(RT.HH.AND,RT.HX.AND,RT.XH.AND),
                           CR=NULL, ratio=T)
tvec <- sort(unique(c(RT.HX.AND, RT.XH.AND, RT.HH.AND)))
matplot(tvec, capand.res$Ct(tvec),
        type='l', lty=1, main="Example Capacity Functions", xlab="Time",
        ylab="C(t)")
abline(1,0)

cz <- capand.res$Ctest[1]
pv <- capand.res$Ctest[2]
pvs <- format(pv, scientific = FALSE)
vals <- cbind(cz,pvs)
write.table(vals, file = "cztest.txt", append = TRUE, sep="\t",
            row.names = FALSE, col.names = FALSE)

}
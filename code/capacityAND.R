library(sft)
#setwd("D:/SyncDocs/MyJob/Converg/manuscript_JEPG/data")
setwd("C:/Users/U759254/SyncDocs/MyJob/converg/manuscript_JEPG/data")
dat <- read.table("ANDsd3.txt", header=T)

dat.AND <- subset(dat, dat$Subject != 4 & dat$Subject != 40 & dat$Condition == "AND" & dat$Correct == 1)
#dat.AND <- subset(dat, dat$Condition == "AND" & dat$Correct == 1)

HH.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 1)
HX.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 0)
XH.AND <- subset(dat.AND, dat.AND$Channel1 == 0 & dat.AND$Channel2 == 1)

RT.HH.AND <- HH.AND$RT
RT.HX.AND <- HX.AND$RT
RT.XH.AND <- XH.AND$RT

capand.res <- capacity.and(RT=list(RT.HH.AND,RT.HX.AND,RT.XH.AND),
                           CR=NULL, ratio=T)
ucip.test(list(RT.HH.AND,RT.HX.AND,RT.XH.AND),OR=F)

tvec <- sort(unique(c(RT.HX.AND, RT.XH.AND, RT.HH.AND)))
matplot(tvec, capand.res$Ct(tvec),
        type='p', lty=1, main="Example Capacity Functions", xlab="Time",
        ylab="C(t)")
abline(1,0)

cap.res.and <- capacityGroup(subset(dat.AND, Condition=="AND"),
                             acc.cutoff=.9,stopping.rule="AND")

cty <- cap.res.and$overview
capand.res$Ctest
write.table(cty, file = "captest.txt", append = TRUE, sep="\t",
            row.names = FALSE, col.names = FALSE)

fPCAcapacity(dat.AND, dimensions=2, stopping.rule="AND", plotPCs=TRUE)
library(sft)
setwd("D:/SyncDocs/MyJob/Converg/manuscript_JEPG/data")
#setwd("C:/Users/U759254/SyncDocs/MyJob/converg/manuscript_JEPG/data")
dat <- read.table("ORDel03.txt", header=T)

dat.OR <- subset(dat, dat$Condition == "OR" & dat$Correct == 1)

HH.OR <- subset(dat.OR, dat.OR$Channel1 == 1 & dat.OR$Channel2 == 1)
HX.OR <- subset(dat.OR, dat.OR$Channel1 == 1 & dat.OR$Channel2 == 0)
XH.OR <- subset(dat.OR, dat.OR$Channel1 == 0 & dat.OR$Channel2 == 1)

RT.HH.OR <- HH.OR$RT
RT.HX.OR <- HX.OR$RT
RT.XH.OR <- XH.OR$RT

capor.res <- capacity.or(RT=list(RT.HH.OR,RT.HX.OR,RT.XH.OR))

tvec <- sort(unique(c(RT.HX.OR, RT.XH.OR, RT.HH.OR)))

matplot(tvec, capor.res$Ct(tvec),
        type='l', lty=1, main="Example Capacity Functions", xlab="Time",
        ylab="C(t)", ylim=c(0,3.5))
abline(1,0)

cap.res.or <- capacityGroup(subset(dat.OR, Condition=="OR"),
                            acc.cutoff=.6,stopping.rule="OR")

cap.res.or$overview
capor.res$Ctest

#fPCAcapacity(dat.OR, dimensions=2, stopping.rule="OR", plotPCs=TRUE)
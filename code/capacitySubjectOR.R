library(sft)
#setwd("D:/SyncDocs/new")
setwd("//VUW/Personal$/Homes/S/shangl/My Documents/NCKU/converg/results")

ord = c(1:45)

for (i in ord)
{
dat <- read.table("ORsd3.txt", header=T)

dat.OR <- subset(dat, dat$Condition == "OR" & dat$Correct == 1 & dat$Subject == i)

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
        ylab="C(t)")
abline(1,0)

czor = capor.res$Ctest[1]
write.table(czor, file = "ORczSD3.txt", append = TRUE, sep="\t",
            row.names = FALSE, col.names = FALSE)
}
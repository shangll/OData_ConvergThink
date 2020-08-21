setwd("D:/SyncDocs/new")
datAnd <- read.table(file = "AndDelFinal.txt", head = T)
dat.AND <- subset(datAnd, datAnd$RT <=30000)
write.table(dat.AND, file = "AndDelFinal30000.txt", append = TRUE, sep="\t",
            row.names = FALSE, col.names = FALSE)

HH.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 1)
HX.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 0)
XH.AND <- subset(dat.AND, dat.AND$Channel1 == 0 & dat.AND$Channel2 == 1)

RT.HH.AND <- HH.AND$RT
RT.HX.AND <- HX.AND$RT
RT.XH.AND <- XH.AND$RT

library(sft)
capand.res <- capacity.and(RT=list(RT.HH.AND,RT.HX.AND,RT.XH.AND),ratio=F)
ucip.test(list(RT.HH.AND,RT.HX.AND,RT.XH.AND),OR=F)

tvec <- sort(unique(c(RT.HX.AND, RT.XH.AND, RT.HH.AND)))
matplot(tvec, capand.res$Ct(tvec),
        type='p', lty=1, main="Example Capacity Functions", xlab="Time",
        ylab="C(t)")
abline(1,0)

cap.res.and <- capacityGroup(subset(dat.AND, Condition=="AND"),
                             acc.cutoff=.6,stopping.rule="AND")

cty <- cap.res.and$overview
capand.res$Ctest
write.table(cty, file = "captype30000.txt", append = TRUE,
            sep="\t", row.names = FALSE, col.names = FALSE)

#
ord <-c(1:3,5:39,41:46)
for (n in ord){
  dat <- read.table(file = "AndDelFinal.txt", head = T)
  dat.AND <- subset(dat, dat$RT <=30000)
  
  dat.AND <- subset(dat, dat$Condition == "AND" & dat$Correct == 1 & dat$Subject == n)
  
  HH.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 1)
  HX.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 0)
  XH.AND <- subset(dat.AND, dat.AND$Channel1 == 0 & dat.AND$Channel2 == 1)
  
  RT.HH.AND <- HH.AND$RT
  RT.HX.AND <- HX.AND$RT
  RT.XH.AND <- XH.AND$RT
  
  capand.res <- capacity.and(RT=list(RT.HH.AND,RT.HX.AND,RT.XH.AND))
  tvec <- sort(unique(c(RT.HX.AND, RT.XH.AND, RT.HH.AND)))
  matplot(tvec, capand.res$Ct(tvec),
          type='l', lty=1, main="Example Capacity Functions", xlab="Time",
          ylab="C(t)")
  abline(1,0)
  cz <- capand.res$Ctest[1]
  pv <- capand.res$Ctest[2]
  pvs <- format(pv, scientific = FALSE)
  vals <- cbind(cz,pvs)
  write.table(vals, file = "cz30000.txt", append = TRUE, sep="\t",
              row.names = FALSE, col.names = FALSE)
}







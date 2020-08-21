setwd("D:/SyncDocs/new")
datAnd <- read.table(file = "andResults.txt", head = T)

## Scatterplot====================================================================
library(ggplot2)
# CRAT
windows()
ggplot(datAnd, aes(x = datAnd$CZ.2.5, y = datAnd$CRAT, colour = Capacity, shape = Capacity)) +
  geom_point() +
  stat_smooth(method = "lm", aes(colour = "All", shape = "All")) +
  geom_text(aes(y = datAnd$CRAT + .3, label = Ss)) +
  xlab("C(t)") +
  ylab("CRAT")
cc = lm(datAnd$CRAT~datAnd$CZ.2.5,data = datAnd)
summary(cc)

# PST
windows()
ggplot(datAnd, aes(x = datAnd$CZ.2.5, y = datAnd$PST, colour = Capacity, shape = Capacity)) +
  geom_point() +
  stat_smooth(method = "lm", aes(colour = "All", shape = "All")) +
  geom_text(aes(y = datAnd$PST + 1, label = Ss)) +
  xlab("C(t)") +
  ylab("PST")
cp = lm(datAnd$PST~datAnd$CZ.2.5,data = datAnd)
summary(cp)

# CRAT
windows()
ggplot(datAnd, aes(x = datAnd$fPC1, y = datAnd$CRAT, colour = Capacity, shape = Capacity)) +
  geom_point() +
  stat_smooth(method = "lm", aes(colour = "All", shape = "All")) +
  geom_text(aes(y = datAnd$CRAT + .3, label = Ss)) +
  xlab("fPC1") +
  ylab("CRAT")
cc = lm(datAnd$CRAT~datAnd$fPC1,data = datAnd)
summary(cc)

# PST
windows()
ggplot(datAnd, aes(x = datAnd$fPC1, y = datAnd$PST, colour = Capacity, shape = Capacity)) +
  geom_point() +
  stat_smooth(method = "lm", aes(colour = "All", shape = "All")) +
  geom_text(aes(y = datAnd$PST + 1, label = Ss)) +
  xlab("fPC1") +
  ylab("PST")
cf = lm(datAnd$PST~datAnd$fPC1,data = datAnd)
summary(cp)

## Correlation====================================================================
# r
cor.test(datAnd$CZ.2.5, datAnd$CRAT)
cor.test(datAnd$CZ.2.5, datAnd$PST)
cor.test(datAnd$fPC1, datAnd$CRAT)
cor.test(datAnd$fPC1, datAnd$PST)
cor.test(datAnd$fPC2, datAnd$CRAT)
cor.test(datAnd$fPC2, datAnd$PST)
cor.test(datAnd$CZ.2.5, datAnd$WMC)
cor.test(datAnd$CRAT, datAnd$PST)
cor.test(datAnd$fPC1, datAnd$fPC2)
cor.test(datAnd$CZ.2.5, datAnd$fPC1)
cor.test(datAnd$CZ.2.5, datAnd$fPC2)
#cc = cor(datAnd$CZ.2.5, datAnd$CRAT)
#cp = cor(datAnd$CZ.2.5, datAnd$PST)
#fc1 = cor(datAnd$fPC1, datAnd$CRAT)
#fp1 = cor(datAnd$fPC1, datAnd$PST)
#cw = cor(datAnd$CZ.2.5, datAnd$WMC)
#crp = cor(datAnd$CRAT, datAnd$PST)
# d
#(dCC = 2*cc/(1-cc^2)^(1/2))
#(dCP = 2*cp/(1-cp^2)^(1/2))
#(dfc1 = 2*fc1/(1-fc1^2)^(1/2))
#(dfp1 = 2*fp1/(1-fp1^2)^(1/2))
#(dCW = 2*cw/(1-cw^2)^(1/2))
#(dCRP = 2*crp/(1-crp^2)^(1/2))

library(pwr)
pwr.r.test(n = 41, r = -0.091795, sig.level = 0.5681, alternative = "two.sided")

pwr.r.test(n = 41, r = -0.0847, sig.level = 0.5985, alternative = "two.sided")
pwr.r.test(n = 41, r = -0.1496, sig.level = 0.3504, alternative = "two.sided")
pwr.r.test(n = 41, r = 0.5645, sig.level = 0.0001, alternative = "two.sided")
pwr.r.test(n = 41, r = -0.3625715, sig.level = 0.01982, alternative = "two.sided")
pwr.r.test(n = 41, r = -0.1388388, sig.level = 0.3866, alternative = "two.sided")
## one-way ANOVA==========================================================================
# CRAT
CRATanova = aov(datAnd$CRAT~datAnd$Capacity, data = datAnd)
summary(CRATanova)
pwr.anova.test(k = 3, n = 41, f = 4.963, sig.level = 0.0122)
TukeyHSD(CRATanova)
library(gplots)
windows()
plotmeans(datAnd$CRAT~datAnd$Capacity, data = datAnd, p=0.95, xlab = "Capacity", ylab = "CRAT")
shapiro.test(datAnd$CRAT)
bartlett.test(datAnd$CRAT~datAnd$Capacity)

# PST
PSTanova = aov(datAnd$PST~datAnd$Capacity, data = datAnd)
summary(PSTanova)
pwr.anova.test(k = 3, n = 41, f = 2.738, sig.level = 0.0775)
TukeyHSD(PSTanova)
windows()
plotmeans(datAnd$PST~datAnd$Capacity, data = datAnd, p=0.95, xlab = "Capacity", ylab = "PST")
shapiro.test(datAnd$PST)
bartlett.test(datAnd$PST~datAnd$Capacity)

# WMC
WMCanova = aov(datAnd$WMC~datAnd$Capacity, data = datAnd)
summary(WMCanova)
pwr.anova.test(k = 3, n = 41, f = 5.818, sig.level = 0.00625)
TukeyHSD(WMCanova)
windows()
plotmeans(datAnd$WMC~datAnd$Capacity, data = datAnd, p=0.95)
shapiro.test(datAnd$WMC)
bartlett.test(datAnd$WMC~datAnd$Capacity)

# RT
RTsCanova = aov(datAnd$RT.single~datAnd$Capacity, data = datAnd)
summary(RTsCanova)
TukeyHSD(RTsCanova)
RTrCanova = aov(datAnd$RT.both~datAnd$Capacity, data = datAnd)
summary(RTrCanova)
TukeyHSD(RTrCanova)

RTsPanova = aov(datAnd$RT.single~datAnd$Capacity, data = datAnd)
summary(RTanova)
RTrPanova = aov(datAnd$RT.both~datAnd$Capacity, data = datAnd)
summary(RTanova)
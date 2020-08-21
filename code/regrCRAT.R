setwd("D:/Usr/Documents/NCKU/new")
datAnd <- read.table(file = "resultsAND2.5.txt", head = T)
attach(datAnd)

fit <- lm(CRAT ~ WMC+CZ, data=datAnd)
summary(fit)
windows()
par(mfrow=c(2,2))
plot(fit)

fitint <- lm(CRAT ~ WMC+CZ+WMC:CZ, data=datAnd)
summary(fitint)


fitP <- lm(PST ~ WMC+CZ, data=datAnd)
summary(fitP)
windows()
par(mfrow=c(2,2))
plot(fitP)

fitPint <- lm(PST ~ WMC+CZ+WMC:CZ, data=datAnd)
summary(fitPint)


detach(datAnd)
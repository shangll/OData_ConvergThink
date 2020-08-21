library(sft)
setwd("D:\\Usr\\Documents\\NCKU\\new")
dat <- read.table("AndallC.txt", header=T)

rt1 <- dat$RT[dat$Channel1==1&dat$Channel2==0]
cr1 <- runif(100) < 0.90
rt2 <- dat$RT[dat$Channel1==0&dat$Channel2==1]
cr2 <- runif(100) < 0.95
rt12 <- dat$RT[dat$Channel1==1&dat$Channel2==1]
cr2 <- runif(100) < 0.95
Kucip <- estimateUCIPand(list(rt1, rt2), list(cr1, cr2))

windows()
plot(Kucip$K, do.p=FALSE,
     main="Estimated UCIP Cumulative Reverse Hazard Function",
     xlab="X", ylab="K_UCIP(x)")

times <- sort(c(rt1,rt2))
lines(times, Kucip$K(times) + sqrt(Kucip$Var(times))*qnorm(1-0.05/2),lty=2,lwd=2)
lines(times, Kucip$K(times) - sqrt(Kucip$Var(times))*qnorm(1-0.05/2),lty=2,lwd=2)

lines(times[-1],log(pexp(times[-1],0.5))+log(pexp(times[-1],0.4)), col='red')

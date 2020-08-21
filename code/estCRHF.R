library(sft)
setwd("D:\\Usr\\Documents\\NCKU\\new")
dat <- read.table("AndallC.txt", header=T)
attach(dat)

rt1 <- dat[Condition=="AND" & Channel1==1 & Channel2==0,"RT"]
rt2 <- dat[Condition=="AND" & Channel1==0 & Channel2==1,"RT"]
rt12 <- dat[Condition=="AND" & Channel1==1 & Channel2==1,"RT"]
rt1<-rt1/1000
rt2<-rt2/1000
rt12<-rt12/1000

cr1 <- dat[Condition=="AND" & Channel1==1 & Channel2==0,"Correct"]
cr2 <- dat[Condition=="AND" & Channel1==0 & Channel2==1,"Correct"]
cr12 <- dat[Condition=="AND" & Channel1==1 & Channel2==1,"Correct"]

Kucip <- estimateUCIPand(list(rt1, rt2),list(cr1, cr2))

# Plot the estimated UCIP cumulative reverse hazard function
windows()
plot(Kucip$K,do.p=FALSE,lwd=1.25,
     main="Estimated UCIP Cumulative Reverse Hazard Function",
     xlab="Time(s)",ylab="K_UCIP(t)")

#times <- seq(0, max(c(rt1,rt2)),length.out=length(c(rt1,rt2)))
times <- sort(unique(c(rt1,rt2)))

# Plot 95% Confidence intervals
lines(times, Kucip$K(times)+sqrt(Kucip$Var(times))*qnorm(1-.05/2),
      lwd=2,col="blue",lty=3)
lines(times, Kucip$K(times)-sqrt(Kucip$Var(times))*qnorm(1-.05/2),
      lwd=2,col="blue",lty=3)

# Plot true UCIP cumulative reverse hazard function
lines(times[-1], log(pexp(times[-1],sum(cr12)/length(cr12)))+
        log(pexp(times[-1],sum(cr12)/length(cr12))),
      lwd=2.5,col="red")

legend('bottomright',c("UCIP Cumulative Reverse Hazard Function",
                       "True UCIP Cumulative Reverse Hazard Function",
                       "95% Confidence Intervals"),
       lty=c(1,1,3),col=c("black","red","blue"),
       lwd=2,bty="n")

detach(dat)
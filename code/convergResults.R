library(ggplot2)
library(ggpubr)
library(devtools)
library(ggthemr)
library(ggtech)
library(ggm)
library(psych)
library(car)
library(MASS)
library(lavaan)

setwd("D:/SyncDocs/MyJob/Converg/manuscript_JEPG/data")
#setwd("C:/Users/U759254/SyncDocs/MyJob/converg/manuscript_JEPG/data")

#######################################################################
# ############################# AND-rule
#######################################################################

dat <- read.table(file = "ANDresults.txt", head = T, na.strings="NA",sep="\t")
datAnd <- subset(dat, dat$Subject!= 4 & dat$Subject!= 40)

attach(datAnd)
summary(datAnd)
sd(RT.left)
sd(RT.right)
sd(RT.single)
sd(RT.both)
sd(RT.left.rep)
sd(RT.right.rep)
sd(RT.single.rep)
sd(RT.both.rep)
sd(ACC.left)
sd(ACC.right)
sd(ACC.single)
sd(ACC.both)
sd(ACC.left.rep)
sd(ACC.right.rep)
sd(ACC.single.rep)
sd(ACC.both.rep)
sd(CRAT)
sd(WMC)

## t-test RT & ACC ====================================
RTall <- c(RT.left,RT.right,RT.both)
ACCall <- c(ACC.left,ACC.right,ACC.both)
cond <- c(rep("l",length(RT.left)),rep("r",length(RT.right)),rep("b",length(RT.both)))
RTaov <- aov(RTall~cond)
summary(RTaov)
ACCaov <- aov(ACCall~cond)
summary(ACCaov)
TukeyHSD(ACCaov)
# (2+3.85*2)+(2+2.55*2)+(2+4*2)ACC
t.test(ACC.single,ACC.both,paired=T)
dnonrep <- (mean(ACC.single)-mean(ACC.both))/sqrt(((sd(ACC.single))^2+(sd(ACC.both))^2)/2)
print(dnonrep)
t.test(ACC.single.rep,ACC.both.rep,paired=T)
drep <- (mean(ACC.single.rep)-mean(ACC.both.rep))/sqrt(((sd(ACC.single.rep))^2+(sd(ACC.both.rep))^2)/2)
print(drep)
# RT
t.test(RT.single,RT.both,paired=T)
drt <- (mean(RT.single)-mean(RT.both))/sqrt(((sd(RT.single))^2+(sd(RT.both))^2)/2)
print(drt)
t.test(RT.single.rep,RT.both.rep,paired=T)
drt.rep <- (mean(RT.single.rep)-mean(RT.both.rep))/sqrt(((sd(RT.single.rep))^2+(sd(RT.both.rep))^2)/2)
print(drt.rep)

## split groups =======================================
crat1 <- quantile(CRAT,.3)
crat3 <- quantile(CRAT,.7)
cap1 <- quantile(CZ,.3)
cap3 <- quantile(CZ,.7)
wmc1 <- quantile(WMC,.3)
wmc3 <- quantile(WMC,.7)

datAnd$Level[CRAT<=crat1] <- "lowC"
datAnd$Level[CRAT>crat1&CRAT<crat3] <- "mC"
datAnd$Level[CRAT>=crat3] <- "highC"
datAnd$Group[CZ<=cap1] <- "lowE"
datAnd$Group[CZ>cap1&CZ<cap3] <- "mE"
datAnd$Group[CZ>=cap3] <- "highE"
datAnd$wlevel[WMC<=wmc1] <- "lowW"
datAnd$wlevel[WMC>wmc1&WMC<wmc3] <- "mW"
datAnd$wlevel[WMC>=wmc3] <- "highW"

mean(CRAT[datAnd$Level=="highC"])
sd(CRAT[datAnd$Level=="highC"])
length(CRAT[datAnd$Level=="highC"])
mean(CRAT[datAnd$Level=="lowC"])
sd(CRAT[datAnd$Level=="lowC"])
length(CRAT[datAnd$Level=="lowC"])

# t-test CRAT groups on CZ
t.test(CRAT[datAnd$Level=="highC"],CRAT[datAnd$Level=="lowC"])
dCRAT <- (mean(CRAT[datAnd$Level=="lowC"])-mean(CRAT[datAnd$Level=="highC"]))/sqrt(((sd(CRAT[datAnd$Level=="lowC"]))^2+(sd(CRAT[datAnd$Level=="highC"]))^2)/2)
print(dCRAT)

t.test(CZ[datAnd$Level=="highC"],CZ[datAnd$Level=="lowC"])
dCZ <- (mean(CZ[datAnd$Level=="lowC"])-mean(CZ[datAnd$Level=="highC"]))/sqrt(((sd(CZ[datAnd$Level=="lowC"]))^2+(sd(CZ[datAnd$Level=="highC"]))^2)/2)
print(dCZ)

t.test(WMC[datAnd$Level=="highC"],WMC[datAnd$Level=="lowC"])
dWMC <- (mean(WMC[datAnd$Level=="lowC"])-mean(WMC[datAnd$Level=="highC"]))/sqrt(((sd(WMC[datAnd$Level=="lowC"]))^2+(sd(WMC[datAnd$Level=="highC"]))^2)/2)
print(dWMC)

t.test(CRAT[datAnd$wlevel=="highW"],CRAT[datAnd$wlevel=="lowW"])
dCW <- (mean(CRAT[datAnd$wlevel=="lowW"])-mean(CRAT[datAnd$wlevel=="highW"]))/sqrt(((sd(CRAT[datAnd$wlevel=="lowW"]))^2+(sd(CRAT[datAnd$wlevel=="highW"]))^2)/2)
print(dCW)

mean(datAnd$CZ[datAnd$Level=="highC"])
mean(datAnd$CZ[datAnd$Level=="lowC"])



extrCRAT <- rbind(datAnd[datAnd$wlevel=="highW",],
                 datAnd[datAnd$wlevel=="lowW",])

fit2 <- aov(CRAT~CZ*WMC, data=extrCRAT)
summary(fit2)

datAnd$Subject[datAnd$Level=="highC"]
datAnd$Subject[datAnd$Level=="lowC"]
hitype <- datAnd$Capacity[datAnd$Level=="highC"]
lotype <- datAnd$Capacity[datAnd$Level=="lowC"]

extrCRATfinal <- datAnd[datAnd$Level!="mC",]

#ggthemr('dust')
ttestCZfig <- ggplot(extrCRATfinal, aes(x=Level,y=CZ,fill=Level))+
  geom_boxplot()+
  geom_hline(aes(yintercept=0),colour="red",linetype="dashed",size=1.5)+
  scale_color_brewer(type = "div",palette = "Set1")+
  scale_x_discrete(labels=c("High","Low"))+
  scale_fill_discrete(name="Group",breaks=c("highC","lowC"),
                      labels=c("High-CRAT","Low-CRAT"))+
  xlab("CRAT Groups")+
  ylab("Capacity Z-score")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme_classic()+
  theme(legend.position="bottom",
        legend.background=element_rect(colour='black',fill="white"),
        text=element_text(family="Arial"))
  
windows()
ttestCZfig
#ggthemr_reset()

# table (fisher exact)
# CRAT groups x capacity types
hilimi <- nrow(datAnd[datAnd$Level=="highC"&Capacity!="Super",])
lolimi <- nrow(datAnd[datAnd$Level=="lowC"&Capacity!="Super",])
hinonlimi <- nrow(datAnd[datAnd$Level=="highC"&Capacity=="Super",])
lononlimi <- nrow(datAnd[datAnd$Level=="lowC"&Capacity=="Super",])
chisq.test(matrix(c(hilimi,lolimi,hinonlimi,lononlimi),nr=2))$observed
chisq.test(matrix(c(hilimi,lolimi,hinonlimi,lononlimi),nr=2))$expected
fisher.test(matrix(c(hilimi,lolimi,hinonlimi,lononlimi),nr=2))

# bootstrapping
chisq.test(matrix(c(12,6,2,7),nr=2))$observed
chisq.test(matrix(c(12,6,2,7),nr=2))$expected
fisher.test(matrix(c(12,6,2,7),nr=2))

#######################################################################
# 2-way ANOVA
# CZ
extrCRAT<- rbind(datAnd[datAnd$Level=="highC"&datAnd$Group!="mE",],
                 datAnd[datAnd$Level=="lowC"&datAnd$Group!="mE",])
higSubj <- extrCRAT$Subject[extrCRAT$Level=="highC"]
lowSubj <- extrCRAT$Subject[extrCRAT$Level=="lowC"]
fitCZ <- aov(extrCRAT$CRAT~extrCRAT$Level*extrCRAT$Group)
summary(fitCZ)

# RT
sRT <- RT.single
bRT <- RT.both
allRT <- c(sRT,bRT)
trl <- c(rep("single",length(sRT)),rep("both",length(bRT)))
lev <- rep(datAnd$Level,2)
fitRT <- aov(allRT ~ trl*lev)
summary(fitRT)

# check
leveneTest(extrCRAT$CRAT, as.factor(extrCRAT$Level))
leveneTest(extrCRAT$CRAT, as.factor(extrCRAT$Capacity))

#######################################################################

## correlation b/w CZ & CRAT===========================
diffACCSingle <- ACC.single.rep-ACC.single
diffACCBoth <- ACC.both.rep-ACC.both
corrDat <- cbind(CZ.rep,CZ,D1,D2,CRAT,WMC,
                 RT.single,RT.both,ACC.single,ACC.both,
                 diffACCSingle,diffACCBoth)
cor(corrDat)
cor.test(CZ, CRAT)
cor.test(D1, CRAT)
cor.test(D2, CRAT)
cor.test(CZ.rep, CRAT)
cor.test(CZ, WMC)
cor.test(WMC, CRAT)

ccr1 <- cor(CZ,CRAT)
ccr2 <- cor(CZ.rep,CRAT)
cczf1=1/2*log((1+ccr1)/(1-ccr1))
cczf2=1/2*log((1+ccr2)/(1-ccr2))
cczr <- (cczf1-cczf2)/sqrt(1/(44-3)+1/(44-3))
pnorm(cczr)

fitCorr <- lm(CRAT~CZ)
summary(fitCorr)

pc <- datAnd[c("CZ","CRAT","WMC")]

r1 <- cor(datAnd$CZ, datAnd$CRAT)
r2 <- pcor(c(1,2,3),cov(pc))
zf1=1/2*log((1+r1)/(1-r1))
zf2=1/2*log((1+r2)/(1-r2))
zr <- (zf1-zf2)/sqrt(1/(44-3)+1/(44-3))
pnorm(zr)

# repeated cues
rcr1 <- cor(CZ, CRAT)
rcr2 <- cor(CZ.rep, CRAT)
rczf1=1/2*log((1+rcr1)/(1-rcr1))
rczf2=1/2*log((1+rcr2)/(1-rcr2))
rczr <- (rczf1-rczf2)/sqrt(1/(44-3)+1/(44-3))
pnorm(rczr)

# Repeated Ans
cor.test(CRAT,ans.both.rep)
difr <- pcor(c(1,2,3),cov(cbind(ans.both.rep,CRAT,WMC)))
pcor.test(difr,1,44)

t.test(ans.both.rep[datAnd$Level=="highC"],ans.both.rep[datAnd$Level=="lowC"])
dans <- (mean(ans.both.rep[datAnd$Level=="lowC"])-mean(ans.both.rep[datAnd$Level=="highC"]))/sqrt(((sd(ans.both.rep[datAnd$Level=="lowC"]))^2+(sd(ans.both.rep[datAnd$Level=="highC"]))^2)/2)
print(dans)
t.test(ans.single.rep[datAnd$Level=="highC"],ans.single.rep[datAnd$Level=="lowC"])
danssingle <- (mean(ans.single.rep[datAnd$Level=="lowC"])-mean(ans.single.rep[datAnd$Level=="highC"]))/sqrt(((sd(ans.single.rep[datAnd$Level=="lowC"]))^2+(sd(ans.single.rep[datAnd$Level=="highC"]))^2)/2)
print(danssingle)

summary(lm(CRAT~CZ))
# linear regression
corrfig <- ggplot(datAnd, aes(x=CZ,y=CRAT))+
  geom_point(size = 2)+
  #scale_color_brewer(type = "div",palette = "Set2")+
  scale_shape_manual(values=c(15,16,17))+
  geom_smooth(method = "lm",formula=y~x,size=1,color="red")+
  annotate("text",x=2.5,y=10.15,label="y   = 17.93 - 0.56x")+
  annotate("text",x=1.2,y=9.35,parse=T,label="Ra ^2")+
  annotate("text",x=2.15,y=9.25,label=" = 0.18")+
  scale_y_continuous(limits=c(9,28))+
  xlim(-6,5)+
  ylim(6,28)+
  xlab("Capacity")+
  ylab("CRAT")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme_classic()+
  theme(legend.position="bottom",
        legend.background=element_rect(colour='black',fill="white"),
        text=element_text(family="Arial"))
windows()
corrfig


# --- supplementary analysis ---
# correlation
cor.test(CZ, RT.single)
cor.test(CZ, RT.both)
cor.test(CRAT, RT.single)
cor.test(CRAT, RT.both)
cor.test(CRAT, RT.single.rep)
cor.test(CRAT, RT.both.rep)
cor.test(CRAT, diffACCSingle)
cor.test(CRAT, diffACCBoth)

RTcombin <- rbind(rep('single', len(RT.single)), rep('both', len(RT.both)))
# comparing RTs
leveneTest(RT.single, as.factor(datAnd$Level))
leveneTest(RT.both, as.factor(datAnd$Level))
fitRTs <- aov(RT.single~as.factor(datAnd$Level), data=datAnd)
summary(fitRTs)
# ES = sqrt(F/n)

fitRTb <- aov(RT.both~as.factor(datAnd$Level), data=datAnd)
summary(fitRTb)

t.test(extrCRAT$RT.single[extrCRAT$Level=="highC"], extrCRAT$RT.single[extrCRAT$Level=="lowC"])
dsingleRT <- (mean(RT.single[extrCRAT$Level=="lowC"])-
                mean(extrCRAT$RT.single[extrCRAT$Level=="highC"]))/
  sqrt(((sd(RT.single[extrCRAT$Level=="lowC"]))^2+
          (sd(extrCRAT$RT.single[extrCRAT$Level=="highC"]))^2)/2)
print(dsingleRT)

# MANOVA
y <- cbind(extrCRAT$RT.single, extrCRAT$RT.both)
CRAT_level <- factor(extrCRAT$Level)
aggregate(y, by=list(CRAT_level), FUN=mean)
cov(y)
fitMANV <- manova(y ~ CRAT_level)
summary(fitMANV)
summary.aov(fitMANV)

y3 <- cbind(datAnd$RT.single, datAnd$RT.both)
CRAT_level3 <- factor(datAnd$Level)
Cap_level3 <- factor(datAnd$Capacity)
aggregate(y3, by=list(CRAT_level3), FUN=mean)
cov(y3)
fitMANV3 <- manova(y3 ~ CRAT_level3+Cap_level3)
summary(fitMANV3)
summary.aov(fitMANV3)

# SEM
#datMat <- cbind(datAnd$CRAT, datAnd$RT.single, datAnd$RT.both)
#fitMod <- 
#datfit <- lavaan(fitMod, sample.cov = datMat, sample.nobs = 44)

summary(lm(datAnd$CRAT~datAnd$RT.single+datAnd$RT.both+datAnd$RT.single:datAnd$RT.both))

# --- --- --- --- --- ---
detach(datAnd)

subjAll <- read.table(file = "ANDsd3.txt", head = T, na.strings="NA",sep="\t")
subjAllrep <- read.table(file = "ANDsd3rep.txt", head = T, na.strings="NA",sep="\t")
dat4 <- subset(subjAll, subjAll$Subject==4)
dat4rep <- subset(subjAllrep, subjAllrep$Subject==4)
dat4.single <- subset(dat4, (dat4$Channel1 == 0 & dat4$Channel2 == 1)|
                   (dat4$Channel1 == 1 & dat4$Channel2 == 0))
dat4.both <- subset(dat4, dat4$Channel1 == 1 & dat4$Channel2 == 1)
dat4rep.both <- subset(dat4rep, (dat4rep$Channel1 == 1 & dat4rep$Channel2 == 1))
dat4rep.single <- subset(dat4rep, (dat4rep$Channel1 == 0 & dat4rep$Channel2 == 1)|
                           (dat4rep$Channel1 == 1 & dat4rep$Channel2 == 0))

num4s <- length(dat4.single$Correct[dat4.single$Correct==1])
num4b <- length(dat4.both$Correct[dat4.both$Correct==1])
num4reps <- length(dat4rep.single$Correct[dat4rep.single$Correct==1])
num4repb <- length(dat4rep.both$Correct[dat4rep.both$Correct==1])
(num4reps-num4s)/num4reps
(num4repb-num4b)/num4repb


#######################################################################
# ############################## OR-rule
#######################################################################
datOr <- read.table(file="ORresults.txt",head=T)
summary(datOr)
sd(datOr$RT.left)
sd(datOr$RT.right)
sd(datOr$RT.single)
sd(datOr$RT.both)
sd(datOr$ACC.left)
sd(datOr$ACC.right)
sd(datOr$ACC.single)
sd(datOr$ACC.both)
sd(datOr$CRAT)
sd(datOr$WMC)

## t-test RT & ACC ====================================
RTall <- c(datOr$RT.left,datOr$RT.right,datOr$RT.both)
ACCall <- c(datOr$ACC.left,datOr$ACC.right,datOr$ACC.both)
cond <- c(rep("l",length(datOr$RT.left)),
          rep("r",length(datOr$RT.right)),
          rep("b",length(datOr$RT.both)))
RTaov <- aov(RTall~cond)
summary(RTaov)
ACCaov <- aov(ACCall~cond)
summary(ACCaov)
TukeyHSD(ACCaov)
# ACC
t.test(datOr$ACC.single,datOr$ACC.both)
dnonrep <- (mean(datOr$ACC.single)-mean(datOr$ACC.both))/sqrt(((sd(datOr$ACC.single))^2+(sd(datOr$ACC.both))^2)/2)
print(dnonrep)
t.test(ACC.single.rep,ACC.both)
drep <- (mean(datOr$ACC.single.rep)-mean(datOr$ACC.both))/sqrt(((sd(datOr$ACC.single.rep))^2+(sd(datOr$ACC.both))^2)/2)
print(drep)
# RT
t.test(datOr$RT.single,datOr$RT.both)
drt <- (mean(datOr$RT.single)-mean(datOr$RT.both))/sqrt(((sd(datOr$RT.single))^2+(sd(datOr$RT.both))^2)/2)

## correlation b/w CZ & CRAT===========================
corrDatOr <- cbind(datOr$CZ,datOr$CRAT,datOr$WMC,datOr$PST,datOr$S,datOr$P)
corrDatOr <- data.frame(corrDatOr)
names(corrDatOr) <- c("CZ","CRAT","WMC","PST","S","P")
cor(corrDatOr)
cor.test(datOr$CZ, datOr$CRAT)
cor.test(datOr$CZ, datOr$WMC)
cor.test(datOr$WMC, datOr$CRAT)
cor.test(datOr$RT.single, datOr$RT.both)
cor.test(datOr$RT.single, datOr$CZ)
cor.test(datOr$RT.both, datOr$CZ)

fitCorrOR <- lm(datOr$CRAT~datOr$CZ)
summary(fitCorrOR)

ORv <- corrDatOr[c("CZ","CRAT","WMC")]
ORpc <- pcor(c(1,2,3),cov(ORv))
ORpc
pcor.test(ORpc,1,45)

rOR1 <- cor(corrDatOr$CZ, corrDatOr$CRAT)
rOR2 <- pcor(c(1,2,3),cov(ORpc))
zfOR1=1/2*log((1+rOR1)/(1-rOR1))
zfOR2=1/2*log((1+rOR2)/(1-rOR2))
zrOR <- (zfOR1-zfOR2)/sqrt(1/(45-3)+1/(45-3))
pnorm(zrOR)

## split groups =======================================
crat1 <- quantile(datOr$CRAT,.3)
crat3 <- quantile(datOr$CRAT,.7)
cap1 <- quantile(datOr$CZ,.3)
cap3 <- quantile(datOr$CZ,.7)
wmc1 <- quantile(datOr$WMC,.3)
wmc3 <- quantile(datOr$WMC,.7)

datOr$Level[datOr$CRAT<=crat1] <- "lowC"
datOr$Level[datOr$CRAT>crat1&datOr$CRAT<crat3] <- "mC"
datOr$Level[datOr$CRAT>=crat3] <- "highC"
datOr$Group[datOr$CZ<=cap1] <- "lowE"
datOr$Group[datOr$CZ>cap1&datOr$CZ<cap3] <- "mE"
datOr$Group[datOr$CZ>=cap3] <- "highE"
datOr$wlevel[datOr$WMC<=wmc1] <- "lowW"
datOr$wlevel[datOr$WMC>wmc1&datOr$WMC<wmc3] <- "mW"
datOr$wlevel[datOr$WMC>=wmc3] <- "highW"

mean(datOr$CRAT[datOr$Level=="highC"])
sd(datOr$CRAT[datOr$Level=="highC"])
length(datOr$CRAT[datOr$Level=="highC"])
mean(datOr$CRAT[datOr$Level=="lowC"])
sd(datOr$CRAT[datOr$Level=="lowC"])
length(datOr$CRAT[datOr$Level=="lowC"])
extrCRATor <- rbind(datOr[datOr$Level=="highC",],datOr[datOr$Level=="lowC",])

# t-test CRAT groups on CZ
t.test(datOr$CRAT[datOr$Level=="highC"],datOr$CRAT[datOr$Level=="lowC"])
dCRATOR <- (mean(datOr$CRAT[datOr$Level=="lowC"])-mean(datOr$CRAT[datOr$Level=="highC"]))/sqrt(((sd(datOr$CRAT[datOr$Level=="lowC"]))^2+(sd(datOr$CRAT[datOr$Level=="highC"]))^2)/2)
print(dCRATOR)

t.test(datOr$CZ[datOr$Level=="highC"],datOr$CZ[datOr$Level=="lowC"])
dCZOR <- (mean(datOr$CZ[datOr$Level=="lowC"])-mean(datOr$CZ[datOr$Level=="highC"]))/sqrt(((sd(datOr$CZ[datOr$Level=="lowC"]))^2+(sd(datOr$CZ[datOr$Level=="highC"]))^2)/2)
print(dCZOR)

t.test(datOr$WMC[datOr$Level=="highC"],datOr$WMC[datOr$Level=="lowC"])
dWMCOR <- (mean(datOr$WMC[datOr$Level=="lowC"])-mean(datOr$WMC[datOr$Level=="highC"]))/sqrt(((sd(datOr$WMC[datOr$Level=="lowC"]))^2+(sd(datOr$WMC[datOr$Level=="highC"]))^2)/2)
print(dWMCOR)

t.test(datOr$CRAT[datOr$wlevel=="highW"],datOr$CRAT[datOr$wlevel=="lowW"])
dCWOR <- (mean(datOr$CRAT[datOr$wlevel=="lowW"])-mean(datOr$CRAT[datOr$wlevel=="highW"]))/sqrt(((sd(datOr$CRAT[datOr$wlevel=="lowW"]))^2+(sd(datOr$CRAT[datOr$wlevel=="highW"]))^2)/2)
print(dCWOR)

ttestCZfig <- ggplot(extrCRATor, aes(x=Level,y=CZ,fill=Level))+
  geom_boxplot()+
  geom_hline(aes(yintercept=0),colour="red",linetype="dashed",size=1.5)+
  scale_color_brewer(type = "div",palette = "Set1")+
  scale_x_discrete(labels=c("High","Low"))+
  scale_fill_discrete(name="Group",breaks=c("highC","lowC"),
                      labels=c("High-CRAT","Low-CRAT"))+
  xlab("CRAT Groups")+
  ylab("Capacity Z-score")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme_classic()+
  theme(legend.position="bottom",
        legend.background=element_rect(colour='black',fill="white"),
        text=element_text(family="Arial"))

windows()
ttestCZfig

corrfigOR <- ggplot(datOr, aes(x=CZ,y=CRAT))+
  geom_point(size = 2)+
  xlim(-16,-2)+
  ylim(6,28)+
  xlab("Capacity")+
  ylab("CRAT")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme_classic()+
  theme(legend.position="bottom",
        legend.background=element_rect(colour='black',fill="white"),
        text=element_text(family="Arial"))
windows()
corrfigOR




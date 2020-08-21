setwd("D:/SyncDocs/new")

## ##################### AND ################
datAnd <- read.table(file = "resultsANDCRATdel.txt", head = T)

## split groups =======================================
crat1 <- quantile(datAnd$CRAT,.25)
crat3 <- quantile(datAnd$CRAT,.75)
lowCRAT <- subset(datAnd, datAnd$CRAT <= crat1)
highCRAT <- subset(datAnd, datAnd$CRAT >= crat3)
t.test(lowCRAT$CZ,highCRAT$CZ)

# pst1 <- quantile(PST,.25)
# pst3 <- quantile(PST,.75)
# lowPST <- subset(datAnd, PST <= pst1)
# highPST <- subset(datAnd, PST >= pst3)
# 
# pst.s1 <- quantile(s,.25)
# pst.s3 <- quantile(s,.75)
# lowPST.s <- subset(datAnd, s <= pst.s1)
# highPST.s <- subset(datAnd, s >= pst.s3)
# 
# t.test(lowPST.s$CZ,highPST.s$CZ)
# 
# pst.g1 <- quantile(g,.25)
# pst.g3 <- quantile(g,.75)
# lowPST.g <- subset(datAnd, g <= pst.g1)
# highPST.g <- subset(datAnd, g >= pst.g3)
# 
# t.test(lowPST.g$CZ,highPST.g$CZ)
# =====================================================

# clr <- c("#555555","#EFA86E","#db735c")


## Condition: two-way ANOVA================================================
WMC <- factor(datAnd$LevelW)
Capacity <- factor(datAnd$Group)
CRAT <- datAnd$CRAT

CCinter <- aov(CRAT ~ WMC*Capacity)
summary(CCinter)
library(HH)
windows()
interaction2wt(CRAT ~ WMC*Capacity)

## Scatterplot==========================================
library(ggplot2)
# CRAT
cratandc <- ggplot(datAnd, aes(x = datAnd$CZ, y = datAnd$CRAT, colour = Capacity,
                              shape = Capacity)) +
  geom_point() +
  stat_smooth(method = "lm", aes(colour = "All", shape = "All")) +
  annotate("text", x=0, y=5.15, label="r = -0.47") +
  annotate("text", x=3, y=5.15, label="p < 0.01") +
  scale_y_continuous(limits=c(0,35)) +
  xlab("(A) C(t)") +
  ylab("CRAT") +
  theme_set(theme_bw()) +
  theme(panel.grid.major=element_line(colour=NA))+
  theme(legend.position="none")+
  scale_colour_manual(values = c("#555555","#EFA86E","#db735c","#2A91A2"))
cc <- lm(datAnd$CRAT~datAnd$CZ,data = datAnd)
summary(cc)

# PST
pstandc = ggplot(datAnd, aes(x = datAnd$CZ, y = datAnd$PST, colour = Capacity, shape = Capacity)) +
  geom_point() +
  stat_smooth(method = "lm", aes(colour = "All", shape = "All")) +
  #geom_text(aes(y = datAnd$PST + 1, label = Ss)) +
  annotate("text", x=0, y=41.75, label="r = -0.31") +
  annotate("text", x=3, y=41.75, label="p < 0.05") +
  scale_y_continuous(limits=c(30,110)) +
  xlab("(B) C(t)") +
  ylab("PST") +
  theme_set(theme_bw()) +
  theme(panel.grid.major=element_line(colour=NA))+
  theme(legend.position="none")+
  scale_colour_manual(values = c("#555555","#EFA86E","#db735c","#2A91A2"))
cp = lm(datAnd$PST~datAnd$CZ,data = datAnd)
summary(cp)

# PST.semantic
sandc = ggplot(datAnd, aes(x = datAnd$CZ, y = datAnd$s, colour = Capacity, shape = Capacity)) +
  geom_point() +
  stat_smooth(method = "lm", aes(colour = "All", shape = "All")) +
  #geom_text(aes(y = datAnd$s + 1, label = Ss)) +
  annotate("text", x=0, y=9.55, label="r = -0.30") +
  annotate("text", x=3, y=9.55, label="p = 0.06") +
  scale_y_continuous(limits=c(0,65)) +
  xlab("(C) C(t)") +
  ylab("PST.semantic") +
  theme_set(theme_bw()) +
  theme(panel.grid.major=element_line(colour=NA))+
  theme(legend.position=c(.80,.9))+
  theme(legend.key = element_blank())+
  theme(legend.background = element_blank())+
  scale_colour_manual(values = c("#555555","#EFA86E","#db735c","#2A91A2"))
cs = lm(datAnd$s~datAnd$CZ,data = datAnd)
summary(cs)

# CZ~RT.single
ctandrt = ggplot(datAnd, aes(x = datAnd$RT.single, y = datAnd$CZ, colour = Capacity, shape = Capacity)) +
 geom_point() +
 stat_smooth(method = "lm", aes(colour = "All", shape = "All")) +
geom_text(aes(y = datAnd$RT.single + 1, label = Ss)) +
 annotate("text", x=7900, y=-7.175, label="r = 0.37") +
 annotate("text", x=10500, y=-7.15, label="p = 0.02") +
 scale_y_continuous(limits=c(-10,10)) +
 xlab("(D) RT in single trial") +
 ylab("C(t)") +
 theme_set(theme_bw()) +
 theme(panel.grid.major=element_line(colour=NA))+
 theme(legend.position=c(.80,.9))+
 theme(legend.key = element_blank())+
 theme(legend.background = element_blank())+
 scale_colour_manual(values = c("#555555","#EFA86E","#db735c","#2A91A2"))
ctrt = lm(datAnd$CZ~datAnd$RT.single,data = datAnd)
summary(ctrt)

windows()
library(gridExtra)
grid.arrange(cratandc, pstandc, sandc, ctandrt, ncol = 4)

## r & power====================================================
#cor.test(datAnd$CRAT,datAnd$PST)
#cor.test(datOr$CRAT,datOr$PST)

## one-way ANOVA================================================
library(ggpubr)
w <- ggbarplot(datAnd, x="Capacity", y="WMC", add = "mean_se", fill = "Capacity",
               palette = c("#555555","#EFA86E","#db735c"),
               position = position_dodge(0.8))+
  stat_compare_means(aes(group=Capacity), label = "p.signif", label.y = 50)
c <- ggbarplot(datAnd, x="Capacity", y="CRAT", add = "mean_se", fill = "Capacity",
               palette = c("#555555","#EFA86E","#db735c"),
               position = position_dodge(0.8))+
  stat_compare_means(aes(group=Capacity), label = "p.signif", label.y = 25)
p <- ggbarplot(datAnd, x="Capacity", y="PST", add = "mean_se", fill = "Capacity",
               palette = c("#555555","#EFA86E","#db735c"),
               position = position_dodge(0.8))+
  stat_compare_means(aes(group=Capacity), label = "p.signif", label.y = 90)
windows()
library(gridExtra)
grid.arrange(w, c, p, ncol = 3)

Capacity = datAnd$Capacity
library(lattice)
#WMC
aggregate(datAnd$WMC, by=list(datAnd$Capacity), FUN=mean)
wcanova = bwplot(datAnd$WMC~datAnd$Capacity, ylab = "(A) Mean of WMC")
model=aov(datAnd$WMC~Capacity)

summary(model)
(result1=TukeyHSD(model))

shapiro.test(datAnd$WMC)
bartlett.test(datAnd$WMC~datAnd$Capacity)

# CRAT
aggregate(datAnd$CRAT, by=list(datAnd$Capacity), FUN=mean)
ccanova = bwplot(datAnd$CRAT~datAnd$Capacity, ylab = "(B) Mean of CRAT")
model=aov(datAnd$CRAT~Capacity)
summary(model)
(result2=TukeyHSD(model))

shapiro.test(datAnd$CRAT)
bartlett.test(datAnd$CRAT~datAnd$Capacity)

#PST
aggregate(datAnd$PST, by=list(datAnd$Capacity), FUN=mean)
pcanova = bwplot(datAnd$PST~datAnd$Capacity, ylab = "(C) Mean of PST")
model=aov(datAnd$PST~Capacity)
summary(model)
(result3=TukeyHSD(model))

shapiro.test(datAnd$PST)
bartlett.test(datAnd$PST~datAnd$Capacity)

windows()
grid.arrange(wcanova, ccanova, pcanova, ncol = 3)

windows()
par(mfrow=c(1,3))
plot(result1)
title(sub = "(A) WMC", font.sub = 2, cex.sub = 1.2)
plot(result2)
title(sub = "(B) CRAT", font.sub = 2, cex.sub = 1.2)
plot(result3)
title(sub = "(C) PST", font.sub = 2, cex.sub = 1.2)
# CRAT
t.test(datAnd$CRAT)$statistic
library(pwr)
pwr.t2n.test(n = 39, r = -0.3950198, sig.level = 0.01282, alternative = "two.sided")

## two-way ANOVA================================================
WMC <- factor(datAnd$LevelW)
Capacity <- factor(datAnd$Group)
CRAT <- datAnd$CRAT

CCinter <- aov(CRAT ~ WMC*Capacity)
summary(CCinter)
library(HH)
windows()
interaction2wt(CRAT ~ WMC*Capacity)


## ##################### OR ####################
datOr <- read.table(file = "resultsOR2.5.txt", head = T)
## Scatterplot ===============================================
library(ggplot2)
# Ct~CRAT
orss = ggplot(datOr, aes(x = datOr$CZ, y = datOr$CRAT)) +
  geom_point() +
  #stat_smooth(method = "lm", colour = '#8470FF') +
  xlab("(A) C(t)") +
  ylab("PST.semantic") +
  theme_set(theme_bw()) +
  theme(panel.grid.major=element_line(colour=NA))
ss = lm(datOr$s~datOr$RT.single,data = datOr)
summary(ss)

# single~s
orss = ggplot(datOr, aes(x = datOr$RT.single, y = datOr$s)) +
  geom_point() +
  #stat_smooth(method = "lm", colour = '#8470FF') +
  xlab("(A) RT in single trial") +
  ylab("PST.semantic") +
  theme_set(theme_bw()) +
  theme(panel.grid.major=element_line(colour=NA))
ss = lm(datOr$s~datOr$RT.single,data = datOr)
summary(ss)

# both~s
orbs = ggplot(datOr, aes(x = datOr$RT.both, y = datOr$s)) +
  geom_point() +
  #stat_smooth(method = "lm", colour = '#8470FF') +
  xlab("(B) RT in redundant trial") +
  ylab("PST.semantic") +
  theme_set(theme_bw()) +
  theme(panel.grid.major=element_line(colour=NA))
bs = lm(datOr$s~datOr$RT.both,data = datOr)
summary(bs)

windows()
library(gridExtra)
grid.arrange(orss, orbs, orsg, orbg, ncol = 2)

# RT.single~C(t)
# AND
andsct = ggplot(datAnd, aes(x = datAnd$RT.single, y = datAnd$CZ.3, colour = Capacity, shape = Capacity)) +
  geom_point() +
  stat_smooth(method = "lm", aes(colour = "All", shape = "All")) +
  xlab("(A) RT in single trial (AND)") +
  ylab("C(t)") +
  theme_set(theme_bw()) +
  theme(panel.grid.major=element_line(colour=NA))
sca = lm(datAnd$CZ.3~datAnd$RT.single,data = datAnd)
summary(sca)
# OR
orsct = ggplot(datOr, aes(x = datOr$RT.single, y = datOr$CZ.3, colour = Capacity, shape = Capacity)) +
  geom_point() +
  stat_smooth(method = "lm", colour = '#8470FF') +
  xlab("(B) RT in single trial (OR)") +
  ylab("C(t)") +
  theme_set(theme_bw()) +
  theme(panel.grid.major=element_line(colour=NA))
sco = lm(datOr$CZ.3~datOr$RT.single,data = datOr)
summary(sco)

windows()
library(gridExtra)
grid.arrange(andsct, orsct, ncol = 2)

## r & power ==========================================
cor.test(datAnd$RT.single, datAnd$CZ.3)
cor.test(datOr$RT.single, datOr$CZ.3)
cor.test(datOr$RT.single, datOr$s)
cor.test(datOr$RT.single, datOr$g)
cor.test(datOr$RT.both, datOr$s)
cor.test(datOr$RT.both, datOr$g)

## multi anova ========================================
multiOr <- read.table(file = "multiOr.txt", head = T)
Y <- cbind(multiOr$RT.single, multiOr$RT.both)
srtanova <- manova(Y ~ multiOr$level)
summary(srtanova)

library(Rmisc)
srtanova <- summarySE(multiOr, 
                      vmeasurevar = c(multiOr$RT.single, multiOr$RT.both), 
                      groupvars = multiOr$level)
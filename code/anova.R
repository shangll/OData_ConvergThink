library('car')
CarData<-read.table(file="CarData.txt",header=TRUE)
CarData$ModelYear<-as.factor(CarData$ModelYear)
CarData$cylinders<-as.factor(CarData$cylinders)
table(CarData$cylinders)
Result<-aov(MPG~cylinders+ModelYear+cylinders:ModelYear,data=CarData)
anova(Result)

############多因素方差分析的非饱和模型
Result<-aov(MPG~cylinders+ModelYear,data=CarData)
anova(Result)

######可视化交互效应
interaction.plot(CarData$ModelYear,CarData$cylinders,CarData$MPG,type="b",main="气缸数和车型对MPG的交互效应",xlab="车型",ylab="MPG均值")

###############多因素方差分析的置换检验
CarData<-read.table(file="CarData.txt",header=TRUE)
CarData$ModelYear<-as.factor(CarData$ModelYear)
CarData$cylinders<-as.factor(CarData$cylinders)
Result<-aov(MPG~cylinders+ModelYear,data=CarData)
anova(Result)
library("lmPerm")
Fit<-aovp(MPG~cylinders+ModelYear,data=CarData)
anova(Fit)
library('car')
CarData<-read.table(file="CarData.txt",header=TRUE)
CarData$ModelYear<-as.factor(CarData$ModelYear)
CarData$cylinders<-as.factor(CarData$cylinders)
table(CarData$cylinders)
Result<-aov(MPG~cylinders+ModelYear+cylinders:ModelYear,data=CarData)
anova(Result)

############�����ط�������ķǱ���ģ��
Result<-aov(MPG~cylinders+ModelYear,data=CarData)
anova(Result)

######���ӻ�����ЧӦ
interaction.plot(CarData$ModelYear,CarData$cylinders,CarData$MPG,type="b",main="�������ͳ��Ͷ�MPG�Ľ���ЧӦ",xlab="����",ylab="MPG��ֵ")

###############�����ط���������û�����
CarData<-read.table(file="CarData.txt",header=TRUE)
CarData$ModelYear<-as.factor(CarData$ModelYear)
CarData$cylinders<-as.factor(CarData$cylinders)
Result<-aov(MPG~cylinders+ModelYear,data=CarData)
anova(Result)
library("lmPerm")
Fit<-aovp(MPG~cylinders+ModelYear,data=CarData)
anova(Fit)
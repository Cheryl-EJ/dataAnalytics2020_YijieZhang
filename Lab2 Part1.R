EPI_data <- read.csv("/Users/zhangyijie/Desktop/EPI_data.csv")
attach(EPI_data)
summary(EPI_data$EPI)
fivenum(EPI_data$EPI,na.rm=TRUE)
summary(EPI_data$DALY)
fivenum(EPI_data$DALY,na.rm=TRUE)
hist(EPI_data$EPI)
hist(EPI_data$DALY)
boxplot(ENVHEALTH,ECOSYSTEM)
qqplot(ENVHEALTH,ECOSYSTEM)
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)
cENVH
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<- data.frame(DALY = DALYNEW, AIR_H = AIR_HNEW, WATER_H = WATER_HNEW)
pENV<- predict(lmENVH,NEW,interval="prediction")
pENV
cENV<- predict(lmENVH,NEW,interval="confidence")
cENV
lmAIR_E<-lm(AIR_E~DALY+AIR_H+WATER_H)
lmAIR_E
summary(lmAIR_E)
cAIR_E<-coef(lmAIR_E)
cAIR_E
pAIR_E<- predict(lmAIR_E,NEW,interval="prediction")
pAIR_E
cAIR_E<- predict(lmAIR_E,NEW,interval="confidence")
cAIR_E
lmCLIMATE<-lm(CLIMATE~DALY+AIR_H+WATER_H)
lmCLIMATE
summary(lmCLIMATE)
cCLIMATE<-coef(lmCLIMATE)
cCLIMATE
pCLIMATE<- predict(lmCLIMATE,NEW,interval="prediction")
pCLIMATE
cCLIMATE<- predict(lmCLIMATE,NEW,interval="confidence")
cCLIMATE
EPI_data2<-EPI_data[,-c(1:5,10:13,29, 31, 39, 41, 43:44, 50:51, 54, 57, 60, 62, 67:68, 72:73, 75, 85, 88, 95, 98, 125:160)]
EPI_data3<-na.omit(EPI_data2)
correlations <- cor(EPI_data3)
correlations
cat("The single most important factor in increasing the EPI in a given region is DALY_tr", "\n")



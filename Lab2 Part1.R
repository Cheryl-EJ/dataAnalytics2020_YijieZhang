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
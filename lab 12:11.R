url<- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heartdata<- read.csv(url,header = FALSE)
head(heartdata)
colnames(heartdata) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "thal", "hd", "ca")
head(heartdata)
str(heartdata)
heartdata[heartdata == "?"] <- NA
heartdata[heartdata$sex == 0,]$sex <- "F"
heartdata[heartdata$sex == 1,]$sex <- "M"
heartdata$sex <- as.factor(heartdata$sex)
heartdata$cp <- as.factor(heartdata$cp)
heartdata$fbs <- as.factor(heartdata$fbs)
heartdata$restecg <- as.factor(heartdata$restecg)
heartdata$exang <- as.factor(heartdata$exang)
heartdata$slope <- as.factor(heartdata$slope)
heartdata$ca <- as.integer(heartdata$ca)
heartdata$ca <- as.factor(heartdata$ca)
heartdata$thal <- as.integer(heartdata$thal)
heartdata$thal <- as.factor(heartdata$thal)
heartdata$hd <- ifelse(test = heartdata$hd == "3.0", yes = "Healthy", no = "Unhealthy")
heartdata$hd <- as.factor(heartdata$hd)
str(heartdata)
nrow(heartdata[is.na(heartdata$ca)|is.na(heartdata$thal),])
heartdata[is.na(heartdata$ca)|is.na(heartdata$thal),]
nrow(heartdata)
xtabs(~heartdata$hd + heartdata$sex, data = heartdata)
xtabs(~ heartdata$hd + heartdata$cp, data = heartdata)
xtabs(~ heartdata$hd + heartdata$fbs, data=heartdata)
xtabs(~ heartdata$hd + heartdata$restecg, data=heartdata)
logis <- glm(heartdata$hd ~ heartdata$sex, data = heartdata, family = "binomial")
summary(logis)



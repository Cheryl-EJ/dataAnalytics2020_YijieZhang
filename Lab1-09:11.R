EPI_data <- read.csv("/Users/zhangyijie/Desktop/2010EPI_data.csv")
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI
tf <- is.na(EPI)
E <- EPI[!tf]
summary(EPI_data$EPI)
fivenum(EPI_data$EPI,na.rm=TRUE)
help(stem)
stem(EPI_data$EPI)
help(hist)
hist(EPI_data$EPI)
hist(EPI_data$EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI_data$EPI,na.rm=TRUE,bw=1.))
help(rug)
rug(EPI_data$EPI)
plot(ecdf(EPI_data$EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$EPI); qqline(EPI_data$EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
plot(ecdf(EPI_data$DALY), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$DALY); qqline(EPI_data$DALY)
y<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(y)
plot(ecdf(EPI_data$WATER_H), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$WATER_H); qqline(EPI_data$WATER_H)
z<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(z)
boxplot(EPI_data$EPI,DALY)
help(distributions)
EPILand<-EPI_data$EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
View(Eland)
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
EPILand<-EPI_data$DALY[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
View(Eland)
hist(Eland)
EPILand<-EPI_data$EPI_regions[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
View(Eland)
EPI_South_Asia <-EPI_data[EPI_data$EPI_regions=="South Asia"]
View(EPI_South_Asia)

EPI_data <- read.csv("/Users/zhangyijie/Desktop/EPI_data.csv")
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE) 
par(pty="s")
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) 
x <- seq(30,95,1)
x
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)
plot(ecdf(EPI_data$DALY),do.points=TRUE,verticals = TRUE) 
plot(ecdf(EPI_data$WATER_H),do.points=TRUE,verticals = TRUE) 
qqnorm(EPI_data$DALY)
qqline(EPI_data$DALY) 
qqnorm(EPI_data$WATER_H)
qqline(EPI_data$WATER_H) 
boxplot(EPI_data$EPI,EPI_data$WATER_H)
qqplot(EPI_data$EPI, EPI_data$WATER_H)



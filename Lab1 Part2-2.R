multivariate <- read.csv("/Users/zhangyijie/Desktop/multivariate.csv")
attach(multivariate)
mm<-lm(Homeowners~Immigrant)
mm
summary(mm)$coef 
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
attributes(mm)
mm$coefficients
plot(mtcars$wt, mtcars$mpg)

v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 3) 
factanal(m1, factors = 3, rotation = "promax")
prcomp(m1) 
factanal(~v1+v2+v3+v4+v5+v6, factors = 3, scores = "Bartlett")$scores

library(Hmisc)
AthleticsData <- spss.get("/Users/zhangyijie/Desktop/AthleticsData.sav")
names(AthleticsData)
cor(AthleticsData)
prcomp(AthleticsData)
fit.2 <- factanal(AthleticsData,factors=2,rotation="varimax")
print(fit.2)
fit.3 <- factanal(AthleticsData,factors=3,rotation="varimax")
print(fit.3)
print(fit.3, digits = 2, cutoff = .2, sort = TRUE)
library(psych)
fit <- principal(AthleticsData, nfactors=3, rotate="varimax")
fit 
fit.3.promax <- update(fit.3,rotation="promax") 
colnames(fit.3.promax$loadings)<-c("Endurance","Strength","Hand-Eye") 
print(loadings(fit.3.promax), digits = 2, cutoff = .2, sort = TRUE)
AssignFactorNames <- function(fit.object,names)
{
  colnames(fit.object$promax.loadings)<-names
  colnames(fit.object$varimax.loadings)<-names
  rownames(fit.object$corr.factors)<-names
  colnames(fit.object$corr.factors)<-names
}
fit.3.Enzmann <- fa(AthleticsData,nfactors=3, rotate="promax",fm="pa") 
AssignFactorNames(fit.3.Enzmann,factor.names)
fit.3.Enzmann
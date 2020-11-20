library(MASS)
data(Boston, package="MASS")
help(Boston)
help(prcomp)
pca_out <- prcomp(Boston,scale. = T)
pca_out
plot(pca_out)
help(biplot)
biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
head(boston_pc)
summary(boston_pc)
n <- 150 
p <- 2 
sigma <- 1 
meanpos <- 0 
meanneg <- 3 
npos <- round(n/2) 
nneg <- n-npos 
xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)
x <- rbind(xpos,xneg)
y <- matrix(c(rep(1,npos),rep(-1,nneg)))
plot(x,col=ifelse(y>0,1,2))
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))
ntrain <- round(n*0.8) 
tindex <- sample(n,ntrain) 
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain=rep(0,n)
istrain[tindex]=1
plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),col=c(1,1,2,2), pch=c(1,2,1,2), text.col=c(1,1,2,2))
library(e1071)
library(rpart)
data(Ozone, package="mlbench")
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex,-3])
trainset <- na.omit(Ozone[-testindex,-3])
svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
svm.pred <- predict(svm.model, testset[,-3])
crossprod(svm.pred - testset[,3]) / length(testindex)
data(iris)
attach(iris)
model <- svm(Species ~ ., data = iris)
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 
print(model)
summary(model)
pred <- predict(model, x)
pred <- fitted(model)
table(pred, y)
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)
m   <- svm(x, y)
new <- predict(m, x)
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)
m <- svm(X, gamma = 0.1)
m <- svm(~., data = X, gamma = 0.1)
m <- svm(~ a + b, gamma = 0.1)
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)
i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)
library(kernlab)
data(promotergene)
ind <- sample(1:dim(promotergene)[1],20)
genetrain <- promotergene[-ind, ]
genetest <- promotergene[ind, ]
gene <-  ksvm(Class~.,data=genetrain,kernel="rbfdot",kpar=list(sigma=0.015),C=70,cross=4,prob.model=TRUE)
genetype <- predict(gene,genetest,type="probabilities")
genetype
library(e1071) 
m1 <- matrix(c(0, 0, 0, 1, 1, 2, 1, 2, 3, 2, 3, 3, 0, 1, 2, 3, 0, 1, 2, 3, 1, 2,
               3, 2, 3, 3, 0, 0, 0, 1, 1, 2, 4, 4, 4, 4, 0, 1, 2, 3, 1, 1, 1, 1,
               1, 1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1, 1, -1, -1), ncol = 3 ) 
Y = m1[,3] 
X = m1[,1:2] 
df = data.frame( X , Y ) 
par(mfcol=c(4,2)) 
for( cost in c(1e-3, 1e-2, 1e-1, 1e0, 1e+1, 1e+2, 1e+3)) { 
  model.svm <- svm( Y ~ . , data = df, type = "C-classification", kernel = "linear", cost = cost, scale =FALSE ) 
print(model.svm$SV) 
plot(x=0,ylim=c(0,5), xlim=c(0,3),main= paste( "cost: ",cost, "#SV: ", nrow(model.svm$SV))) 
  points(m1[m1[,3]>0,1], m1[m1[,3]>0,2], pch=3, col="green") 
  points(m1[m1[,3]<0,1], m1[m1[,3]<0,2], pch=4, col="blue") 
  points(model.svm$SV[,1],model.svm$SV[,2], pch=18 , col = "red") 
}
data(spam)
index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]
filter <- ksvm(type~.,data=spamtrain,kernel="rbfdot",kpar=list(sigma=0.05),C=5,cross=3)
filter
mailtype <- predict(filter,spamtest[,-58])
table(mailtype,spamtest[,58])
data(iris)
rbf <- rbfdot(sigma=0.1)
rbf
irismodel <- ksvm(Species~.,data=iris,type="C-bsvc", kernel=rbf,C=10,prob.model=TRUE)
irismodel
fitted(irismodel)
predict(irismodel, iris[,-5], type="probabilities")
x <- rbind(matrix(rnorm(120),,2),matrix(rnorm(120,mean=3),,2))
y <- matrix(c(rep(1,60),rep(-1,60)))
svp <- ksvm(x,y,type="C-svc")
plot(svp,data=x)
K <- as.kernelMatrix(crossprod(t(x)))
svp2 <- ksvm(K, y, type="C-svc")
svp2
xtest <- rbind(matrix(rnorm(20),,2),matrix(rnorm(20,mean=3),,2))
Ktest <- as.kernelMatrix(crossprod(t(xtest),t(x[SVindex(svp2), ])))
predict(svp2, Ktest)
k <- function(x,y) {(sum(x*y) +1)*exp(-0.001*sum((x-y)^2))}
class(k) <- "kernel"
data(promotergene)
gene <- ksvm(Class~.,data=promotergene[c(1:20, 80:100),], kernel=k, C=5, cross=5)
gene
data(reuters)
is(reuters)
tsv <- ksvm(reuters,rlabels,kernel="stringdot", kpar=list(length=5),cross=3,C=10)
tsv
x <- seq(-20,20,0.1)
y <- sin(x)/x + rnorm(401,sd=0.03)
regm <- ksvm(x,y,epsilon=0.01,kpar=list(sigma=16),cross=3)
plot(x,y,type="l")
lines(x,predict(regm,x),col="red")
k <- function(x,y) {(sum(x*y) +1)*exp(-0.001*sum((x-y)^2))}
class(k) <- "kernel"
data(promotergene)
gene <- ksvm(Class~.,data=promotergene[c(1:20, 80:100),], kernel=k, C=5, cross=5)
gene
svp <- ksvm(xtrain,ytrain,type="C-svc",kernel='vanilladot',C=100,scaled=c())
svp
attributes(svp)
alpha(svp)
alphaindex(svp)
b(svp)
dev.off()
plot(svp,data=xtrain)
cv.folds <- function(n,folds=3)
{split(sample(n),rep(1:3,length=length(y)))}
svp <- ksvm(x,y,type="C-svc",kernel= "vanilladot", C=1,scaled=c(),cross=5)
print(cross(svp))



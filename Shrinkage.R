library(faraway)
data(fat)
index <- seq(10,250,by=10)
train <- fat[-index,-c(1,3,8)]
test <- fat[index,-c(1,3,8)]
ans<-matrix(nrow=1,ncol=3)


#Linear Regression
g1 <- lm(siri~.,data=train)
rmse <- function(x,y){sqrt(mean((x-y)^2))}
rmse(g1$fit,train$siri)
rmse(predict(g1,test),test$siri)
ans[1,1]<-rmse(predict(g1,test[,-1]),test$siri)



#PCA
library(HSAUR2)
library(MVA)
fatpca <- prcomp(train[,-1])
round(fatpca$sdev,3)

#Look for best number of PCs to use
mm <-apply(train[,-1],2,mean)
testx <-as.matrix(sweep(test[,-1],2,mm))

rmsfat <- NULL
for( i in 1:14){
nx <- testx%*% fatpca$rot[,1:i]
g2 <- lm(siri~fatpca$x[,1:i],train)
pv <- cbind(1,nx) %*% g2$coef
rmsfat[i] <- rmse(pv,test$siri)
}
which.min(rmsfat)
min(rmsfat)
ans[1,2]<-min(rmsfat)


#Ridge Regression
library(MASS)
trainx <-as.matrix(sweep(train[,-1],2,mm))
trainy <-train$siri-mean(train$siri)
gridge <- lm.ridge(siri~.,train,lambda=seq(0,10,1e-3))
which.min(gridge$GCV)
ans[1,3]<-rmse(scale(testx,FALSE,gridge$scales)%*%gridge$coef[,1088]+
mean(train$siri),test$siri)

#Results report
ans<-as.data.frame(ans)
names(ans)<-c("IR","PCR","Ridge")
ans
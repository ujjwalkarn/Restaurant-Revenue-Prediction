#install and load packages
install.packages(c("e1071","lubridate"))
library(e1071)
library(lubridate)

#read the data
train <- read.csv("train.csv",header=TRUE)
test <- read.csv("test.csv",header=TRUE)

#convert date field to separate columns
train$day<-as.factor(day(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$month<-as.factor(month(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$year<-as.factor(year(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))

test$day<-as.factor(day(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))
test$month<-as.factor(month(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))
test$year<-as.factor(year(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))

#select relevant columns
train_cols<-train[,c(3:42,44:46)]
labels<-as.matrix(train[,43])
testdata<-test[,3:45]

#convert all columns to numeric
#please note that this is not advised as the categorical factors are converted to numeric as well 
#encoding each factor level as a distinct integer is not recommended, use other methods such as one-hot encoding
train_cols <- data.frame(lapply(train_cols,as.numeric))
testdata<-data.frame(lapply(testdata,as.numeric))

#run support vector regression and predict on test data
fit<- svm(x=as.matrix(train_cols),y=labels,cost=10,scale=TRUE,type="eps-regression")
predictions<-as.data.frame(predict(fit,newdata=testdata))

#create submission file
submit<-as.data.frame(cbind(test[,1],predictions))
colnames(submit)<-c("Id","Prediction")
write.csv(submit,"submission.csv",row.names=FALSE,quote=FALSE)

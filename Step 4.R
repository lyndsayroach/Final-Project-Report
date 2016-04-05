#necessary commands from first step
data<-read.csv("C:/Users/source/Documents/train.csv",header=TRUE,sep=",")
data$Choice<-as.factor(data$Choice)
#split training data into training and testing sets 
rn_train<-sample(nrow(data),floor(nrow(data)*0.7))
trainNaive<-data[rn_train,]
testNaive<-data[-rn_train,]
#fit data to naive bayes model
library(e1071)
model2<-naiveBayes(Choice~.,data=trainNaive)
summary(model2)
#make confusion matrix to evaluate errors
pred<-predict(model2,newdata=testNaive)
table(pred,testNaive$Choice)
#make table with accurary percentages 
table(pred==testNaive$Choice)/length(testNaive$Choice)

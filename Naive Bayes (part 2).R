train2<-read.csv("C:/Users/source/Documents/train.csv",header=TRUE,sep=",")
train2$Choice<-as.factor(train2$Choice)
options(scipen=999)
train<-train[c(-3,-9,-14,-20)]
#split training data into training and testing sets 
rn_train<-sample(nrow(train),floor(nrow(train)*0.7))
trainNaive<-train[rn_train,]
testNaive<-train[-rn_train,]
#fit data to naive bayes model
library(e1071)
model2<-naiveBayes(Choice~.,data=trainNaive)
summary(model2)
#make confusion matrix to evaluate errors
pred<-predict(model2,newdata=testNaive)
table(pred,testNaive$Choice)
#make table with accurary percentages 
table(pred==testNaive$Choice)/length(testNaive$Choice)
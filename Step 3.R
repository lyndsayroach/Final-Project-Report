#necessary commands from first step
data<-read.csv("C:/Users/source/Documents/train.csv",header=TRUE,sep=",")
data$Choice<-as.factor(data$Choice)
#create formula where the dependent variable is Choice
tmp<-paste(names(data[1]),"~",paste(names(data[2:23]),collapse="+"))
formula<-as.formula(tmp)
formula
#split training data into training and testing sets 
rn_train<-sample(nrow(data),floor(nrow(data)*0.7))
trainLogit<-data[rn_train,]
testLogit<-data[-rn_train,]
#fit logistic regression model for training data
model<-glm(formula,data=trainLogit,family="binomial")
summary(model)
#make confusion matrix to evaluate errors
pred<-predict(model,newdata=testLogit,type="response")
pred_model<-rep("0",1651)
pred_model[pred>0.5]<-"1"
table(pred_model,testLogit$Choice)
#make table with accurary percentages 
table(pred_model==testLogit$Choice)/length(testLogit$Choice)
#install necessary packages
library(ROCR)
library(caret)
pred2<-prediction(pred,testLogit$Choice)
perf<-performance(pred2,"tpr","fpr")
plot(perf,main="ROC Curve")
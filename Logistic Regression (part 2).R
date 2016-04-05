train<-read.csv("C:/Users/source/Documents/train.csv",header=TRUE,sep=",")
train$Choice<-as.factor(train$Choice)
#removed A posts and B posts
train<-train[c(-9,-20)]
str(train)
#create formula where the dependent variable is Choice
tmp<-paste(names(train[1]),"~",paste(names(train[2:21]),collapse="+"))
formula<-as.formula(tmp)
formula
#split training data into training and testing sets 
rn_train<-sample(nrow(train),floor(nrow(train)*0.7))
trainLogit<-train[rn_train,]
testLogit<-train[-rn_train,]
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
#remove A and B following count, as well as, A and B mentions received
train<-read.csv("C:/Users/source/Documents/train.csv",header=TRUE,sep=",")
train$Choice<-as.factor(train$Choice)
options(scipen=999)
train<-train[c(-3,-5,-9,-14,-16,-20)]
str(train)
#create formula where the dependent variable is Choice
tmp<-paste(names(train[1]),"~",paste(names(train[2:17]),collapse="+"))
formula<-as.formula(tmp)
formula
#split training data into training and testing sets 
rn_train<-sample(nrow(train),floor(nrow(train)*0.7))
trainLogit<-train[rn_train,]
testLogit<-train[-rn_train,]
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
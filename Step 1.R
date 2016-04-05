#import data and look at its structure
data<-read.csv("C:/Users/source/Documents/train.csv",header=TRUE,sep=",")
str(data)
#reassign attribute Choice as a factor because it is categorical data
data$Choice<-as.factor(data$Choice)
str(data)
#check for missing values
anyNA(data)

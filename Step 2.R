#necessary commands from first step
data<-read.csv("C:/Users/source/Documents/train.csv",header=TRUE,sep=",")
data$Choice<-as.factor(data$Choice)
#start by looking at dimensions of dataset and summary of dataset
dim(data)
head(data)
#brief descriptive statistics
#correlation of network features
cor(data$A_network_feature_2,data$A_network_feature_3,method="pearson")
cor(data$A_network_feature_1,data$A_network_feature_3,method="pearson")
cor(data$A_network_feature_2,data$A_network_feature_1,method="pearson")
cor(data$B_network_feature_2,data$B_network_feature_3,method="pearson")
cor(data$B_network_feature_1,data$B_network_feature_3,method="pearson")
cor(data$B_network_feature_2,data$B_network_feature_1,method="pearson")
cor(data$A_network_feature_1,data$B_network_feature_1,method="pearson")
cor(data$A_network_feature_2,data$B_network_feature_2,method="pearson")
cor(data$A_network_feature_3,data$B_network_feature_3,method="pearson")
#look at summary of data for means 
summary(data)
#standard deviations
stdA<-matrix(c(sd(data$A_follower_count),sd(data$A_following_count),sd(data$A_listed_count),sd(data$A_mentions_received),sd(data$A_retweets_received),sd(data$A_mentions_sent),sd(data$A_retweets_sent),sd(data$A_posts)),ncol=1,byrow=TRUE)
rownames(stdA)<-c("A follower count","A following count","A listed count","A mentions received","A retweets receved","A mentions sent","A retweets sent","A post")
colnames(stdA)<-c("Standard deviation")
stdA<-as.table(stdA)
stdA
stdB<-matrix(c(sd(data$B_follower_count),sd(data$B_following_count),sd(data$B_listed_count),sd(data$B_mentions_received),sd(data$B_retweets_received),sd(data$B_mentions_sent),sd(data$B_retweets_sent),sd(data$B_posts)),ncol=1,byrow=TRUE)
rownames(stdB)<-c("B follower count","B following count","B listed count","B mentions received","B retweets receved","B mentions sent","B retweets sent","B post")
colnames(stdB)<-c("Standard deviation")
stdB<-as.table(stdB)
stdB
#split dataset into two based on labels and removed label column
data1<-subset(data,Choice==1) 
data0<-subset(data,Choice==0)
data1<-data1[c(-1)]
data0<-data0[c(-1)]
#compare various medians
#make table for records where individual A is more influencial, where table includes the median of individual A's follower and following count and individual B's follower and following count
followersA1<-c(median(data1$A_follower_count),median(data1$A_following_count))
followersB1<-c(median(data1$B_follower_count),median(data1$B_following_count))
followersMedian1<-rbind(followersA1,followersB1)
colnames(followersMedian1)<-c("Follower Count Median","Following Count Median")
followersMedian1
#here we can assume that the Follower Count of the more influencial individual, in this case A, is significant
#will explore follower/following medians for when B is the more influencial individual to see if this assumption holds
followersA0<-c(median(data0$A_follower_count),median(data0$A_following_count))
followersB0<-c(median(data0$B_follower_count),median(data0$B_following_count))
followersMedian0<-rbind(followersA0,followersB0)
colnames(followersMedian0)<-c("Follower Count Median","Following Count Median")
followersMedian0
#explore mentions received/mentions sent medians
mentionsA1<-c(median(data1$A_mentions_received),median(data1$A_mentions_sent))
mentionsB1<-c(median(data1$B_mentions_received),median(data1$B_mentions_sent))
mentionsMedian1<-rbind(mentionsA1,mentionsB1)
colnames(mentionsMedian1)<-c("Mentions Received Median","Mentions Sent Median")
mentionsMedian1
mentionsA0<-c(median(data0$A_mentions_received),median(data0$A_mentions_sent))
mentionsB0<-c(median(data0$B_mentions_received),median(data0$B_mentions_sent))
mentionsMedian0<-rbind(mentionsA0,mentionsB0)
colnames(mentionsMedian0)<-c("Mentions Received Median","Mentions Sent Median")
mentionsMedian0
#explore retweets received/retweets sent medians
retweetsA1<-c(median(data1$A_retweets_received),median(data1$A_retweets_sent))
retweetsB1<-c(median(data1$B_retweets_received),median(data1$B_retweets_sent))
retweetsMedian1<-rbind(retweetsA1,retweetsB1)
colnames(retweetsMedian1)<-c("Retweets Received Median","Retweets Sent Median")
retweetsMedian1
retweetsA0<-c(median(data0$A_retweets_received),median(data0$A_retweets_sent))
retweetsB0<-c(median(data0$B_retweets_received),median(data0$B_retweets_sent))
retweetsMedian0<-rbind(retweetsA0,retweetsB0)
colnames(retweetsMedian0)<-c("Retweets Received Median","Retweets Sent Median")
retweetsMedian0
#data visualization
#install necessary package
library(lattice)
xyplot(A_retweets_received~A_posts | Choice, data=data, xlim=c(0,20),ylim=c(0,5000),main="Effect of Number of Posts on Retweets for user A")
xyplot(B_retweets_received~B_posts | Choice, data=data, xlim=c(0,20),ylim=c(0,5000),main="Effect of Number of Posts on Retweets for user B")
xyplot(A_retweets_received~A_listed_count | Choice, data=data, xlim=c(0,20000),ylim=c(0,1000),main="Effect of Number of Lists on Retweets for user A")
xyplot(B_retweets_received~B_listed_count | Choice, data=data, xlim=c(0,20000),ylim=c(0,1000),main="Effect of Number of Lists on Retweets for user B")
#repeat previous 8 plots with subset of first 100 records
dataTemp<-data[1:100,]
xyplot(A_retweets_received~A_posts | Choice, data=dataTemp, xlim=c(0,20),ylim=c(0,4000),main="Effect of Number of Posts on Retweets for user A",type = c("p", "smooth"),col.line="red")
xyplot(B_retweets_received~B_posts | Choice, data=dataTemp, xlim=c(0,20),ylim=c(0,1000),main="Effect of Number of Posts on Retweets for user B",type = c("p", "smooth"),col.line="red")
xyplot(A_retweets_received~A_listed_count | Choice, data=dataTemp, xlim=c(0,20000),ylim=c(0,1000),main="Effect of Number of Lists on Retweets for user A",type = c("p", "smooth"),col.line="red")
xyplot(B_retweets_received~B_listed_count | Choice, data=dataTemp, xlim=c(0,20000),ylim=c(0,1000),main="Effect of Number of Lists on Retweets for user B",type = c("p", "smooth"),col.line="red")
#look at example of skewness of data using Q-Q plot 
dataTemp1<-subset(dataTemp, Choice==1)
qqnorm(dataTemp1$A_mentions_received,main="Normal Q-Q Plot: A Mentions Received")
qqline(dataTemp1$A_mentions_received,col="red")
#look at same example of skewness of data using a desnity function
plot(density(dataTemp1$A_mentions_received),main="Density Plot of A Mentions Recevied",xlab="A mentions received",ylab="Density")
setwd("C:/Users/SHIVAM BHAGWANI/Desktop/Winter Sem 2018-19/Data Mining/Data Mining Project")
data<-read.csv('housing.csv')
head(data)
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
hist(data$population, breaks = 20)
hist(data$housing_median_age, breaks = 20)
hist(data$median_income, breaks = 20)
hist(data$households, breaks = 20)

library(corrplot)
par(mfrow=c(1,1))
corrplot(cor(as.matrix(data[,-10])))

library(moments)
sapply(data[,-10], function(x) skewness(x))
sapply(data[,-10], function(x) var(x))

data2<-data2[,-9]
head(data2)
library(caret)
predata<-preProcess(x=data2, method=c("BoxCox", "scale"))
data2<-predict(predata,newdata=data2)

sapply(data2, function(x) skewness(x))

par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
hist(data2$population, breaks = 20)
hist(data2$housing_median_age, breaks = 20)
hist(data2$median_income, breaks = 20)
hist(data2$households, breaks = 20)

sample<-data.frame(t(na.omit(data2)))
pcaModel<-prcomp(sample,retx=TRUE)
head(pcaModel)
summary(pcaModel)
par(mfrow=c(1,1))
screeplot(pcaModel)

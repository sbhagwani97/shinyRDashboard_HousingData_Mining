setwd("C:/Users/SHIVAM BHAGWANI/Desktop/Winter Sem 2018-19/Data Mining/Data Mining Project")
data<-read.csv('housing.csv')
colnames(data)
#data[,10]
set.seed(2)
id<-sample(2, nrow(data), prob=c(0.3,0.7), replace=T)
data_train<-data[id==1,]
data_test<-data[id==2,]
nrow(data_train)
nrow(data_test)
#data_train[,10]
#data_test[,10]
library(caret)
library(rpart)
tree_model<-rpart(ocean_proximity~longitude+latitude+population+median_house_value+households, data=data_train)
tree_model
plot(tree_model, margin=0.1)
text(tree_model, use.n=TRUE, pretty=TRUE, cex=0.8)
#test<-data.frame(-122, 39, 300, 230000, 200)
#colnames(test) <- c("longitude","latitude","population","median_house_value","households")
#test
pred<-predict(tree_model, newdata=data_test, type='class')
pred
confusionMatrix(table(pred, data_test$ocean_proximity))

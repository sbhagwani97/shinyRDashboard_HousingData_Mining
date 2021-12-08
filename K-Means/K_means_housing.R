setwd("C:/Users/SHIVAM BHAGWANI/Desktop/Winter Sem 2018-19/Data Mining/Data Mining Project")
data<-read.csv("housing.csv")
data$ocean_proximity<-NULL
View(data)
sum(is.na(data))
data<-na.omit(data)
sum(is.na(data))
results<-kmeans(data, 3)
results #may give different results each time- initialization of cluster centers.
results[2] #center values for longitude and latitude not so far away from each other
#table(data$ocean_proximity, results)
library(ggplot2)
ggplot(data, aes(x=data$median_house_value, y=data$households, col=results$cluster))+
  geom_point()

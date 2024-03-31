data <- read.csv("Mall_Customers.csv",sep=",")
library(tidyverse)

#scale the data
newdata <- data %>%
  mutate(
    gender = ifelse(Gender=='Male',1,0),
    #age = (Age - min(Age)) / (max(Age) - min(Age)),
    annual.income = (AnnualIncome - min(AnnualIncome)) / (max(AnnualIncome) - min(AnnualIncome)),
    spending.score = (SpendingScore - min(SpendingScore)) /(max(SpendingScore) - min(SpendingScore))
  )

view(newdata)
newdata<-newdata[,-c(2,4,5)]
ggplot(newdata, aes(x=spending.score, y=annual.income)) +
  geom_point()

#elbow method to find k
fviz_nbclust(newdata[,4:5], 
             FUNcluster = kmeans,# K-Means
             method = "wss",     # total within sum of square
             k.max = 20          # max number of clusters to consider
) 

# Average silhouette for kmeans
fviz_nbclust(newdata[,4:5], FUN = kmeans, method = "silhouette")
fviz_nbclust(newdata[,4:5], FUN = kmeans, method = "wss")

km <- kmeans(newdata[,4:5], centers=5,nstart=20)

plot(km, data=newdata, class="gender")
table(km$cluster)
cc = km$cluster
library(factoextra)
fviz_cluster(km,           # 分群結果
             data = newdata[,4:5],              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             ellipse.type = "norm")      # 框架型態

data = cbind(data,cc)
ggplot(data, aes(x=SpendingScore, y=AnnualIncome,color=as.factor(cc))) +
  geom_point()
ggplot(data, aes(x=as.factor(cc), y=Age)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cc), y=AnnualIncome)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cc), y=SpendingScore)) + 
  geom_boxplot()
ggplot( data =data) +
  geom_bar( aes( x = Gender)) + 
  facet_wrap( ~ cc)



#hc method
E.dist <- dist(newdata[,2:5], method = "euclidean")
tree1 <- hclust(E.dist, method = "ward.D2")
plot(tree1, xlab="Euclidean",h=-1, cex = 0.6)
abline(h=10,col="red")

fviz_dist(E.dist,gradient = list(low = "red", mid = "pink", high = "white")) #heatmap
fviz_nbclust(newdata[,2:5], FUN = hcut, method = "wss")

cluster <- cutree(tree1, k = 5)
table(cluster)

data = cbind(data,cluster)
ggplot(data, aes(x=SpendingScore, y=AnnualIncome,color=as.factor(cluster))) +
  geom_point()
ggplot(data, aes(x=as.factor(cluster), y=Age)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cluster), y=AnnualIncome)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cluster), y=SpendingScore)) + 
  geom_boxplot()
ggplot( data =data) +
  geom_bar( aes( x = Gender)) + 
  facet_wrap( ~ cluster)



#try another clustering method
dist <- get_dist(newdata[,c(2:5)], method="pearson")
fviz_dist(dist,gradient = list(low = "red", mid = "pink", high = "white")) #heatmap

fviz_nbclust(newdata[,c(2:5)], FUN = hcut, method = "silhouette")

tree1 <- hclust(dist, method="ward.D2")
plot(tree1, xlab="pearson",h=-1)
cluster <- cutree(tree1, k=4) 
table(cluster)
rect.hclust(tree1,k=4,border="red")

#EDA
data = cbind(data,cluster)
ggplot(data, aes(x=as.factor(cluster), y=Age)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cluster), y=AnnualIncome)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cluster), y=SpendingScore)) + 
  geom_boxplot()
ggplot( data =data) +
  geom_bar( aes( x = Gender)) + 
  facet_wrap( ~ cluster)



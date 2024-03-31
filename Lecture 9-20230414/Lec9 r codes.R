library(datasets)
data(iris)

################
# UNSUPERVISED #
################
### Kmean ###
k = kmeans(iris[,1:4], centers=3) # k=3
k = kmeans(iris[,1:4], centers=3, nstart=25) # k=3, generate 25 initial configurations 重複做25次
str(k)
####
#cluster: a vector of integers (from 1:k) indicating the cluster to which each point is allocated.
#centers: a matrix of cluster centers.
#withinss: vector of within-cluster sum of squares, one component per cluster.
#tot.withinss: total within-cluster sum of squares. That is, sum(withinss).
#size: the number of points in each cluster.
#####
k$centers
k$withinss #within class variance
k$tot.withinss
table(k$cluster, iris$Species)  
k$size # cluster size

# plot the results
install.packages("useful")
library(useful)
#library(tidyverse)
plot(k,data=iris,class="Species")
## or
install.packages("factoextra")
library(factoextra)
fviz_cluster(k,           # 分群結果
             data = iris[,1:4],              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             ellipse.type = "norm")      # 框架型態

## or
library(tidyverse)
fviz_cluster(k, data = iris[,-5])


# Elbow Method for K-Means (find the best k by SSE) 手肘方法 k unknown
fviz_nbclust(iris[,1:4], 
             FUNcluster = kmeans,# K-Means
             method = "wss",     # total within sum of square
             k.max = 20          # max number of clusters to consider
) +
  labs(title="Elbow Method for K-Means")+
  geom_vline(xintercept = 3, linetype = 2)

# another method is method="silhouette" (Average silhouette Method)
# +geom_vline(xintercept = 3, linetype = 2) # draw a vertical line at k=3

fviz_nbclust(iris[,-5], kmeans, method = "silhouette")

# Compute gap statistic for kmeans
# Recommended value for B is ~500
library(cluster)
gap_stat <- clusGap(iris[,-5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 500)
fviz_gap_stat(gap_stat)


### HC tree ###
E.dist <- dist(iris[,1:4], method="euclidean") # 歐式距離
M.dist <- dist(iris[,1:4], method="manhattan") # 曼哈頓距離
#other function: get_dist(iris[,1:4], method="euclidean"), 
#get_dist() has some correlation measures

# visualize
fviz_dist(E.dist,gradient = list(low = "red", mid = "pink", high = "white")) #heatmap

# By Euclidean Distance
tree1 <- hclust(E.dist, method="ward.D2")
plot(tree1, xlab="Euclidean",h=-1)
abline(h=10,col="red")

# Gap statistic for hierarchical clustering
fviz_nbclust(iris[,1:4], FUN = hcut, method = "wss")

# By Manhattan Distance
tree2 <- hclust(M.dist) 
plot(tree2, xlab="Manhattan")

cluster <- cutree(tree1, k=3)  # 分成三群, can cut by h=10 too
cluster    
table(cluster)
which(cluster==1)

rect.hclust(tree1,k=3,border="red")

table(cluster, iris$Species)       # 分群結果和實際結果比較


# find the best cluster size
p = fviz_nbclust(iris[,1:4], 
             FUNcluster = hcut,  # hierarchical clustering
             method = "wss",     # total within sum of square
             k.max = 20          # max number of clusters to consider
) 
p
(p = p + labs(title="Elbow Method for HC") )
  
p + geom_vline(xintercept = 3,       # 在 X=3的地方 
             linetype = 2)   

# Agglomerative coefficient
# Compute with agnes
library(cluster)
ac_ward <- agnes(E.dist, method = "ward")
ac_ward$ac

pltree(ac_ward, hang = -1) #, main="tree"

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(E.dist, method = x)$ac
}

sapply(m,ac)

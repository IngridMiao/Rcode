#######################
# dimension reduction #
trip <- read.csv("tripadvisor_review.csv")
data = trip[,-1] #去掉第一行

## understand our data
M = cor(data)

## create heatmap for correlation 
library(reshape2)
melted_cormat <- melt(M)  #算corr.
head(melted_cormat)

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


ggplot(data = melted_cormat,
       aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

heatmap(M)
library(corrplot)
corrplot(M, method="circle")

## PCA ##-------------------------------------------------------------------
#install.packages("stats")
library(stats)
pca<- prcomp(data, center = TRUE, scale = TRUE) #scale要不要標準化
#center data with mean =0. scale=T means standard deviation is set 1.
names(pca) #sdev=std. deviation, rotation=coefficient主成份的係數矩陣
summary(pca)
#the first two components account for over 42.40% of 
#the variance in the entire data set.

plot(pca) #Variation	explained
#first component is the one with the highest variance 
#and second will be the second highest in variance

## how to get the results of summary(pca)
var = pca$sdev^2 #該主成份解釋變異數的數值
prop = (pca$sdev)^2 / sum((pca$sdev)^2) #該主成份解釋變異數的比率
cum_prop = cumsum((pca$sdev)^2 / sum((pca$sdev)^2)) #該主成份解釋變異數的累積比率

## scree plot: variance
screeplot(pca) #same as plot(pca)
plot(pca, type="line")
abline(h=1, col="blue") #Kaiser eigenvalue-greater-than-one rule, choose pc1~pc5 by Kaiser

summary(pca)



## Rotation matrix: Loadings are the percent of variance explained by the variable
pca$rotation

#visualize
ggplot(melt(pca$rotation[,1:5]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())



## The actual principal components
head(pca$x)

## Standard deviation of components is represents the percent of variation each component explains
pca$sdev

## Compute variance explained
pve=(pca$sdev)^2 / (sum(pca$sdev^2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')#scree plot
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')
abline(h=0.8)

## pc1 vs pc2 plot
biplot(pca,scale=T) #first two components
biplot(pca,scale=T,choices=2:3)
# another way 
install.packages("ggfortify")
library(ggfortify)
autoplot(prcomp(data, center = TRUE, scale = TRUE), 
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 5)

# by cluster 
group = as.factor(cutree(hclust(dist(data)),k=3))
data2 = cbind(data, group)
autoplot(prcomp(data, center = TRUE, scale = TRUE), data2, colour = 'group', 
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3)



## SPCA ##-------------------------------------------------------------------
install.packages("nsprcomp")
library(nsprcomp)
spca <- nscumcomp(data, k=40, nneg=T, scale=T)
summary(spca)
#k：非 0 係數個數，通常是「每個主成份期待非 0 係數個數」x 變數個數
#nneg：是否希望所有係數都非負，TRUE 代表有非負限制
screeplot(spca)

ggplot(melt(spca$rotation), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "white", high = "steelblue") +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())



## Image Compression ##--------------------------------------------------------
#######################################################################
# The aim of this experiment was to use the PCA technique as an image #
# compression technique to reduce the size of the image by removing   #
# some redundant information.                                         #
#######################################################################
library(jpeg)

####### read photo #######
photo <- readJPEG("pic2.jpeg")
dim(photo)
####### Creating separated matrix for every RGB color scale #######
r <- photo[,,1]
g <- photo[,,2]
b <- photo[,,3]

####### Introducing PCA method #######
r.pca <- prcomp(r, center = F)
g.pca <- prcomp(g, center = F)
b.pca <- prcomp(b, center = F)

rgb.pca <- list(r.pca, g.pca, b.pca)



for (i in round(seq.int(3, nrow(photo), length.out = 20))) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('photo_', round(i,0), '_components.jpg', sep = ''))
}



#
o = file.info('pic2.jpeg')$size / 1000
n = file.info('photo_1270_components.jpg')$size / 1000 
cat( 'file size decrease', abs(round((n-o)/o , 2 )*100), '%', sep=' ')

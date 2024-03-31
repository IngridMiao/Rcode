####Exercise----iris data------------------------------------------
data(iris)
pca.iris=prcomp(iris[,1:4], center = TRUE, scale = TRUE) 
summary(pca.iris)
pca.iris$rotation #PC2 is	dominated	by Sepal.Width

plot(pca.iris, type="line")

biplot(pca.iris)

autoplot(prcomp(iris[,1:4], center = TRUE, scale = TRUE),data = iris, colour = 'Species', 
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3)

plot.pca <- cbind(iris, pca.iris$x)
ggplot(plot.pca, aes(x = PC1, y = PC3, color = Species)) + geom_point() 


#
install.packages("HDclassif")
library(HDclassif)
data(wine)
wine$class <- factor(wine$class)
pca=prcomp(wine[,-1], center = TRUE, scale = TRUE) 
plot(pca, type="line")
avg_var = mean(pca$sdev^2) 
abline(h=avg_var, col="blue")  #3 PCs


ggplot(melt(pca$rotation[,1:3]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

biplot(pca)

autoplot(pca, data = wine, colour = 'class', 
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3)


#
spca <- nscumcomp(wine[,-1], k=80, nneg=T, scale=T)
summary(spca)
biplot(spca)

ggplot(melt(spca$rotation), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "white", high = "steelblue") +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())


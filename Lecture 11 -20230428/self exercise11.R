#exercise
##
data(iris)
library(stats)
pca<- prcomp(data, center = TRUE, scale = TRUE)
summary(pca)
screeplot(pca) #same as plot(pca)
plot(pca, type="line")
abline(h=1, col="blue")

pca$rotation

ggplot(melt(pca$rotation[,1:5]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())


install.packages("HDclassif")
library(HDclassif)
data(wine)

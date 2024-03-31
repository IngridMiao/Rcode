library(AER)
data("CreditCard")
pairs(CreditCard, col=CreditCard$card)

#decision tree
n <- nrow(CreditCard)
trains <- sample(1:n, size = round(0.5*n))
trained <- CreditCard[trains,]
tested <- CreditCard[-trains,]

library(rpart)
trees <- rpart(card ~. ,data=trained, method="class")
preds <- predict(trees, newdata=tested, type="class")
table(Real = tested$card, Predict = preds)

library(rpart.plot)
rpart.plot(trees)
rpart.rules(trees,cover=T)


#Random Forest
library(randomForest)
rfs <- randomForest(card ~ ., data = trained, importance = TRUE)
plot(rfs)

pred=predict(rfs, newdata = tested)
table(Real = tested$card, Predict = pred)

#

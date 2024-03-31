library(AER)

data(CreditCard)
CreditCard$card <- as.factor(ifelse(CreditCard$card == "yes", 1, 0))


n <- nrow(CreditCard)
trainI <- sample(1:n,  size = round(0.7*n))
traind <- CreditCard[trainI,]
testd <- CreditCard[-trainI,]

#Decision tree 
dtree <- rpart(formula = card~., data = traind, method="class")
pred <- predict(dtree, newdata=testd, type="class")
table(Real = testd$card, Predict = pred)
rpart.plot(dtree)

#Random Forest
rf <- randomForest(card ~., data = traind, importance=TRUE, ntree=100) 
importance(rf)
table(Real = testd$card, Predict = pred)


#SVM
s_rbf <- svm(card ~ ., data = traind, probability = TRUE,kernel="radial")

svm_tune <- tune(svm, card ~ .,data=traind, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune) #or
after_tune <- svm(card ~ ., data=traind, kernel="radial", cost=1, gamma=0.5)
pred <- predict(after_tune,testd)
table(Real = testd$card, Predict = pred)


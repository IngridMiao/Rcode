library(datasets)
library(MASS)
library(tidyverse)
data(iris)
head(iris)
pairs(iris, col=iris$Species)

################
# SUPERVISED   #
################


# training and test data
trainI <- sample(1:150, 75)
traind <- iris[trainI,]
testd <- iris[-trainI,]
#or
iris$index =c(1:nrow(iris))
train_df <- iris %>% group_by(Species) %>% sample_frac(0.8)
test_df  <- anti_join(iris, train_df, by = 'index')
traind <-as.data.frame(train_df[,-6])
testd <-as.data.frame(test_df[,-6])

### KNN ###
library(class)
pred=knn(traind[,1:4], testd[,1:4], cl=traind[,5], k = 6) #prob=T cl為class
table(real=testd[,5], pred)

#choose k
range <- 1:round(0.2 * nrow(traind)) #通常 k 的上限為訓練樣本數的 20%
accuracies <- rep(NA, length(range))

for (i in range) {
  test_predicted <- knn(train = traind[,1:4], test = testd[,1:4], cl = traind[,5], k = i)
  conf_mat <- table(testd$Species, test_predicted)
  accuracies[i] <- sum(diag(conf_mat))/sum(conf_mat)
}

##視覺化上面結果
plot(range, accuracies, xlab = "k")
which.max(accuracies) #k


### Decision tree ###
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
tree <- rpart(Species ~. ,data=traind, method="class") #inside part: y~X1+X2+...
pred <- predict(tree, newdata=testd, type="class")
table(Real = testd$Species, Predict = pred)

library() 
rpart.plot(tree)
rpart.rules(tree,cover=T)


# another way to get a tree
install.packages("tree")
library(tree)
tree2 <- tree(Species ~ ., data = traind)
summary(tree2)rpart.plot
pred <- predict(tree2, newdata=testd, type="class")
table(Real = testd$Species, Predict = pred)
plot(tree2)
text(tree2)

# CP
printcp(tree)
?rpart.control
# 修剪樹(Post-Pruning)
tree_prune <- prune(tree, cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
rpart.plot(tree_prune)
table(Real = testd$Species, Predict = pred)



### Random Forest ###

install.packages("randomForest")
library(randomForest)

rf <- randomForest(Species ~ ., data = traind, importance=TRUE) 
# other parameters: importane = T ,ntree = 100, proximity = T, do.trace = 100
# na.action = na.omit 遇到na要怎樣（預設遇到要停止na.fail）
# By default, randomForest() uses mtry=sqrt(p) （每個節點隨機抽樣的變數量）for classification tree
rf 

#choose tree no.
plot(rf)
legend("topright", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)
#增加每一棵決策樹，整體誤差的改變量
#黑色線:整體的OOB error rate，
#其他顏色虛線:各類別的OOB Error Rate。

#Evaluate variable importance
importance(rf)
varImpPlot(rf)
###
#Mean Decrease Accuracy - How much the model accuracy decreases if we drop that variable.
#Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees. 
###

#prediction
pred=predict(rf, newdata = testd)
table(Real = testd$Species, Predict = pred)

# mtry=?
tuneRF(x = iris[,-5], y = iris[,5])

### Bagging ###
install.packages("ipred")
library(ipred)
bag <- bagging(Species ~ ., data = iris[,-6], coob=TRUE)
print(bag)
pred <- predict(bag, testd)
table(Real = testd$Species, Predict = pred)



### Boosting ###
#install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("adabag")
library(adabag)
boo <- boosting(Species ~ ., data = traind, mfinal = 100)

pred <- predict(boo,testd)
pred
pred$confusion


### XGBoost ###
install.packages("xgboost")
library(xgboost)
X_train = data.matrix(traind[,-5])                
y_train = as.integer(traind[,5])-1                               

X_test = data.matrix(testd[,-5])                 
y_test = as.integer(testd[,5] )-1                               

# convert the train and test data into xgboost matrix type.
xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)

# train a model using our training data
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob", #multiclassification #通常只改這一行以下
  eval_metric = "mlogloss",
  num_class = 3 #length(levels(traind$Species))
)
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgboost_train,
  nrounds = 5000,
  verbose = 1
)
xgb_model

#make predictions on test data
xgb_preds <- predict(xgb_model, as.matrix(X_test), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- levels(iris$Species)
xgb_preds
xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- levels(iris$Species)[y_test + 1]
xgb_preds
confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))

# Important Features
importance_matrix <- xgb.importance(
  feature_names = colnames(xgboost_train), 
  model = xgb_model
)
importance_matrix
xgb.plot.importance(importance_matrix)

### LightGBM ###
library(lightgbm)
dtrain = lgb.Dataset(X_train, label = y_train)
dtest = lgb.Dataset.create.valid(dtrain, data = X_test, label = y_test)

# define parameters
params = list(
  objective= 'multiclass',
  metric = "multi_error",
  num_class= 3
) 

# validataion data
valids = list(test = dtest)
model = lgb.train(params,
                  dtrain,
                  nrounds = 100,
                  valids,
                  min_data=1,
                  learning_rate = 1,
                  early_stopping_rounds = 10)

print(model$best_score)

# prediction
pred = predict(model, X_test, reshape=T)
pred_y = levels(iris$Species)[max.col(pred)]
confusionMatrix(testd$Species, as.factor(pred_y))

# Feature Importance
tree_imp = lgb.importance(model, percentage = T)
lgb.plot.importance(tree_imp, measure = "Gain")


### SVM ###
library(e1071)
s <- svm(Species ~ ., data = traind, probability = TRUE)
results <- predict(s, testd, probability = TRUE)
table(Real = testd$Species, Predict = results)

# Draw Data and Decision Boundary
#install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

rcols <- palette(brewer.pal(n = 3, name = "Set3"))
plot(s, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4),col = rcols)

# Kernel
s_linear <- svm(Species ~ ., data = traind, probability = TRUE,kernel="linear")
s_poly <- svm(Species ~ ., data = traind, probability = TRUE,kernel="polynomial")
s_rbf <- svm(Species ~ ., data = traind, probability = TRUE,kernel="radial")
s_sig <- svm(Species ~ ., data = traind, probability = TRUE,kernel="sigmoid")
plot(s_linear, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4),col = rcols)
plot(s_poly, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4),col = rcols)
plot(s_rbf, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4),col = rcols)
plot(s_sig, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4),col = rcols)

#Tuning SVM to find the best cost and gamma 

svm_tune <- tune(svm, Species ~ .,data=traind, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune) #or
svm_tune$best.model

plot(svm_tune) #
after_tune <- svm(Species ~ ., data=traind, kernel="radial", cost=1, gamma=0.5)
summary(after_tune)
pred <- predict(after_tune,testd)
table(Real = testd$Species, Predict = pred)



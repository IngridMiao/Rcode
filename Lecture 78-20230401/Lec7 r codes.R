#符合exponential family，才可以使用GLM
## Lec 7 Example: 
install.packages("AER")
library(AER)
data("CreditCard")
CreditCard$card <- ifelse(CreditCard$card == "yes", 1, 0) #you can define "y" first
table(CreditCard$card) #frequency table

##binary → logit； count → log
##### logistic regression ##### binary的分類方法
# fit model
logit_model <- glm(card ~ reports + age + income + owner, data=CreditCard, family=binomial(link="logit"))
summary(logit_model)
temp=predict(logit_model, type = "response") #P(Y=1) 返回預測的機率值
pred=ifelse(temp>0.5,1,0) #usually we use 0.5 as the threshold
table(Predict=pred,True=CreditCard$card) #做出type1, type2  error
(105+1006)/1319 #accuracy rate

# 95% CI for beta
confint(logit_model)

# to exponentiate for odds ratios with CI’s 取消log操作
exp(cbind(OR = coef(logit_model), confint(logit_model))) 

# Analysis of Deviance Table (Goodness of Fit)
anova(logit_model, test = "Chi")

# predict confusion matrix混淆矩陣 預測和已知的結果有沒有差異
new <- data.frame(reports = 0, age = 30, income = 10, owner = "yes")
result <- predict(logit_model, newdata = new, type = "response")
result #prob(y=1)

odds = exp(coef(logit_model)[1]+30*coef(logit_model)[3]+10*coef(logit_model)[4]+coef(logit_model)[5])
odds/(1+odds) #P(y=1)

library(arm) #invlogit(x beta) = P(y=1)
invlogit(coef(logit_model)[1]+30*coef(logit_model)[3]+10*coef(logit_model)[4]+coef(logit_model)[5]) #prob(y=1)


# stepwise
step(logit_model)

library(MASS)
stepAIC(logit_model)


# creat training and test data 把數據切成兩部分，一部份用來training；一部份用來testing
ones <- CreditCard[which(CreditCard$card == "yes"), ]  # all yes's
zeros <- CreditCard[which(CreditCard$card == "no"), ]  # all no's
set.seed(100)  # for repeatability of samples
train1_index <- sample(1:nrow(ones), 0.7*nrow(ones))  # yes's for training
train0_index <- sample(1:nrow(zeros), 0.7*nrow(zeros))  # no's for training. Pick as many no's as yes's
train1 <- ones[train1_index, ]  
train0 <- zeros[train0_index, ]
traind <- rbind(train1, train0)  # row bind the yes's and no's 

# Create Test Data
test1 <- ones[-train1_index, ]
test0 <- zeros[-train0_index, ]
testd <- rbind(test1, test0)

# training and test by tidyverse
library(tidyverse)
CreditCard$index =c(1:nrow(CreditCard))
train_df <- CreditCard %>% group_by(card) %>% sample_frac(0.7)
test_df  <- anti_join(CreditCard, train_df, by = 'index') #anti_join 分開

# fit model
model <- glm(card ~ reports + age + income + owner, data=train_df, family=binomial(link="logit"))
summary(model)
predicted1 <- plogis(predict(model, test_df)) #prob(y=1), the same as predicted2
predicted2 <- predict(model, test_df, type="response")  #prob(y=1)

# another way to fit a glm model
library(caret)
model2 <- train(card ~ reports + age + income + owner, 
                   data = train_df, 
                   method = "glm",
                   family = "binomial")
summary(model2)

# find cut point (threshold)
library(MKclass)
threshold = optCutoff(predicted2, truth = test_df$card, namePos = "yes")
predresult = ifelse(predicted2>threshold[1],"yes","no") 


confusionMatrix(data = as.factor(predresult), reference = as.factor(test_df$card),mode = "everything")

#another way
install.packages("ROCR") 
library(ROCR)
ROCR_pred_test <- prediction(predicted2,test_df$card)
ROCR_perf_test <- performance(ROCR_pred_test,measure="tpr", x.measure="fpr")
plot(ROCR_perf_test,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))



# validation
summary(model)
library(car)
vif(model) #all<4

#ROC curve

install.packages("pROC")
library(pROC)
temp <- roc(test_df$card, predicted2)
plot(temp)
temp$auc #area under the curve
auc(temp) #area under the curve

library(tidyverse)
roc.data <- tibble(x = 1-temp$specificities, y = temp$sensitivities)
ggplot(roc.data, aes(x = x, y = y)) + 
  geom_line() + 
  #scale_x_reverse() +  #if use, no "1-" in x above
  ylab("Sensitivity") + 
  xlab("1-Specificity")

# selection via AUC
model2 <- glm(card ~ age + income, data=train_df, family=binomial(link="logit"))
predicted_m2 <- predict(model2, test_df, type="response")  # predicted scores

## easy way 
temp2 <- roc(test_df$card, predicted_m2)
plot(temp)
plot(temp2, add=TRUE, col='red') #black one is better

temp2$auc #compare auc with the one from model1

## ggplot way
roc1.data <- tibble(x = temp$specificities,
                    y = temp$sensitivities,
                    model = "model1")
roc2.data <- tibble(x = temp2$specificities,
                    y = temp2$sensitivities,
                    model = "model2")
roc.both <- rbind(roc1.data, roc2.data)
ggplot(roc.both, aes(x=x,y=y,color=model)) +
  geom_line() + 
  scale_x_reverse()



##### Poisson ######
ggplot(CreditCard,aes(x=active)) +
  geom_histogram(binwidth=5)

pmodel <- glm(active ~ reports + age + income + owner, data=CreditCard, family=poisson)
summary(pmodel)
predict(pmodel, type="response")

exp(coef(pmodel)) #coef. explanation

# check for overdispersion
## quick check
6857.1/1314 # dev/df, =\=1, indicating severe overdispersion

install.packages("qcc")
library(qcc) 
qcc.overdispersion.test(CreditCard$active, type = "poisson")

library(AER)
dispersiontest(pmodel)

list(res.deviance = deviance(pmodel), df = df.residual(pmodel),
     p = pchisq(deviance(pmodel), df.residual(pmodel), lower.tail=FALSE)) #sig. means not a good fit.


##if overdispersion, can fit: 
# quasi poisson
pmodel2 <- glm(active ~ reports + age + income + owner, data=CreditCard, family=quasipoisson)

# negative binomial
library(MASS)
pmodel3 <- glm.nb(active ~ reports + age + income + owner, data=CreditCard)
summary(pmodel3)


#### For rates: offset
data(Fatalities)
?Fatalities

# offset model
model1 <- glm(fatal ~ year, offset = log(milestot),family = poisson, data=Fatalities)
model1 <- glm(fatal ~ year + offset(log(milestot)),family = poisson, data=Fatalities)

summary(model1)
exp(model1$coefficients) # or 
exp(coef(model1))

exp(confint(model1)) # 95%CI



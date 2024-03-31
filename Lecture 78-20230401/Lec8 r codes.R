#Resampling Method
library(AER)
data("CreditCard")
##SRS
library(foreign)
kidiq <- read.dta(file="kidiq.dta")
train = sample(434,200) #random sample 200 data from 434 obs. to train a model
fit = lm(kid_score ~ mom_iq , data=kidiq, subset=train)
mean( (kidiq$kid_score - predict(fit,kidiq))[-train]^2 )  #MSE

kidiq %>% sample_n(200)->temp 
##Cross-Validation 交叉驗證
#Leave-one-out cross-validation (LOOCV) (low bias hign variance)
glm.fit = glm(kid_score ~ mom_iq , data=kidiq)
library(boot)
cv.err = cv.glm(kidiq,glm.fit)
cv.err$delta #The first component is the raw cross-validation estimate of prediction error. The second component is the adjusted cross-validation estimate.
#i.e avg (MSE) and adj MSE

#k-Fold Cross-Validation 
cv.err = cv.glm(kidiq,glm.fit,K=10)
cv.err$delta

###logistic example
logit_model <- glm(card ~ reports + age + income + owner, data=CreditCard, family=binomial(link="logit"))
cost <- function(card, pi = 0) mean(abs(card-pi) > 0.5)

cv.err = cv.glm(CreditCard,logit_model,cost,K=10)
cv.err$delta


##bootstrap (sampling w replacement)
boot.fn = function(data,index){
  return( coef( lm(kid_score ~ mom_iq ,data=data,subset=index) ) )
} #定義boot.fn函數


boot.fn(kidiq ,1:434) #to all data unit
boot.fn(kidiq,sample(434,434,replace=T)) #bootstrapping replace = T 取後放回
#數據集的觀測值數量；從數字範圍中抽取的樣本數量

b=boot(kidiq ,boot.fn ,1000) #resampling 1000 times
bias = mean(b$t[,1]) - b$t0[1] #= (est. coeff - original coeff)
bias2 = mean(b$t[,2]) - b$t0[2] #= (est. coeff - original coeff)

#t0 is coef(fit)



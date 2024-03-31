##### Lec 7 Exercise 1 #####
### logistic regression:
library(ISLR)
?Smarket
attach(Smarket)
#1.
index = which(Year == 2005)
testd = Smarket[index,]
traind = Smarket[-index,]

#2
fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=traind ,family=binomial)
summary(fit)

#3
prob = predict(fit,new=testd,type="response")
predD = ifelse(prob>0.5,"Up","Down") # y=1(Up) or 0(Down)
dd = table(predD,testd$Direction) #confusion table

#4
sum(diag(dd))/sum(dd) #accuracy rate
mean(predD == testd$Direction) #accuracy rate
mean(predD != testd$Direction) #error rate


##### Lec 7 Exercise 2 #####
### Poisson regression
## Load ISwR package
install.packages("ISwR")
library(ISwR)
## Load data
data(eba1977)
eba1977


## Fit Poisson model
#1.
fit.p <- glm(cases ~ city + age, offset = log(pop), family = poisson, data = eba1977)
summary(fit.p)

#2.
qcc.overdispersion.test(eba1977$cases, type = "poisson") #or
dispersiontest(fit.p) # no overdispersion. The Poisson model is probably acceptable. 
pchisq(deviance(fit.p), df.residual(fit.p), lower.tail=FALSE) #good fit



##### Lec 8 Exercise 1 #####
fnc <- function(d, i){
  temp <- d[i]
  return(mean(temp))
}

x <- c(1:50)

library(boot)
b = boot(x,fnc,1000)
summary(b)
b$t
plot(b)
hist(b$t, breaks = 100) 
abline(v=25.5,col="red")
abline(v=mean(b$t),col="green")


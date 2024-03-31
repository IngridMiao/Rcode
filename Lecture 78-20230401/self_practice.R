#ch7 exercise
##1
library(ISLR)
library(AER)
data(Smarket)

###2001~2004 trained ; 2005 tested
index = which(Smarket$Year == 2005)
trained = Smarket[-index,]
tested = Smarket[index,]

Smarket$Direction <- ifelse(Smarket$Direction == 'Up',1,0)
###
fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 +Lag5 + Volume, data = trained, family = binomial(link = logit))
summary(fit)

###
temp = predict(fit, new = tested, type = 'response')
pred = ifelse(temp > 0.5,1,0)
table(Predict = pred, tested$Direction)

### accurary rate
(77+44)/252

##2
install.packages("ISwR")
library(ISwR)
data(eba1977)
summary(eba1977)

###
fit1 <- glm(cases ~ city + age, offset = log(pop), data = eba1977, family = poisson)
summary(fit1)

###過度離散
library(qcc) 
qcc.overdispersion.test(eba1977$cases, type = "poisson")
library(AER)
dispersiontest(fit1)
pchisq(deviance(fit1), df.residual(fit1), lower.tail=FALSE)

#ch8 exercise
fnc = function(d,i){
  temp <- d[i]
  return(mean(temp))
}

x <- c(1:50)

library(boot)
b = boot(x, fnc, 1000)
b$t
plot(b)
hist(b$t, breaks = 100) #100一群
abline(v = 25.5, col = 'red')
abline(v = mean(b$t), col = 'green')

library(tidyverse)
load("abtest.Rdata")
## check your data1
str(data1)
## descriptive statistics
summary(data1)
## check the differnce (EDA)
### test difference
data1 %>%
  group_by(test) %>%
  summarise(mean_purchase = mean(purchase))

### gender differece
data1 %>%
  group_by(sex) %>%
  summarise(mean_purchase = mean(purchase))

### device differece
data1 %>%
  group_by(device) %>%
  summarise(mean_purchase = mean(purchase))

### service differece
data1 %>%
  group_by(service) %>%
  summarise(mean_purchase = mean(purchase))

### country differece
data1 %>%
  group_by(country) %>%
  summarise(mean_purchase = mean(purchase))
 
## Hypothesis Test:
### t-test for two sample mean
### Ha:  mu_1 - mu_0 >0
t.test(data1[data1$test == 1, ]$purchase,
       data1[data1$test == 0, ]$purchase,
       alternative = "greater") #根據Ha寫的
#### reject H0, if p-value is less than the significance level 0.05

### test for more than two samples:
### ANOVA
aov.model <- aov(
  purchase ~ test + country + device + sex + service, data1)

summary(aov.model)

## check the interaction term 交互關係
interaction.model <- aov( purchase ~ test*sex + test*country, data1)
summary(interaction.model)

## final model
aov.model <- aov( purchase ~ test*sex + test*country, data1)

summary(aov.model)

## post-hoc test (事後比較)
TukeyHSD(aov.model, "test") #p是p-value
TukeyHSD(aov.model, "sex")
TukeyHSD(aov.model, "country") #大於2的類別，不知道誰和誰有差
plot(TukeyHSD(aov.model, "country"))

## check daily differece
daily.purchase <- data1 %>%
  group_by(date, test) %>%
  summarise(purchase_amount = mean(purchase))

ggplot(daily.purchase, aes(x = date, y = purchase_amount, colour = as.factor(test))) + 
  geom_point() + geom_line() +
  xlab("Date") + ylab("Purchase Amount") +
  ggtitle("Time Series Plot of Purchase Amount: Test versus Control") +
  theme_bw()

## visualize method to compare
ggplot(data1, aes(x = country, y = purchase)) +
  geom_boxplot() +
  xlab("Country") + ylab("Purchase Amount") +
  ggtitle("Boxplot of Purchase Amount by Country") +
  theme_bw()


ggplot(data1, aes(x = service, y = purchase, colour = test)) +
  geom_boxplot() +
  xlab("Service") + ylab("Purchase Amount") +
  ggtitle("Boxplot of Purchase Amount by Service: Test versus Control") +
  theme_bw() #繪製黑白風格的主題

ggplot(data1, aes(x = country, y = purchase, colour = test)) +
  geom_boxplot() +
  xlab("Country") + ylab("Purchase Amount") +
  ggtitle("Boxplot of Purchase Amount by Country: Test versus Control") +
  theme_bw()

## Data2
# website A: proportion of purchased
#subset是分子；visitor是分母
subset_A <- data2 %>% filter(website == "A" & purchased == "TRUE")
purchased_A <- nrow(subset_A)
visitors_A <- nrow(data2 %>% filter(website == "A"))
phat_A <-  (purchased_A/visitors_A)  

# website B: proportion of purchased
subset_B <- data2 %>% filter(website == "B" & purchased == "TRUE")
purchased_B <- nrow(subset_B)
visitors_B <- nrow(data2 %>% filter(website == "B"))
phat_B <-  (purchased_B/visitors_B)  

#
uplift <- (phat_B - phat_A)/ phat_A * 100
uplift  #82.72%
#B is better than A by 83%. 

p_pool <- (purchased_A + purchased_B)/(visitors_A + visitors_B) #pooled proportion

SE_pool<- sqrt(p_pool*(1-p_pool) * ((1/visitors_A) + (1/visitors_B)))

d_hat <- phat_B - phat_A #Point Estimate or Difference in proportion

z_score <- d_hat/SE_pool

p_value <- pnorm(q = -z_score, mean = 0, sd = 1) * 2


ci <- c(d_hat - qnorm(0.975)*SE_pool, d_hat + qnorm(0.975)*SE_pool) #0.001305794 0.039727961
    

#compute SE and CI for test website A and B separately

se_hat_A <- sqrt(phat_A*(1-phat_A)/visitors_A) 
ci_A <- c(phat_A - qnorm(0.975)*se_hat_A, phat_A + qnorm(0.975)*se_hat_A) 
se_hat_B <- sqrt(phat_B*(1-phat_B)/visitors_B) 
ci_B <- c(phat_B - qnorm(0.975)*se_hat_B, phat_B + qnorm(0.975)*se_hat_B) 

#install.packages("pwr")
library(pwr)
#Run a 1-sample test
prop.test(c(purchased_A + purchased_B),c(visitors_A + visitors_B))
#前放分母後放分子；n1+n2

#Run a 2-sampled test
prop.test(c(purchased_A , purchased_B), c(visitors_A,visitors_B))

#chi squared test
chisq.test(data2$purchased,data2$website)

# fake data
x<-seq(from=90, by=10, length.out=6)
n<-rep(1000,6)
prop.test(x,n) #goodness-of-fit test
pairwise.prop.test(x, n, p.adjust.method = "none")

## sample size determine
install.packages("pwr")
library(pwr)
pwr.anova.test( k = 2, n= NULL, f = 0.2, sig.level = 0.05, power =0.8)
#n不知道，所以估計n
pwr.t.test(n= NULL, d = 0.3, sig.level = 0.05, power = 0.8, type="two.sample",alternative = "greater")

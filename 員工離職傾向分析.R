library(tidyverse)
library(data.table)
library(ggplot2)
library(ALSM)
library(adabag)
library(gbm)
library(ada)
library(caret)

dta <- as.data.frame(fread("C:/Users/ingri/OneDrive/桌面/政大/HRDA/畢業生數據2_.csv", sep=","))

dta <- dta[,3:32]
dta[["是否1年内离职"]] <- factor(dta[["是否1年内离职"]])
colnames(dta)
levels(dta[["是否1年内离职"]]) <- list(在职 = 0, 离职 = 1)
set.seed(4410)

#boosting
dta$性别 <- factor(dta$性别, levels = c("男", "女"))
dta$学历 <- factor(dta$学历, levels = c("大专","本科", "硕士", "博士"))
dta$工作单位 <- factor(dta$工作单位, levels = c("A分公司", "B分公司", "C分公司", "D分公司", "E分公司","F分公司","G分公司","H分公司","I分公司","J分公司","K分公司","L分公司","M分公司","N分公司","O分公司","P分公司","Q分公司","R分公司","S分公司","T分公司"))
dta$职业资格 <- factor(dta$职业资格, levels = c("无","软体设计师","助理人力资源管理师","全国计算机信息高新技术考试合格证","会计从业资格","电工证","仓库保管工","三级企业人力资源管理师","CCNA证书"))
dta$毕业院校地区 <- factor(dta$毕业院校地区, levels = c("上海","山东","山西","广东","广西","云南","内蒙古","天津","北京","四川","宁夏","甘肃","吉林","安徽","江西","江苏","辽宁","武汉","河北","河南","贵州","重庆","陕西","浙江","海南","湖北","湖南","黑龙江","新疆","境外","福建"))
dta$专业类别 <- factor(dta$专业类别, levels = c("工学","文学","农学","体育","医学","其他","法学","哲学","教育学","理学","管理学"))
dta$职称 <- factor(dta$职称, levels = c("工程师","无","助理工程师","助理会计师","助理经济师"))

dta$工作单位类别 <- factor(dta$工作单位类别, levels = c("A类", "B类","C类","D类","E类","F类","G类"))
dta$是否党员 <- factor(dta$是否党员, levels = c("是", "否"))

#by boosting
b <- boosting(是否1年内离职~., dta)
bp <- predict(b, dta)
table(dta$是否1年内离职, bp$class)
importanceplot(b, top = 5)


#by gbm_model
dta$是否1年内离职 <- ifelse(dta$是否1年内离职 == "在职", 0, 1)
gbm_model <- gbm(是否1年内离职 ~ ., 
                 data = dta, 
                 distribution = "bernoulli",
                 n.trees = 500, 
                 interaction.depth = 3, 
                 shrinkage = 0.01, 
                 cv.folds = 5)
n_trees <- length(gbm_model$trees)
# Calculate feature importance using permutation-based method
importance <- varImp(gbm_model, scale = FALSE, numTrees = n_trees)

print(importance)

summary(
  gbm_model, # gbm object
  cBars = 10, # the number of bars to draw. length(object$var.names)
  plotit = TRUE, # an indicator as to whether the plot is generated.defult TRUE.
  method = relative.influence, # The function used to compute the relative influence. 亦可使用permutation.test.gbm
  las = 2,
  main = "Boosting"
)



#Random forest
library(randomForest)
set.seed(4410)
dta2 <- read.csv("C:/Users/ingri/OneDrive/桌面/政大/HRDA/畢業生數據2_.csv")
dta2 <- dta2[,3:32]
dta2[["是否1年内离职"]] <- factor(dta2[["是否1年内离职"]])
levels(dta2[["是否1年内离职"]]) <- list(在职 = 0, 离职 = 1)
set.seed(101010)
r <- randomForest(是否1年内离职~., data = dta2, proximity = TRUE, importance = TRUE, na.rm = TRUE)
rp <- predict(r, dta2)
table(dta2$是否1年内离职, rp)

# Relative Influence
var_importance <- importance(r)
print(var_importance)

print(importance(r, type = 1))

varImpPlot(r, sort = TRUE, type = 1,main = "Random Forest")
varImpPlot(r, type = 2,main = "Random Forest")

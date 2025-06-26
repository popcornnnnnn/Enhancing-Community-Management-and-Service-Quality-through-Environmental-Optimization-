getwd()
setwd("/Users/Administrator/ Documents ")              
#install.packages("lars")
#install.packages("mlogit")
library(lars)
data=read.csv("sqsjx.csv")
head(data)
data1=data[,-c(1:8)]
head(data1)

##lasso变量判断
x = as.matrix(data1[, 1:18]) #data为自己的数据集
y = as.matrix(data1[, 19])
lar1 <-lars(x,y,type = "lasso")
lar1 #查看得到的结果
plot(lar1) 
summary(lar1) #输出lasso对象的细节，包括Df、RSS和Cp，其中Cp是MallowsCp统计量，通常选取Cp最小的那个模型
lar1$Cp[which.min(lar1$Cp)]  #选择最小Cp，结果如下：
lar1$beta #可以得到每一步对应的自变量对应的系数
coef <-coef.lars(lar1,mode="step",s=16) #s为step+1，也比图2中竖线为2的迭代次数对应，与图3中df值相等；s取值范围1-7.
coef[coef!=0] #获取系数值不为零的自变量对应的系数值

###mlogit回归针对lasso后的变量
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(nnet)
library(pROC)
library(stargazer)
data2=data[,-c(11,12,21)]
head(data2)
fit1=multinom(satisfaction~.,data=data2)#多分类logis回归
summary(fit1)
z1=summary(fit1)$coefficients/summary(fit1)$standard.errors
#p1=(1-pnorm(abs(z1),0,1))*2
#p1
#fit1rr=exp(coef(fit1))
#fit1rr

##ordinal logit model

library(MASS)
#install.packages("erer")
library(erer)
fit2=polr(as.factor(satisfaction)~.,data=data2,Hess=TRUE)
summary(fit2)
fit2.coef=data.frame(coef(summary(fit2)))
fit2.coef$pval=round((pnorm(abs(fit2.coef$t.value),lower.tail = FALSE)*2),2)
fit2.coef
stargazer(fit2 , type="text",out="/Users/chentuo/R语言相关/社区/结果1.docx")


##lm回归
fit3=lm(satisfaction~.,data=data2)
summary(fit3)
stargazer(fit3, nobs=T , type="text",out="/Users/chentuo/R语言相关/社区/结果1.docx")


##回归树和随机森林模型
library(rpart.plot)
library(maptree)
library(randomForest)
fit=rpart(satisfaction~.,method='anova',data=data1)
draw.tree(fit)
a=rpart(satisfaction~.,data1)
rpart.plot(a,type=2)

#带入预设的mtry，尝试寻找ntree
ntree_fit<-randomForest(satisfaction~.,data=data1,mtry=6,ntree=1000)
plot(ntree_fit)

a=0
for (i in 1:10){
  mtry_fit<- randomForest(satisfaction~., data=data1, mtry=6 ,ntree=200)
  err<-mean(mtry_fit$mse)
  a=a+err
}
m=a/10
m
mtry_fit<- randomForest(satisfaction~., data=data1, mtry=9 ,ntree=200)
mtry_fit

forest<-randomForest(satisfaction~.,data =data1,importance=TRUE,ntree=200,mtry=1)
forest
importance<-importance(x=forest)
importance
varImpPlot(forest)

##偏效应
rforest<-randomForest(satisfaction~.,data =data,ntree=200,importance=TRUE,mtry=1)
rforest
varImpPlot(rforest)
importance<-importance(x=rforest)
importance
par(mfrow=c(2,2))
partialPlot(rforest,data,x.var = sqgx)###偏效应图
partialPlot(rforest,data,x.var = abjk)
partialPlot(rforest,data,x.var = gxwl)
partialPlot(rforest,data,x.var = zzld)




##异质性
data<-read.csv("spf.csv",header=T)##代入数据
head(data)
rforest<-randomForest(satisfaction~.,data =data,ntree=200,importance=TRUE,mtry=1)
rforest
varImpPlot(rforest)
importance<-importance(x=rforest)
importance

data<-read.csv("dwx.csv",header=T)##代入数据
head(data)
rforest<-randomForest(satisfaction~.,data =data,ntree=200,importance=TRUE,mtry=1)
rforest
varImpPlot(rforest)
importance<-importance(x=rforest)
importance

par(mfrow=c(1,2))

barplot(height = c(3.89,4.01,4.36,4.84,5.08),# 绘图数据（数值型向量）
        names.arg = c('sqmz','dxcs','sqws',"gxwl",'sqgx'),  # 柱子名称
        family = 'Kai',  # 中文字体
        xlab = '重要程度',  # X轴名称
        ylab = '影响变量',  # Y轴名称
        main = '商品房',  # 主标题
        horiz = TRUE,  # 是否为水平放置
)

barplot(height = c(4.67,4.91,5.08,5.46,5.97),# 绘图数据（数值型向量）
        names.arg = c('sqwm','zzld','abjk',"sqgx",'zzqk'),  # 柱子名称
        family = 'Kai',  # 中文字体
        xlab = '重要程度',  # X轴名称
        ylab = '影响变量',  # Y轴名称
        main = '单位型',  # 主标题
        horiz = TRUE,  # 是否为水平放置
)





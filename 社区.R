getwd()
setwd("/Users/Administrator/ Documents ")              
#install.packages("lars")
#install.packages("mlogit")
library(lars)
data=read.csv("sqsjx.csv")
head(data)
data1=data[,-c(1:8)]
head(data1)

##Lasso variable selection
x = as.matrix(data1[, 1:18]) #data is the dataset
y = as.matrix(data1[, 19])
lar1 <-lars(x,y,type = "lasso")
lar1 #View the results
plot(lar1) 
summary(lar1) #Output details of the lasso object, including Df, RSS, and Cp, where Cp is the MallowsCp statistic. Usually, the model with the smallest Cp is selected.
lar1$Cp[which.min(lar1$Cp)]  #Select the smallest Cp, results are as follows:
lar1$beta #Get the coefficients corresponding to each step for the independent variables
coef <-coef.lars(lar1,mode="step",s=16) #s is step+1, which also corresponds to the number of iterations where the vertical line in Figure 2 is 2, and equals the df value in Figure 3; s ranges from 1-7.
coef[coef!=0] #Get the coefficient values of independent variables with non-zero coefficients

###mlogit regression for variables after lasso
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(nnet)
library(pROC)
library(stargazer)
data2=data[,-c(11,12,21)]
head(data2)
fit1=multinom(satisfaction~.,data=data2) #Multinomial logistic regression
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


##lm regression
fit3=lm(satisfaction~.,data=data2)
summary(fit3)
stargazer(fit3, nobs=T , type="text",out="/Users/chentuo/R语言相关/社区/结果1.docx")


##Regression tree and random forest model
library(rpart.plot)
library(maptree)
library(randomForest)
fit=rpart(satisfaction~.,method='anova',data=data1)
draw.tree(fit)
a=rpart(satisfaction~.,data1)
rpart.plot(a,type=2)

#Set mtry and try to find ntree
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

##Partial effects
rforest<-randomForest(satisfaction~.,data =data,ntree=200,importance=TRUE,mtry=1)
rforest
varImpPlot(rforest)
importance<-importance(x=rforest)
importance
par(mfrow=c(2,2))
partialPlot(rforest,data,x.var = sqgx) ###Partial dependence plot
partialPlot(rforest,data,x.var = abjk)
partialPlot(rforest,data,x.var = gxwl)
partialPlot(rforest,data,x.var = zzld)

##Heterogeneity analysis
data<-read.csv("spf.csv",header=T) ##Load data
head(data)
rforest<-randomForest(satisfaction~.,data =data,ntree=200,importance=TRUE,mtry=1)
rforest
varImpPlot(rforest)
importance<-importance(x=rforest)
importance

data<-read.csv("dwx.csv",header=T) ##Load data
head(data)
rforest<-randomForest(satisfaction~.,data =data,ntree=200,importance=TRUE,mtry=1)
rforest
varImpPlot(rforest)
importance<-importance(x=rforest)
importance

par(mfrow=c(1,2))

barplot(height = c(3.89,4.01,4.36,4.84,5.08), # Plot data (numeric vector)
        names.arg = c('sqmz','dxcs','sqws',"gxwl",'sqgx'), # Column names  
        family = 'Kai', # Chinese font
        xlab = 'Importance level', # X-axis label  
        ylab = 'Influencing variables', # Y-axis label
        main = 'Commercial housing', # Main title
        horiz = TRUE, # Whether to place horizontally
)

barplot(height = c(4.67,4.91,5.08,5.46,5.97), # Plot data (numeric vector)
        names.arg = c('sqwm','zzld','abjk',"sqgx",'zzqk'), # Column names
        family = 'Kai', # Chinese font  
        xlab = 'Importance level', # X-axis label
        ylab = 'Influencing variables', # Y-axis label
        main = 'Unit-type housing', # Main title
        horiz = TRUE, # Whether to place horizontally
)


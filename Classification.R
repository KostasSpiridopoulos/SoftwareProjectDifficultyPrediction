#isbg = read.table("isbg.txt", header = TRUE)
library(zoom)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(png)
library(gridExtra)
library(grid)
library(RColorBrewer)
library(pROC)
library(boot)
names(isbg)
dim(isbg)

#isbg.rcorr = rcorr(as.matrix(isbg))

#corrplot(isbg.rcorr$r)
#corrplot(isbg.rcorr$P)

#theme1 = ttheme_minimal(
#  core=list(bg_params = list(fill = c(brewer.pal(9, "Blues"),blues9[9]), col=NA),
#            fg_params=list(fontface=3)),
#  colhead=list(fg_params=list(col="#3366FF", fontface=4L)),
#  rowhead=list(fg_params=list(col="#3366FF", fontface=3L)))

#table.tune.out = summary(tune.out)
#table.tune.out$performances
#grid.table(table.tune.out$performances,theme = theme1)
#dev.off()
#zm()

isbg = isbg[isbg$effort < 30000,] # remove outliers
#isbgS = as.data.frame(scale(isbg))#standardize data 
#isbgS$effort = isbg$effort unscale target variable
isbg$High = isbg$effort > median(isbg$effort)
isbg$effort = NULL
attach(isbg)


smp_size = floor(0.75 * nrow(isbg))#training set 75 % of data
set.seed(3)
train_ind = sample(seq_len(nrow(isbg)), size = smp_size)

train = isbg[train_ind, ]
test = isbg[-train_ind, ]

#train$effort = log(train$effort)
#Logistic Regression
form1 = as.formula('High~AdjustedFunctionPoints + Inputcount')
form2 = as.formula('High~AdjustedFunctionPoints + Addedcount')
form3 = as.formula('High~AdjustedFunctionPoints + Enquirycount')
form4 = as.formula('High~AdjustedFunctionPoints + Inputcount + Enquirycount + Addedcount')
form5 = as.formula('High~AdjustedFunctionPoints + Outputcount')
form6 = as.formula('High~AdjustedFunctionPoints + Outputcount + Addedcount')
form7 = as.formula('High~AdjustedFunctionPoints')

fit1=glm(form1,data=train,family=binomial)
fit2=glm(form2,data=train,family=binomial)
fit3=glm(form3,data=train,family=binomial)
fit4=glm(form4,data=train,family=binomial)
fit5=glm(form5,data=train,family=binomial)
fit6=glm(form6,data=train,family=binomial)
fit7=glm(form7,data=train,family=binomial)

glm.probs1=predict(fit1,test,type="response")
glm.probs2=predict(fit2,test,type="response")
glm.probs3=predict(fit3,test,type="response")
glm.probs4=predict(fit4,test,type="response")
glm.probs5=predict(fit5,test,type="response")
glm.probs6=predict(fit6,test,type="response")
glm.probs7=predict(fit7,test,type="response")

glm.pred1=rep("FALSE",nrow(test))
glm.pred2=rep("FALSE",nrow(test))
glm.pred3=rep("FALSE",nrow(test))
glm.pred4=rep("FALSE",nrow(test))
glm.pred5=rep("FALSE",nrow(test))
glm.pred6=rep("FALSE",nrow(test))
glm.pred7=rep("FALSE",nrow(test))
glm.pred1[glm.probs1>.5]="TRUE"
glm.pred2[glm.probs2>.5]="TRUE"
glm.pred3[glm.probs3>.5]="TRUE"
glm.pred4[glm.probs4>.5]="TRUE"
glm.pred5[glm.probs5>.5]="TRUE"
glm.pred6[glm.probs6>.5]="TRUE"
glm.pred7[glm.probs7>.5]="TRUE"

class_accuracy = function (t){
  print(paste("Class accuracy A:", round(t[1]/(t[1]+t[2]), digits=2)))
  print(paste("Class accuracy B:", round(t[4]/(t[3]+t[4]), digits=2)))
}

table(glm.pred1,test$High)
mean(glm.pred1==(test$High))
class_accuracy(table(glm.pred1,test$High))

table(glm.pred2,test$High)
mean(glm.pred2==(test$High))
class_accuracy(table(glm.pred2,test$High))


table(glm.pred3,test$High)
mean(glm.pred3==(test$High))
class_accuracy(table(glm.pred3,test$High))

table(glm.pred4,test$High)
mean(glm.pred4==(test$High))
class_accuracy(table(glm.pred4,test$High))

table(glm.pred5,test$High)
mean(glm.pred5==(test$High))
class_accuracy(table(glm.pred5,test$High))

table(glm.pred6,test$High)
mean(glm.pred6==(test$High))
class_accuracy(table(glm.pred6,test$High))

table(glm.pred7,test$High)
mean(glm.pred7==(test$High))
class_accuracy(table(glm.pred7,test$High))

library(pROC)

g1 = roc(test$High ~ glm.probs1, data = test)
a1 = auc(test$High ~ glm.probs1, data = test)

g2 = roc(test$High ~ glm.probs2, data = test)
a2 = auc(test$High ~ glm.probs2, data = test)

g3 = roc(test$High ~ glm.probs3, data = test)
a3 = auc(test$High ~ glm.probs3, data = test)

g4 = roc(test$High ~ glm.probs4, data = test)
a4 = auc(test$High ~ glm.probs4, data = test)

g5 = roc(test$High ~ glm.probs5, data = test)
a5 = auc(test$High ~ glm.probs5, data = test)

g6 = roc(test$High ~ glm.probs6, data = test)
a6 = auc(test$High ~ glm.probs6, data = test)

par(mfrow=c(3,2))
plot(g1) 
a1

plot(g2)
a2

plot(g3)
a3

plot(g4)
a4

plot(g5)
a5

plot(g6)
a6
zm()

boot.logistic = function(data,index){
  train = data[index, ]
  
  fit=glm(form1,data=train,family=binomial)
  glm.probs=predict(fit,data,type="response")
  glm.pred=rep("FALSE",nrow(data))
  glm.pred[glm.probs>.5]="TRUE"
  
  return(mean(glm.pred==(data$High)))
}

options(warn =0)
bootstrap = boot(isbg,boot.logistic, R = 1000)
bootstrap

plot(bootstrap)

boot.ci(boot.out = bootstrap,type = "norm",conf = 0.95)#print confidence intervals(Normal and basic)


#Support Vector Machines

library(e1071)
names(isbg)
train.x = data.matrix(isbg[train_ind, c(1,4)])
train.y = isbg$High[train_ind]
test.x = data.matrix(isbg[-train_ind, c(1,4)])
test.y = isbg$High[-train_ind]
dat = data.frame(x = train.x, y = as.factor(train.y))
testdat = data.frame(x = test.x, y = as.factor(test.y))

svmfit=svm(y~., data=dat, kernel="linear",  gamma=0.5, cost=10)
plot(svmfit, dat)
ypred=predict(svmfit,testdat)
table(ypred, test.y)
mean(ypred==test.y)

tune.out=tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
plot(tune.out$best.model, dat)
summary(tune.out)
ypred=predict(tune.out$best.model,testdat)
params = tune.out$best.parameters
params
table(ypred, test.y)
mean(ypred==test.y)
# zm()


boot.svm = function(data,index){
  
  train.x = data.matrix(isbg[index, c(1,4)])
  train.y = isbg$High[index]
  test.x = data.matrix(isbg[, c(1,4)])
  test.y = isbg$High
  dat = data.frame(x = train.x, y = as.factor(train.y))
  testdat = data.frame(x = test.x, y = as.factor(test.y))
  
  fit=svm(y~., data=dat, kernel="linear",  gamma=params[2], cost=params[1])
  ypred=predict(fit,testdat)
  
  return(mean(ypred==(test.y)))
}

bootstrap.svm = boot(isbg, boot.svm, R= 1000)
plot(bootstrap.svm)
bootstrap.svm
boot.ci(boot.out = bootstrap.svm,type = "norm",conf = 0.95)

#ROC for svm
library(ROCR)

par(mfrow=c(3,2))
pred.roc = prediction(as.numeric(ypred), test$High)
roc = performance(pred.roc, measure="tpr",x.measure="fpr")
plot(roc)
lines(x = c(0,1), y = c(0,1),col="blue")

auc = performance(pred.roc,measure = "auc")
auc@y.values[[1]]


#LDA

par(mfrow=c(1,2))

library(MASS)
lda.fit=lda(form7,data=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, test)
lda.class=lda.pred$class
table(lda.class,test$High)
mean(lda.class==test$High)

qplot(AdjustedFunctionPoints, data=test, cex=2, col=High)
qplot(AdjustedFunctionPoints, Inputcount, data=test, cex=2, col=High)
qplot(AdjustedFunctionPoints, Enquirycount, data=test, cex=2, col=High)
qplot(AdjustedFunctionPoints, Enquirycount, data=test, cex=2, col=lda.class==High)
pred = prediction(lda.pred$posterior[,2],test$High)
perf = performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
zm()


#QDA

qda.fit=qda(form7,data=train)
qda.fit

qda.pred = predict(qda.fit,test)
qda.class=predict(qda.fit,test)$class
table(qda.class,test$High)
mean(qda.class==test$High)
qplot(Inputcount, AdjustedFunctionPoints, data=test, cex=2, col=qda.class==High)
pred = prediction(qda.pred$posterior[,2],test$High)
perf = performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)


boot.lda = function(data,index){
  train = data[index, ]
  
  lda.fit=lda(form6,data=train)
  lda.pred=predict(lda.fit, data)
  lda.class=lda.pred$class
  
  return(mean(lda.class==data$High))
}


boot.qda = function(data,index){
  train = data[index, ]
  
  qda.fit=qda(form3,data=train)
  qda.pred=predict(qda.fit, data)
  qda.class=qda.pred$class
  
  return(mean(qda.class==data$High))
}

#lda bootstrap
bootstrap.lda = boot(isbg,boot.lda, R = 1000)
bootstrap.lda

plot(bootstrap.lda)

boot.ci(boot.out = bootstrap.lda,type = "norm",conf = 0.95)

#qda bootstrap

bootstrap.qda = boot(isbg,boot.qda, R = 1000)
bootstrap.qda
plot(bootstrap.qda)

boot.ci(boot.out = bootstrap.qda,type = "norm",conf = 0.95)

# Model Comparison
df2 = data.frame(rep(0,3000), rep('None',3000))
names(df2) = c("Accuracy", "Model")
df2$Accuracy[1:1000] = bootstrap$t
df2$Model[1:1000] = "Logistic Regression"
df2$Accuracy[1001:2000] = bootstrap.svm$t
df2$Model[1001:2000] = "SVM"
df2$Accuracy[2001:3000] = bootstrap.lda$t
df2$Model[2001:3000] = "LDA"

boxplot(Accuracy~Model, data = df2)

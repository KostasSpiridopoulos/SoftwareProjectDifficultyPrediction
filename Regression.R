isbg = read.table("isbg.txt", header = TRUE)
names(isbg)
dim(isbg)
library(splines)
library(gam)
library(zoom)
library(boot)
library(leaps)

isbg = isbg[isbg$effort < 30000,] # remove outliers
isbgS = as.data.frame(scale(isbg))#standardize data 
#isbgS$effort = isbg$effort #unscale target variable
attach(isbgS)

hist(effort,breaks = 100, col = "red")#histogram of y

plot(AdjustedFunctionPoints,effort)
plot(Inputcount,effort)
plot(Outputcount,effort)
plot(Enquirycount,effort)
plot(Filecount,effort)
plot(Interfacecount,effort)
plot(Addedcount,effort)
plot(Changedcount,effort)
plot(Deletedcount,effort)
par(mfrow=c(1,1))

smp_size = floor(0.75 * nrow(isbgS))#training set 75 % of data

set.seed(123)
train_ind = sample(seq_len(nrow(isbgS)), size = smp_size)

train = isbgS[train_ind, ]
test = isbgS[-train_ind, ]

fit.gam1 = gam(effort ~ s(AdjustedFunctionPoints,df = 2) + Inputcount + Outputcount + Filecount,data = train)
fit.gam2 = gam(effort ~ s(AdjustedFunctionPoints,df = 2) + Inputcount + Addedcount + Changedcount,data = train)
fit.gam3 = gam(effort ~ s(AdjustedFunctionPoints,df = 2) + Inputcount + Addedcount + Changedcount + Deletedcount,data = train)
fit.lm = lm(effort ~ AdjustedFunctionPoints + Inputcount + Outputcount + Filecount,data = train)

preds.gam1 = predict(fit.gam1,newdata=test)
preds.gam2 = predict(fit.gam2,newdata=test)
preds.gam3 = predict(fit.gam3,newdata=test)
preds.lm = predict(fit.lm,newdata=test)

#compare 4 gam models 
mean((test$effort - preds.gam1)^2)#mse with validation set
mean((test$effort - preds.gam2)^2)
mean((test$effort - preds.gam3)^2)
mean((test$effort - preds.lm)^2)

anova(fit.gam1,test = "F")#fit.gam1 has the best ratio F to P value so its best option
anova(fit.gam2,test = "F")
anova(fit.gam3,test = "F")

#function declarations
cv_gam = function(data, k, d){
  n = floor(dim(data)[1]/k)
  mse = rep(0, k)
  form = as.formula(paste('effort ~ s(AdjustedFunctionPoints, df = ', d, ') + Inputcount + Outputcount + Filecount'))
  for (i in 1:k){
    start = (i-1) * n + 1
    end = start + n -1
    test_idx = start:end
    train_cv = data[-test_idx, ]
    test_cv = data[test_idx, ]
    fit.gam = gam(form ,data = train_cv)
    preds.gam = predict(fit.gam,newdata=test_cv)
    mse[i] = mean((test_cv$effort - preds.gam)^2)
  }
  return(mean(mse))
}

cv_gam_best_df = function(data,k,d){
  df_mse = rep(0, d)
  for(i in 1:d)
    df_mse[i] = cv_gam(data,k,i)
  print(df_mse)
  return (match(min(df_mse),df_mse))
}

boot.gam = function(data,index){
  ret = cv_gam(data[index,],5,2)
  return(ret)
}

bestDf = cv_gam_best_df(isbgS,20,5)
bestDf

cv_gam(isbgS,10,bestDf)# cross validation


bootstrap = boot(isbgS,boot.gam, R = 100)
bootstrap

plot(bootstrap)

boot.ci(boot.out = bootstrap,type = c("norm","basic"),conf = 0.90)#print confidence intervals(Normal and basic)






#knn 
library(class)


train.X = isbgS[train_ind, -1]
test.X = isbgS[-train_ind, -1]
train.Y=isbgS[train_ind, 1]
test.Y=isbgS[-train_ind, 1]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=3)
predlist.knn = as.numeric(as.character(knn.pred)) #convert to list from factor
mean((test.Y - predlist.knn)^2)



cv.knn.mse =  function(data, k=10, kn=3){ #returns the MSE of the data with cross validation
  n = floor(dim(data)[1]/k)
  mse = rep(0, k)
  for (i in 1:k){
    start = (i-1) * n + 1
    end = start + n -1
    test_idx = start:end
    train_cv.X = data[-test_idx, -1]
    test_cv.X = data[test_idx, -1]
    train_cv.Y = data[-test_idx, 1]
    test_cv.Y = data[test_idx, 1]
    cv.pred=knn(train_cv.X,test_cv.X,train_cv.Y,kn)
    cv.predlist = as.numeric(as.character(cv.pred)) #convert factor to list
    mse[i] = mean((test_cv.Y - cv.predlist)^2)
  }
  return(mean(mse))
}

knn.bestK = function(data, cv_k){ #chooses the best K through cross validation
  mins = rep(0, 10)
  for (i in 1:10){
    mins[i] = cv.knn.mse(data, cv_k, i)
  }
  return (which.min(mins))
}

bestK = knn.bestK(isbgS, 10)


#bootstrapping
boot.knn = function(data,index){
  boot_train = data[index,]
  ret = cv.knn.mse(boot_train, bestK, 4)
  return(ret)
}
knn.boot = boot(isbgS, boot.knn, R = 200)
knn.boot

plot(knn.boot)

boot.ci(boot.out = knn.boot,type = c("norm","basic"),conf = 0.90)#print confidence intervals(Normal and basic)



#trees boosting
library(tree)
library(gbm)

set.seed(1)  
fit.boosting = gbm(effort ~ AdjustedFunctionPoints + Inputcount + Filecount,data=isbgS,distribution="gaussian",
                   n.trees=1000,interaction.depth=3,cv.folds =  10,shrinkage = 0.1)

min(fit.boosting$cv.error)

bestTreesNumber = gbm.perf(fit.boosting, method = "cv")#find best number of true with cv 
bestTreesNumber


#find best learning rate
select_shrinkage = c(0.3,0.1,0.01,0.005)
mse = rep(0,4)

for(i in 1:length(select_shrinkage)){
  test.boosting = gbm(effort ~ AdjustedFunctionPoints + Inputcount + Filecount,data=isbgS,distribution="gaussian",
                      n.trees=bestTreesNumber,interaction.depth=3,cv.folds =  10,shrinkage = select_shrinkage[i])
  mse[i] = min(test.boosting$cv.error)
}

mse
bestShrinkage = select_shrinkage[which.min(mse)]#best learning rate
bestShrinkage

#find best tree depth
select_depth = c(3,5,7,9)

for(i in 1:length(select_depth)){
  test.boosting = gbm(effort ~ AdjustedFunctionPoints + Inputcount + Filecount,data=isbgS,distribution="gaussian",
                      n.trees=bestTreesNumber,interaction.depth=select_depth[i],cv.folds =  10,shrinkage = bestShrinkage)
  mse[i] = min(test.boosting$cv.error)
}

mse
bestDepth = select_depth[which.min(mse)]#best learning rate
bestDepth

#final model with selected shrinkage ,number of tress and depth
fit.boosting = gbm(effort ~ AdjustedFunctionPoints + Inputcount + Filecount,data=isbgS,distribution="gaussian",
                   n.trees=bestTreesNumber,interaction.depth=bestDepth,cv.folds =  10,shrinkage = bestShrinkage)

min(fit.boosting$cv.error)

#bootstrap function
boot.boosting = function(data,index){
  fit.boosting = gbm(effort ~ AdjustedFunctionPoints + Inputcount + Filecount,data=data[index,],distribution="gaussian",
                     n.trees=bestTreesNumber,interaction.depth=bestDepth,cv.folds =  5,shrinkage = bestShrinkage)
  return(mean(fit.boosting$cv.error))
}


bootstrap = boot(isbgS,boot.boosting, R = 10)
bootstrap

plot(bootstrap)

boot.ci(boot.out = bootstrap,type = "norm",conf = 0.90)#print confidence intervals(Normal and basic)

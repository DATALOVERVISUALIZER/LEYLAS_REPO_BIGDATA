setwd(getwd())
load("descr.rdata")
load("mutagen.rdata")
descr<-descr[,1:100]

library("caret")
library("FactoMineR")
library("e1071")
library("gbm")

set.seed(1)
inTrain <- createDataPartition(mutagen, p = 3/4, list = FALSE)
trainDescr <- descr[inTrain,] #Train, predictors
testDescr <- descr[-inTrain,] #Test, predictors

trainClass <- mutagen[inTrain] #Train, dependent
testClass <- mutagen[-inTrain] #Test, dependent

prop.table(table(mutagen))
prop.table(table(trainClass))

#remove data that has high correlation
ncol(trainDescr)
descrCorr <- cor(trainDescr)
highCorr <- findCorrelation(descrCorr, 0.90)
trainDescr <- trainDescr[, -highCorr]
testDescr <- testDescr[, -highCorr]
ncol(trainDescr)

#standardize data
xTrans <- preProcess(trainDescr)
trainDescr <- predict(xTrans, trainDescr)
testDescr <- predict(xTrans, testDescr)

#End of Data Clean

#Caret using Bootstrap
bootControl <- trainControl(method = "boot",number = 10)
set.seed(2)

library(doParallel) #start of parallelization
cl <- makePSOCKcluster(4) #number of cores
registerDoParallel(cl)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), #number of levels of tree
                        n.trees = c(10, 20, 50), #number of trees
                        shrinkage = 0.1, #learning rate
                        n.minobsinnode = 5) #number of terminal nodes

gbmFit <- caret::train(trainDescr, trainClass, method = "gbm", 
                trControl = bootControl, verbose = FALSE, 
                bag.fraction = 0.5, 
                #the fraction of the training set observations randomly selected 
                #to propose the next tree in the expansion.
                tuneGrid = gbmGrid)



plot(gbmFit)                       
plot(gbmFit, plotType = "level")
gbmFit$results

best(gbmFit$results, metric="Accuracy", maximize=T)
tolerance(gbmFit$results, metric="Accuracy", maximize=T
          , tol=2)

#Caret using 10 fold CV

fitControl <- trainControl(
  method = "repeatedcv",
  ## 10-fold CV
  number = 10,
  ## repeated one time
  repeats = 1)

gbmFit <- caret::train(trainDescr, trainClass, method = "gbm", 
                trControl = fitControl, verbose = FALSE, 
                tuneLength = 4)

plot(gbmFit)                       
plot(gbmFit, plotType = "level")
gbmFit$results

best(gbmFit$results, metric="Accuracy", maximize=T)
tolerance(gbmFit$results, metric="Accuracy", maximize=T, 
          tol=2)

#load data
############################XGBOOST###########################################
library(data.table)
library(mlr)

#set variable names
setcol<-c("age","workclass","fnlwgt","education","education-num",
          "marital-status","occupation","relationship","race","sex",
          "capital-gain", "capital-loss","hours-per-week","native-country",
          "target")

#load data
train <- read.table("adultdata.txt", header = F, 
                    sep = ",", col.names = setcol, na.strings = c(" ?"),
                    stringsAsFactors = F)

test <- read.table("adulttest.txt",header = F,sep = ",",col.names = setcol,
                  na.strings = c(" ?"),stringsAsFactors = F)

#convert data frame to data table
setDT(train)
setDT(test)

#check missing values 
table(is.na(train))
sapply(train, function(x) sum(is.na(x))/length(x))*100

table(is.na(test))
sapply(test, function(x) sum(is.na(x))/length(x))*100

#quick data cleaning
#remove extra character from target variable
library(stringr)
test [,target := substr(target,start = 1,stop = nchar(target)-1)]

#remove leading whitespaces
char_col <- colnames(train)[ sapply (test,is.character)]
for(i in char_col) set(train,j=i,value = str_trim(train[[i]],side = "left"))
for(i in char_col) set(test,j=i,value = str_trim(test[[i]],side = "left"))

#set all missing value as "Missing" 
train[is.na(train)] <- "Missing"
test[is.na(test)] <- "Missing"

#using one hot encoding 
new_tr <- model.matrix(~.+0,data = train[,-c("target"),with=F])
new_ts <- model.matrix(~.+0,data = test[,-c("target"),with=F])

#detect the problematic variable
k=1
for (i in colnames(new_tr)) {
  check<-(i %in% colnames(new_ts))
  print(check)
  print(k)
  k=k+1
}

new_tr<-new_tr[,-74]

#dependent variable
labels <- train$target
ts_label <- test$target

#convert continuous dependent variable to categorical dependent variable
labels<-(labels=="<=50K")
labels<-as.numeric(labels)
ts_label<-(ts_label=="<=50K")
ts_label<-as.numeric(ts_label)

#end of data cleaning

library("xgboost")

#convert regular data to xgboost data 
dtrain <- xgb.DMatrix(data = new_tr,label = labels)
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)

#nrounds: 
#eta (0,1) 0.3: learning rate
#gamma (0,Inf) 0: A node is split only when the resulting split gives a positive reduction 
#in the loss function. Gamma specifies the minimum loss reduction required to make a 
#split.
#max depth (0, Inf) 6: depth of trees
#min_child_weight (0,Inf) 1: it refers to the minimum number of instances required in a child node. 
#In classification, if the leaf node has a minimum sum of instance weight 
#(calculated by second order partial derivative) lower than min_child_weight, 
#the tree splitting stops.
#subsample (0,1) 1: the number of samples (observations) supplied to a tree.
#colsample_bytree (0,1) 1: Denotes the fraction of columns to be randomly samples for each tree.
#lambda 0: L2 regularization (Ridge Regression)
#alpha 1: L1 regularization (Lasso Regression)
#objective: reg:linear, binary:logistic, multi:softmax, multi:softprob
#eval_metric: rmse->regression, error->classification
# rmse – root mean square error
# mae – mean absolute error
# logloss – negative log-likelihood
# error – Binary classification error rate (0.5 threshold)
# merror – Multiclass classification error rate
# mlogloss – Multiclass logloss
# auc: Area under the curve
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=6, min_child_weight=1, 
               subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, 
                 nfold = 5, showsd = T, stratified = T, 
                 print_every_n = 10, early_stopping_rounds = 20, maximize = F)

xgbcv$best_iteration

xgb1 <- xgb.train (params = params, data = dtrain, 
                   nrounds = xgbcv$best_iteration)

xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)

table(xgbpred, ts_label)
mean(xgbpred==ts_label)

mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])

#caret based grid search
library("caret")

xgbGrid <-  expand.grid(nrounds = c(10,100), 
                        max_depth = c(5, 10), 
                        eta = 0.3,
                        gamma = 0, colsample_bytree=1,
                        min_child_weight=1, subsample=1)

fitControl <- trainControl(## 2-fold CV
  method = "repeatedcv",
  number = 2,
  ## repeated ten times
  repeats = 1)

gbmFit <- caret::train(new_tr, as.factor(labels), method = "xgbTree", 
                trControl = fitControl, verbose = T, 
                tuneGrid = xgbGrid)

plot(gbmFit)                       
plot(gbmFit, plotType = "level")
gbmFit$results

best(gbmFit$results, metric="Accuracy", maximize=T)
tolerance(gbmFit$results, metric="Accuracy", maximize=T, tol=2)

stopCluster(cl)

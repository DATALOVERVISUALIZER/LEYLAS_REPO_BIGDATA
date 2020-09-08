
# Model Building & Validation - Final Project

library(tree)
library(ISLR)
attach(Carseats)
library(MASS)
library(randomForest)
library(caret)
library(FactoMineR)
library(e1071)
library(gbm)
library(xgboost)
library(data.table)
library(mlr)
library(Hmisc)
library(GGally)
library(stats4)
library(SmartEDA)
library(doParallel)
library(plyr); library(dplyr)
library(adabag)
library(leaps)
library(elasticnet)
library(glmnet)

# Data introduction
setwd("C:/Users/kerim.acar/Desktop/BDA/Model Building/Final")

rm(list=ls()) # clean all
options(scipen = 999)

train <- read.csv2("Train_New_Data.csv")
test <- read.csv2("Test_New_Data.csv")

str(train) # data types
table(is.na(train)) # total NA values

# NAs by features
NAs.train <- as.data.frame(sapply(train, function(x) sum(is.na(x))/length(x))*100)
NAs.test <- as.data.frame(sapply(test, function(x) sum(is.na(x))/length(x))*100)

table(is.na(train))

# These 5 features are omitted. 
# Alley = 93.5% ,LotFrontage = 17.3%, FireplacesQu = 47.7%, PoolQC = 99.7%, MiscFeature = 95.6%, Fence = 80.5%

drop.cols <- c("Alley", "FireplaceQu", "PoolQC", "MiscFeature", "Fence")

train <- train %>% select(-one_of(drop.cols))
test <- test %>% select(-one_of(drop.cols))



# 999 rows in original, 915 rows after omitting NA containing rows for TRAIN set
# train <- train[complete.cases(train), ] # 8% lost !!!!!!!!!!!
# test <- test[complete.cases(test), ] # 8% lost   !!!!!!!!!!!

# With removal of NA features, we precisely lost 8% of both train & test data. 
#Thus, it is better not to omit missing values.

# columns with missing values
# train.NAs <- train[, c("GarageType", "GarageYrBlt", "GarageFinish", "GarageQual", "GarageCond", "BsmtExposure", 
#                       "BsmtFinType2", "BsmtQual", "BsmtCond", "BsmtFinType1", "MasVnrType", "MasVnrArea", "LotFrontage")]





# Missing value replacement with MODE 
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

# train set NA replacement
for (var in 1:ncol(train)) {
  if (class(train[,var])=="integer") {
    train[is.na(train[,var]),var] <- mean(train[,var], na.rm = TRUE)
  } else if (class(train[,var]) %in% c("character", "factor")) {
    train[is.na(train[,var]),var] <- Mode(train[,var], na.rm = TRUE)
  }
}

# test set NA replacement
for (var in 1:ncol(test)) {
  if (class(test[,var])=="integer") {
    test[is.na(test[,var]),var] <- mean(test[,var], na.rm = TRUE)
  } else if (class(train[,var]) %in% c("character", "factor")) {
    test[is.na(test[,var]),var] <- Mode(test[,var], na.rm = TRUE)
  }
}


############### Feature Engineering - Years ############### 
# New Feature Creation - BuildingSoldAge
train$BuildingSoldAge <- train$YrSold - train$YearBuilt
test$BuildingSoldAge <- test$YrSold - test$YearBuilt

# New Feature Creation - RemodelAge
train$RemodelAge <- train$YrSold - train$YearRemodAdd
test$RemodelAge <- test$YrSold - test$YearRemodAdd

# New Feature Creation - GarageAge
train$GarageAge <- train$YrSold - train$GarageYrBlt
test$GarageAge <- test$YrSold - test$GarageYrBlt


drop.cols_x1 <- c("YrSold", "YearBuilt","YearRemodAdd", "GarageYrBlt")
train <- train %>% select(-one_of(drop.cols_x1))
test <- test %>% select(-one_of(drop.cols_x1))

# OveralQual > Numeric to Categorical
#hist(train$OverallQual)
#table(train$OverallQual)

#train$OverallQualCat <- cut(train$OverallQual, c(0, 6, 8,11), right=FALSE, labels=c("low","average","high"))
# 1-2-3-4-5 = low, 6-7 = average, 8-9-10 = 159
#table(train$OverallQualCat)
# train$OverallQualCat <- NULL

############### Numerical features - EDA - (37 features in raw) ############### 
numeric.names <- names(train[, sapply(train, is.numeric)])
numeric <- train[, numeric.names]

# count of unique values in numeric features
numeric.unique <- apply(numeric, 2, function(x) nlevels(as.factor(x)))


describe(numeric) # detailed summary
summary(numeric)




# Below feautes can be converted into factor data format, since they are scaled in points *********************
# MSSubClass - 15, OverallQual - 10, OverallCond - 9, LowQualFinSF - 19, BsmtFullBath - 4
# BsmtHalfBath - 3, FullBath - 4, HalfBath - 3, BedroomAbvGr - 8, KitchenAbvGr  - 4
# TotRmsAbvGrd - 12, Fireplaces - 4, GarageCars  - 5, X3SsnPorch - 15, PoolArea - 3
# MiscVal - 16, MoSold - 12, YrSold - 5

i1 <- sapply(numeric, function(x) length(unique(x)) < 20)
table(i1) # We got 18 features which are actually categorical



# Correlation matrix of numeric features
correlation.matrix <- cor(numeric)

corrs <- setDT(melt(correlation.matrix))[order(value)]
cor_saleprice <- subset(corrs, corrs$Var2 == "SalePrice") # correlations with target

# features that have correlation coefficient less than 0.1 - omitted 
drop.cols2 <- c("X3SsnPorch", "BsmtFinSF2", "BsmtHalfBath", "LowQualFinSF", "Id", "MiscVal",
                "MSSubClass", "OverallCond")

train <- train %>% select(-one_of(drop.cols2))
test <- test %>% select(-one_of(drop.cols2))



############### Categorical features (38 features in raw) ###############
categorical.names <- names(train[, sapply(train, is.factor)])
categorical <- train[, categorical.names]

summary(categorical) # observe distribution of categorical features

# Street >> 995 pave - can be removed (not informative)
# Utilities >> 998 AllPub - 
# Condition2 >> 987 norm - 
# RoofMatl >> 980 CompShg - 
# Heating >> 981 GasA - 

drop.cols3 <- c("Street", "Utilities", "Condition2", "RoofMatl", "Heating")
train <- train %>% select(-one_of(drop.cols3))
test <- test %>% select(-one_of(drop.cols3))


# One Hot Encoding
train <- model.matrix(~.+0,data = train)
train <- as.data.frame(train)

test <- model.matrix(~.+0,data = test)
test <- as.data.frame(test)

colnames(train)[1] <- "MSZoningCall"
colnames(test)[1] <- "MSZoningCall"


# non-matching features are excluded
setdiff(train, test)

drop.cols4 <- c("BsmtCondPo", "FunctionalSev", "SaleConditionAdjLand", "GarageQualPo", "ExterCondPo", "ElectricalFuseP",
                "GarageCondFa", "SaleTypeCon", "HeatingQCPo", "NeighborhoodBlueste", "Condition1RRNe", "Exterior2ndOther",
                "GarageQualFa", "SaleTypeConLI", "ElectricalMix")
train <- train %>% select(-one_of(drop.cols4))

drop.cols5 <- c("Exterior1stImStucc", "RoofStyleShed", "Exterior2ndCBlock", "Exterior1stCBlock", "Exterior1stStone",
                "Exterior1stAsphShn")
test <- test %>% select(-one_of(drop.cols5))



# Colnames normalized
cols_train <- colnames(train)
cols_train <- chartr(".", "_", cols_train)
cols_train <- chartr(" ", "_", cols_train)
colnames(train) <- cols_train

cols_test <- colnames(test)
cols_test <- chartr(".", "_", cols_test)
cols_test <- chartr(" ", "_", cols_test)
colnames(test) <- cols_test


# Features & Labels
y_train <- train[ ,"SalePrice"]
x_train <- train
x_train$SalePrice <- NULL

y_test <- test[ ,"SalePrice"]
x_test <- test
x_test$SalePrice <- NULL

# Histogram of Sale price train
s <- ggplot(data=train,aes(x=SalePrice))
s + geom_histogram(binwidth = 50000,color="Black")

# Histogram of Sale price test
x <- ggplot(data=test,aes(x=SalePrice))
x + geom_histogram(binwidth = 50000,color="Black")
# Both test & train set have similar distribution of sale price


# Creating 2 labels for sale price 
y_train_label <- cut(train$SalePrice, c(0, 150000, 756000), right=FALSE, labels=c("below","upper"))
y_train_label<-(y_train_label=="upper")
y_train_label<-as.numeric(y_train_label) # train set

y_test_label <- cut(test$SalePrice, c(0, 150000, 750000), right=FALSE, labels=c("below","upper"))
y_test_label<-(y_test_label=="upper")
y_test_label<-as.numeric(y_test_label)# test set













################## Stepwise Feature Selection #################
attach(train)

x_train_rf <- as.data.frame(x_train)
y_train_rf <- as.numeric(y_train)

selectionFit <- caret::train(x_train_rf, y_train_rf, method = "lmStepAIC", # boosting parameter
                       trControl = bootControl, verbose = FALSE, # CV parameter
                       bag.fraction = 0.5)

summary(selectionFit)

# selected features listed below

train.selected <- train[, c("MSZoningCall" , "MSZoningRL" , "LotArea" , 
                              "LandContourHLS" , "LandContourLow" , "LandContourLvl" , "LotConfigCulDSac" , 
                              "LotConfigFR2" , "LandSlopeSev" , "NeighborhoodBrDale" , "NeighborhoodCrawfor" , 
                              "NeighborhoodEdwards" , "NeighborhoodMeadowV" , "NeighborhoodNAmes" , 
                              "NeighborhoodNoRidge" , "NeighborhoodNPkVill" , "NeighborhoodNridgHt" , 
                              "NeighborhoodSomerst" , "NeighborhoodStoneBr" , "NeighborhoodVeenker" , 
                              "Condition1Norm" , "Condition1PosN" , "Condition1RRAe" , "BldgTypeTwnhs" , 
                              "BldgTypeTwnhsE" , "HouseStyle1_5Unf" , "HouseStyle1Story" , "HouseStyle2Story" , 
                              "HouseStyleSFoyer" , "OverallQual" , "RoofStyleHip" , "Exterior1stHdBoard" , 
                              "Exterior1stMetalSd" , "Exterior1stPlywood" , "Exterior1stStucco" , 
                              "Exterior1stVinylSd" , "Exterior1stWd_Sdng" , "Exterior1stWdShing" , 
                              "Exterior2ndBrkFace" , "Exterior2ndCmentBd" , "Exterior2ndWd_Sdng" , 
                              "MasVnrArea" , "ExterQualGd" , "ExterQualTA" , "BsmtQualFa" , "BsmtQualGd" , 
                              "BsmtQualTA" , "BsmtCondTA" , "BsmtExposureGd" , "BsmtExposureNo" , 
                              "BsmtFinType1LwQ" , "BsmtFinSF1" , "BsmtFinType2BLQ" , "BsmtFinType2LwQ" , 
                              "BsmtFinType2Rec" , "BsmtFinType2Unf" , "HeatingQCGd" , "HeatingQCTA" , 
                              "GrLivArea" , "FullBath" , "HalfBath" , "BedroomAbvGr" , "KitchenAbvGr" , 
                              "KitchenQualFa" , "KitchenQualGd" , "KitchenQualTA" , "TotRmsAbvGrd" , 
                              "FunctionalTyp" , "Fireplaces" , "GarageTypeDetchd" , "GarageArea" , 
                              "GarageCondTA" , "WoodDeckSF" , "ScreenPorch" , "MoSold" , "SaleTypeCWD" , 
                              "SaleTypeNew" , "SaleConditionAlloca" , "SaleConditionNormal" , 
                              "SaleConditionPartial" , "RemodelAge")]


test.selected <- test[, c("MSZoningCall" , "MSZoningRL" , "LotArea" , 
                            "LandContourHLS" , "LandContourLow" , "LandContourLvl" , "LotConfigCulDSac" , 
                            "LotConfigFR2" , "LandSlopeSev" , "NeighborhoodBrDale" , "NeighborhoodCrawfor" , 
                            "NeighborhoodEdwards" , "NeighborhoodMeadowV" , "NeighborhoodNAmes" , 
                            "NeighborhoodNoRidge" , "NeighborhoodNPkVill" , "NeighborhoodNridgHt" , 
                            "NeighborhoodSomerst" , "NeighborhoodStoneBr" , "NeighborhoodVeenker" , 
                            "Condition1Norm" , "Condition1PosN" , "Condition1RRAe" , "BldgTypeTwnhs" , 
                            "BldgTypeTwnhsE" , "HouseStyle1_5Unf" , "HouseStyle1Story" , "HouseStyle2Story" , 
                            "HouseStyleSFoyer" , "OverallQual" , "RoofStyleHip" , "Exterior1stHdBoard" , 
                            "Exterior1stMetalSd" , "Exterior1stPlywood" , "Exterior1stStucco" , 
                            "Exterior1stVinylSd" , "Exterior1stWd_Sdng" , "Exterior1stWdShing" , 
                            "Exterior2ndBrkFace" , "Exterior2ndCmentBd" , "Exterior2ndWd_Sdng" , 
                            "MasVnrArea" , "ExterQualGd" , "ExterQualTA" , "BsmtQualFa" , "BsmtQualGd" , 
                            "BsmtQualTA" , "BsmtCondTA" , "BsmtExposureGd" , "BsmtExposureNo" , 
                            "BsmtFinType1LwQ" , "BsmtFinSF1" , "BsmtFinType2BLQ" , "BsmtFinType2LwQ" , 
                            "BsmtFinType2Rec" , "BsmtFinType2Unf" , "HeatingQCGd" , "HeatingQCTA" , 
                            "GrLivArea" , "FullBath" , "HalfBath" , "BedroomAbvGr" , "KitchenAbvGr" , 
                            "KitchenQualFa" , "KitchenQualGd" , "KitchenQualTA" , "TotRmsAbvGrd" , 
                            "FunctionalTyp" , "Fireplaces" , "GarageTypeDetchd" , "GarageArea" , 
                            "GarageCondTA" , "WoodDeckSF" , "ScreenPorch" , "MoSold" , "SaleTypeCWD" , 
                            "SaleTypeNew" , "SaleConditionAlloca" , "SaleConditionNormal" , 
                            "SaleConditionPartial" , "RemodelAge")]


################## Lasso Regression #################

x <- model.matrix(train$SalePrice~., train)[,-1]
y <- train$SalePrice

cv <- cv.glmnet(x, y, alpha = 1)
# Display the best lambda value
cv$lambda.min

# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
# Dsiplay regression coefficients
coef(model)


summary(model)

rsq = 1 - cv$cvm/var(y)
plot(cv$lambda,rsq)

# Make predictions on the test data
x.test <- model.matrix(SalePrice ~., test)[,-1]
predictions.lasso <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions.lasso, x.test$SalePrice),
  Rsquare = R2(predictions.lasso, x.test$SalePrice)
)







################## Random Forest Regressor #################

RF.reg = randomForest(SalePrice ~ . , data = train)

RF.reg

set.seed(5)
yhat = predict(RF.reg, newdata=test)
mean((yhat - y_test)^2) # RMSE = 29121

data.frame(
  RMSE = RMSE(yhat, test.selected$SalePrice),
  Rsquare = R2(yhat, test.selected$SalePrice)
)



# Make predictions on the test data
x.test <- model.matrix(SalePrice ~., test.selected)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.selected$SalePrice),
  Rsquare = R2(predictions, test.selected$SalePrice)
)




################## Gradient Boosting Regressor #################

cl <- makePSOCKcluster(4)
registerDoParallel(cl)



bootControl <- trainControl(method = "boot",number = 10)  
set.seed(5)

gbmFit.r <- caret::train(x_train, y_train, method = "gbm", 
                       trControl = bootControl, verbose = FALSE, 
                       bag.fraction = 0.5,
                       tuneLength = 4) 

summary(gbmFit.r)

gbmFit.r$results 
gbmFit.r$bestTune 

pred.gbm.r <- gbmFit.r %>% predict(x_test)

data.frame(
  RMSE = RMSE(pred.gbm.r, y_test),
  Rsquare = R2(pred.gbm.r, y_test))





#### Fitting best parameters to GBM
set.seed(5)
gbmBest<-gbm(y_train ~ . , data = x_train,
             distribution = "gaussian",
             interaction.depth=4,
             shrinkage = 0.1,
             n.minobsinnode = 10,
             n.trees = 150)


summary(gbmBest)

##### check for overfit
set.seed(5)
yhat_train = predict(gbmBest, newdata=x_train, n.trees = 200)
sqrt(mean((yhat_train - y_train)^2)) # RMSE = 16074


# R-squared value of train set GBM
set.seed(5)
residuals_gbm_train = y_train - yhat_train
y_train_mean = mean(y_train)
# Calculate total sum of squares
tss_train =  sum((y_train - y_train_mean)^2)
# Calculate residual sum of squares
rss_gbm_train =  sum(residuals_gbm_train^2)
# Calculate R-squared
rsq_gbm_train =  1-(rss_gbm_train/tss_train) # R-squared = 95.9% train




# Predictions on Test set
set.seed(5)
yhat = predict(gbmBest, newdata=x_test, n.trees = 200)
sqrt(mean((yhat - y_test)^2)) # RMSE = 29121
# R-squared value of test set
set.seed(5)
residuals_gbm = y_test - yhat
y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2)
# Calculate residual sum of squares
rss_gbm =  sum(residuals_gbm^2)
# Calculate R-squared
rsq_gbm =  1-(rss_gbm/tss) # R-squared = 85.8%



################# XGBoost Regressor #################
#convert regular data to xgboost data 
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

dtrain <- xgb.DMatrix(data = x_train,label = y_train)
dtest <- xgb.DMatrix(data = x_test,label=y_test)


set.seed(5)
fitControl <- trainControl(# 10-fold CV
  method = "boot",
  number = 10)

xgbFit.r <- caret::train(x_train, y_train, method = "xgbTree", 
                       trControl = fitControl, verbose = T, 
                       tuneLength = 4) 

xgbFit.r$results
xgbFit.r$bestTune

pred_xgb_r <- xgbFit.r %>% predict(x_test)

data.frame(
  RMSE = RMSE(pred_xgb_r, y_test),
  Rsquare = R2(pred_xgb_r, y_test)
)

# predictions
set.seed(5)
params <- list(booster = "gbtree", objective = "reg:linear", 
               eta=0.3, gamma=0, max_depth=4, min_child_weight=1, 
               subsample=0.8333333, colsample_bytree=0.8)


xgbBest <- xgb.train (params = params, data = dtrain, 
                   nrounds = 200)

# predictions are made 
yhat2=predict(xgbBest, newdata=dtest)
sqrt(mean((yhat2-y_test)^2)) # RMSE = 27957

# importance
importance_matrix <- xgb.importance(model = xgbBest)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


# another prediction
predicted = predict(xgbBest, dtest)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2))

# R-squared value of test set
y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq_xg  =  1 - (rss/tss) # R-squared = 86.9%



################# XGBoost Classification #################
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

dtrain.c <- xgb.DMatrix(data = x_train,label = y_train_label)
dtest.c <- xgb.DMatrix(data = x_test,label=y_test_label)

set.seed(5)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5, repeats = 2)

xgbFit.c <- caret::train(x_train, as.factor(y_train_label), method = "xgbTree",
                       trControl = fitControl, verbose = T, 
                       metric = "Accuracy",
                       tuneLength = 4) 


xgbFit.c$results
xgbFit.c$bestTune


pred_xgb.c <- xgbFit.c %>% predict(x_test)

table(pred_xgb.c, y_test_label)
mean(pred_xgb.c==y_test_label)




# fitting XGB classifier
set.seed(5)
params.xgb.c <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=2, min_child_weight=1, 
               subsample=0.8333333, colsample_bytree=0.8)


xgbBest.c <- xgb.train (params = params.xgb.c, data = dtrain.c, 
                   nrounds = 150)




# train set predictions
xgbC.pred.train <- predict (xgbBest.c, dtrain.c)
xgbC.pred.train <- ifelse (xgbC.pred.train > 0.5,1,0)

table(xgbC.pred.train, y_train_label) 
mean(xgbC.pred.train==y_train_label) # 94.8% accuracy on train set 



# test set predictions
xgbC.pred <- predict (xgbBest.c, dtest.c)
xgbC.pred <- ifelse (xgbC.pred > 0.5,1,0)

table(xgbC.pred, y_test_label) 
mean(xgbC.pred==y_test_label) # 91.5% accuracy on test set 

# important features
mat <- xgb.importance (feature_names = colnames(x_train),model = xgbBest.c)
xgb.plot.importance (importance_matrix = mat[1:20])


################# GBM Classification #################



set.seed(5)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5, repeats = 2)

gbmFit.c <- caret::train(x_train, as.factor(y_train_label), method = "gbm", 
                         trControl = fitControl, verbose = T, 
                         metric = "Accuracy",
                         tuneLength = 4)


gbmFit.c$results
gbmFit.c$bestTune

pred_gbm.c <- gbmFit.c %>% predict(x_test)

table(pred_gbm.c, y_test_label)
mean(pred_gbm.c == y_test_label)







#### Caret model predictions on test
pred_gbm.c <- gbmFit.c %>% predict(x_test_selected)

# Model prediction performance test set
table(predictions_xgb.c, y_test_label)
mean(predictions_xgb.c==y_test_label)

# Model prediction performance train set
predictions_xgb.c_train <- xgbFit.c %>% predict(x_train_selected)
table(predictions_xgb.c_train, y_train_label)
mean(predictions_xgb.c_train==y_train_label)






################# AdaBoost (Bagged) Classification #################
set.seed(5)
fitControl <- trainControl(# 10-fold CV
  method = "boot",
  number = 10)

adaFit.c <- caret::train(x_train, as.factor(y_train_label), method = "AdaBag", # method selected as adaboost
                         trControl = fitControl, verbose = T, # CV
                         metric = "Accuracy",
                         tuneLength = 2) # random search for 4 best parameters


adaFit.c$results # 86.3% accuracy
adaFit.c$bestTune












################## Scaling  #################
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

for (var in 1:ncol(train)) {
  if (class(train[,var]) %in% c("integer", "numeric")) {
    train[, var] <- normalize(train[, var])
  }
}






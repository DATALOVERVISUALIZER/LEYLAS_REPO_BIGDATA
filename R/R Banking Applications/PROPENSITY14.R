



library(cluster)
library(Rtsne)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(rlang)
library(caret)
library(multcomp)
library(stringr)
library(xgboost)
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(FactoMineR)
library(e1071)
library(gbm)
library(data.table)
library(mlr)
library(Hmisc)
library(GGally)
library(stats4)
library(SmartEDA)
library(doParallel)
library(plyr)
library(dplyr)
library(adabag)
library(leaps)
library(elasticnet)
library(glmnet)
library(dplyr)
library(caTools)
library(caret)
library(data.table)


 
######################################## GET  DATA ########################################

rm(list = ls()) # clean all
options(scipen = 999)




# Data
setwd("C:/Users/FB006680/Desktop/MY_DATA_SCIENCE/SPRINTS/SPRINT14/Part I/Propensity Model/")
getwd()


#get data
train <- read.csv2("train.csv")
test <- read.csv2("test.csv")

table(test$VERIKAYNAK_STATU_ID)



# NAs by features
NAs.train <-
  as.data.frame(sapply(train, function(x)
    sum(is.na(x)) / length(x)) * 100)
NAs.test <-
  as.data.frame(sapply(test, function(x)
    sum(is.na(x)) / length(x)) * 100)


#test datada operation  tablosu olusturma ve GELEN TEST DATASINdan predict edilecek kolonları bulma

test_operation_colnames <-
  c (
    "MUSTERI_NO",
    "TC_KIMLIK_NO",
    "MUSTERI_MESLEK",
    "MUSTERI_SUBESI_KODU",
    "MUSTERI_SUBESI_ADI",
    "BHCS_DURUM",
    "SHRT_NM",
    "FULL_NM",
    "CITY_CODE",
    "MUS_CEP_TEL",
    "MUS_IS_TEL",
    "PORTFOLIOCODE",
    "MUS_EMAIL",
    "MERNIS_ADRES_IL",
    "MERNIS_ADRES_ILCE",
    "EMAIL_DURUM",
    "GSM_TEL_DURUM",
    "IS_TEL_DURUM",
    "IZIN_POSTA",
    "IZIN_SMS",
    "IZIN_CM",
    "MUSTERI_NO",
    "VERIKAYNAK_STATU_ID",
    "MUSTERI_TIPI",
    "MUSTERI_YASI",
    "MUSTERI_SEGMENT",
    "MUSTERI_SINIFI",
    "MUSTERI_CINSIYET",
    "ONONAYIHT_INITIATIONDATE",
    "ONONAYIHT_LIMIT_GCRLLK_TRH",
    "ONONAYIHT_MAXCREDITAMOUNT",
    "ONONAYIHT_RESULTTEXT",
    "ONONAY3R_INITIATIONDATE",
    "ONONAY3R_LIMIT_GCRLLK_TRH",
    "ONONAY3R_MAXCREDITAMOUNT",
    "ONONAY3R_RESULTTEXT",
    "ONONAY5T_INITIATIONDATE",
    "ONONAY5T_LIMIT_GCRLLK_TRH",
    "ONONAY5T_MAXCREDITAMOUNT",
    "ONONAY5T_RESULTTEXT",
    "ONONAYIHT_SUBPRODUCTTYPE",
    "KKB_SKORU",
    "KKB_EGILIM_SKORU",
    "KKB_SORGU_TRH",
    "ALTMIS_GUN_KRD_KULLANDI_FLAG",
    "MIKS_RL3_4_LAST30GUN_FLAG",
    "PHONECALL_HISTORY_ACIK_FLAG",
    "BRANCH_ACTIVITY_ACIK_FLAG",
    "FIELDSALES_HISTORY_BEKLE_FLAG",
    "PHONECALL_HISTORY_TMMLN_FLAG",
    "BRANCH_ACTIVITY_TMMLN_FLAG",
    "FIELDSALES_HISTORY_31CALL_FLAG"
    #  "PHONECALLHISTORY_PHONECALLTYPE",
    # "BRANCH_ACTIVITY_CAMPAIGN_NAME" ,          ## is birimi talebi ile kapatildi
    #  "FIELDSALESHISTORY_CAMPAIGNNAME"
  )



test_operation <-
  test %>% dplyr::select(one_of(test_operation_colnames))

test$INTERNET_MOBIL_KULLANIMI <- NULL

colnames(test_operation)


#PREPARE TEST DATA

drop.cols_test <-
  c (
    "TC_KIMLIK_NO",
    "MUSTERI_SUBESI_KODU",
    "MUSTERI_SUBESI_ADI",
    "BHCS_DURUM",
    "MERNIS_ADRES_IL",
    "MUSTERI_NUMARASI",
    "NATIONALIDENTITYNO",
    "MUSTERI_NO",
    "MERNIS_ADRES_ILCE",
    "EMAIL_DURUM",
    "GSM_TEL_DURUM",
    "IS_TEL_DURUM",
    "IZIN_POSTA",
    "IZIN_SMS",
    "IZIN_CM",
    "ONONAYIHT_INITIATIONDATE",
    "ONONAYIHT_LIMIT_GCRLLK_TRH",
    "ONONAYIHT_MAXCREDITAMOUNT",
    "ONONAYIHT_RESULTTEXT",
    "ONONAY3R_INITIATIONDATE",
    "ONONAY3R_LIMIT_GCRLLK_TRH",
    "ONONAY3R_MAXCREDITAMOUNT",
    "ONONAY3R_RESULTTEXT",
    "ONONAY5T_INITIATIONDATE",
    "ONONAY5T_LIMIT_GCRLLK_TRH",
    "ONONAY5T_MAXCREDITAMOUNT",
    "ONONAY5T_RESULTTEXT",
    "ONONAYIHT_SUBPRODUCTTYPE",
    "KKB_SKORU",
    "KKB_EGILIM_SKORU",
    "KKB_SORGU_TRH",
    "ALTMIS_GUN_KRD_KULLANDI_FLAG",
    "MIKS_RL3_4_LAST30GUN_FLAG",
    "PHONECALL_HISTORY_ACIK_FLAG",
    "BRANCH_ACTIVITY_ACIK_FLAG",
    "FIELDSALES_HISTORY_BEKLE_FLAG",
    "PHONECALL_HISTORY_TMMLN_FLAG",
    "BRANCH_ACTIVITY_TMMLN_FLAG",
    "FIELDSALES_HISTORY_31CALL_FLAG",
    "SHRT_NM",
    "MUS_EMAIL",
    "FULL_NM",
    "PORTFOLIOCODE",
    "MUS_IS_TEL",
    "MUS_CEP_TEL",
    "CITY_CODE",
    "PHONECALLHISTORY_PHONECALLTYPE",
    "BRANCH_ACTIVITY_CAMPAIGN_NAME" ,
    "FIELDSALESHISTORY_CAMPAIGNNAME"
  )


test <- test %>% dplyr::select(-one_of(drop.cols_test))


train$MUSTERI_NUMARASI <- NULL
train$MUSTERI_NO <- NULL
train$NATIONALIDENTITYNO <- NULL

test$MUSTERI_NO <- NULL
test$NATIONALIDENTITYNO <- NULL
test$APPNO.1 <- NULL

#setdiff(train, test)

#set all missing value as "Missing"
#train[is.na(train)] <- "Missing"
#test[is.na(test)] <- "Missing"


############################################
table(is.na(train))

# KNOW ABOUT DATA
#train
nrow(train)  #68488
NCOL(train) #366

#test
nrow(test)  #1073491
NCOL(test) # 366



################################################# DATA TYPES
#TRAIN
#print(train_datatypes)
train_datatypes <-
  as.data.frame((lapply(train, class)))  # get train data data types
#test
#print(train_datatypes)
test.datatypes <-
  as.data.frame((lapply(test, class)))  # get train data data types



################Numeric and factor values detect

require(MASS)
require(dplyr)

#train
train.factors <- train[, sapply(train, is.factor)]
colnames(train.factors)
#describe(train.factors)

#test
test.factors <- test[, sapply(test, is.factor)]
colnames(test.factors)
#describe(test.factors)




#####Delete unnecessary columns

drop.cols <-
  c("MUSTERI_TIPI",
    "MUSTERI_SEGMENT",
    "MUSTERI_SINIFI",
    "MUSTERI_VERI_KAYNAGI") #musteri meslek daha sonra tekrar dusunulecek.low variance sebebiye silindi
train <- train %>% dplyr::select(-one_of(drop.cols))
test <- test %>% dplyr::select(-one_of(drop.cols))

# MUSTERI_TIPIzeo variance variable

#################ENCODE
#TRAIN
dmy_train <-
  dummyVars(
    "~ MUSTERI_KANALI + MUSTERI_CALISMA_DURUMU + MUSTERI_MESLEK+ EGITIM_DURUMU + MUSTERI_CINSIYET+  MEDENI_DURUM",
    data = train,
    fullRank = T
  )
df_all_ohe_train <- as.data.frame(predict(dmy_train, newdata = train))
ohe_feats_train <-
  c(
    'MUSTERI_KANALI',
    'MUSTERI_CALISMA_DURUMU',
    'MUSTERI_MESLEK',
    'EGITIM_DURUMU',
    'MUSTERI_CINSIYET',
    'MEDENI_DURUM'
  )
train <-
  cbind(train[, -c(which(colnames(train) %in% ohe_feats_train))], df_all_ohe_train)



#tst
dmy_test <-
  dummyVars(
    "~ MUSTERI_KANALI + MUSTERI_CALISMA_DURUMU + MUSTERI_MESLEK+ EGITIM_DURUMU + MUSTERI_CINSIYET+  MEDENI_DURUM"
    ,
    data = test,
    fullRank = T
  )

df_all_ohe_test <- as.data.frame(predict(dmy_test, newdata = test))
ohe_feats_test <-
  c(
    'MUSTERI_KANALI',
    'MUSTERI_CALISMA_DURUMU',
    'MUSTERI_MESLEK',
    'EGITIM_DURUMU',
    'MUSTERI_CINSIYET',
    'MEDENI_DURUM'
  )
test <-
  cbind(test[, -c(which(colnames(test) %in% ohe_feats_test))], df_all_ohe_test)



#encode 'dan kaynaklı col diference düzeltme
setdiff(train, test)




drop.cols2 <-
  c(
    "MEDENI_DURUM.Bekar",
    "MUSTERI_KANALI.TELE PERFORMANCE",
    "MUSTERI_KANALI.BIREYSEL INTERNET",
    "MUSTERI_KANALI.BATCH",
    "MUSTERI_KANALI.ONLINE SAT-TARFIN",
    "MUSTERI_KANALI.TOPLU M?STERI KAZANIMI",
    "MUSTERI_CINSIYET.E",
    "MUSTERI_CINSIYET. "
  )


test <- test %>% dplyr::select(-one_of(drop.cols2))

drop.cols_train <-
  c("MUSTERI_KANALI.WebService")

train <- train %>% dplyr::select(-one_of(drop.cols_train))




#Tekrar factor kolonlar bulunur
#train
train.factors <- train[, sapply(train, is.factor)]
colnames(train.factors)


#test
test.factors <- test[, sapply(test, is.factor)]
colnames(test.factors)


test$CIHAZ_TAKSIT_SAYISI <- NULL
train$CIHAZ_TAKSIT_SAYISI <- NULL


#Encode sonrası numeric olması gerekirken factor gelen feature'lar düzeltme. 
#Bu kolonlarda harf değerleri olabilir tekrar değerlendirilmeli

#train
cols.num_train <- c(colnames(train.factors))

train[cols.num_train] <- sapply(train[cols.num_train], as.numeric)


#test
cols.num.test <- c(colnames(test.factors))

test[cols.num.test] <- sapply(test[cols.num.test], as.numeric)

#check data types
#TRAIN
#train
train.factors <- train[, sapply(train, is.factor)]
colnames(train.factors)

#test
test.factors <- test[, sapply(test, is.factor)]
colnames(test.factors)

############################################# PREPARE X_TRAIN Y_TRAIN

y_train <- train[, "KREDI_STATU_ID"] # test edilecek hedef
x_train <- train
x_train$KREDI_STATU_ID <- NULL

y_test <- test[, "KREDI_STATU_ID"]
x_test <- test
x_test$KREDI_STATU_ID <- NULL

table(y_test)

setdiff(x_train, x_test)


#make dependent variables to dataframe
y_train <- as.data.frame(y_train)
y_test <- as.data.frame(y_test)
dim(x_train)  # 68481   439
dim(y_train)  #68488     1
dim(x_test) #1042135     439
dim(y_test) #1073491       1



#make all dataframe matrix
x_train_m <- as.matrix(x_train)
x_test_m <- as.matrix(x_test)
y_train_m <- as.matrix(y_train)
y_test_m <- as.matrix(y_test)


#convert regular data to xgboost data
dtrain.c <- xgb.DMatrix(data = x_train_m, label = y_train_m)
dtest.c <- xgb.DMatrix(data = x_test_m, label = y_test_m)


################################### MODEL ###################################


################################## XGB MODEL ###################################
#XGBOOST MODEL PARAMETERS
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
# rmse ? root mean square error
# mae ? mean absolute error
# logloss ? negative log-likelihood
# error ? Binary classification error rate (0.5 threshold)
# merror ? Multiclass classification error rate
# mlogloss ? Multiclass logloss
# auc: Area under the curve

# GridSearch

c <- makePSOCKcluster(8)
registerDoParallel(c)

#stopCluster(c)


set.seed(5)
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 2) #10K FOLD


############# TUNE LENGTH FITTING  ###############
#GBM FIT BY tuneLength METHOD, R Caret will make its own grid search

xgbFit.tuneLength <-
  caret::train(
    x_train_m,
    as.factor(y_train_m),
    method = "xgbTree",
    trControl = fitControl,
    verbose = T,
    metric = "Accuracy",
    tuneLength = 4
  )



xgbFit.tuneLength$bestTune
xgbFit.tuneLength$results[252, ]







#RESULTS FOR TUNELENGTH AND 10K FOLD

plot(xgbFit.tuneLength)
plot(xgbFit.tuneLength, plotType = "level")
XGBOOST_TUNELENGTH_RESULTS <- xgbFit.tuneLength$results
print(XGBOOST_TUNELENGTH_RESULTS)

best(xgbFit.tuneLength$results,
     metric = "Accuracy",
     maximize = T) #252
tolerance(
  xgbFit.tuneLength$results,
  metric = "Accuracy",
  maximize = T,
  tol = 2
) #tolarence 108

#stopCluster(cl)
#importance of the model
head(xgbFit.tuneLength)



#nrounds: #200
#max_depth : 4
#eta :0.4
#gamma :0
#colsample_bytree : 0.6
#min_child_weight:  1
#subsample:  1



########################### FITTING TUNELENTGH MODEL


#TUNE LENGHTH

set.seed(5)
params.xgb.c.Tune <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eta = 0.4,
    gamma = 0,
    max_depth = 4,
    min_child_weight = 1,
    subsample = 1,
    colsample_bytree = 0.8
  )


xgbBest.c.tune <-
  xgb.train (params = params.xgb.c.Tune,
             data = dtrain.c,
             nrounds = 200)



#nrounds :150
#max_depth: 4#
#eta:0.3
#gamma:0
#colsample_bytree:0.8
#min_child_weight:1
#subsample:1




#################################### PREDICTION #################


#�TUNE LENGTH PREDICTION

set.seed(5)
xgbC.pred <- predict (xgbBest.c.tune, dtest.c)

xgbC.pred_2 <- ifelse (xgbC.pred > 0.5, 1, 0)

table(xgbC.pred_2, y_test_m)
mean(xgbC.pred_2 == y_test_m) # 0.8581959  % accuracy on test set

#                 y_test_m
#xgbC.pred_2      0      1
#0               869710  10896
#1               136883  24646

precision(data = as.factor(xgbC.pred_2),
          reference = as.factor(y_test_m))

recall(data = as.factor(xgbC.pred_2),
       reference = as.factor(y_test_m))

# important features
mat <-
  xgb.importance (feature_names = colnames(x_train), model = xgbBest.c.tune)
xgb.plot.importance (importance_matrix = mat[1:20])

imps <- as.data.frame(mat)




#################################################### GIVE DATA #################################################### 

### GIVE DATA
Model.sprint.14 <- cbind(test_operation, xgbC.pred)

Model.sprint.14 <- Model.sprint.14[order(-xgbC.pred),]

n <- dim(Model.sprint.14)[1]

minus <- n - 1048575

Model.sprint.14 <- Model.sprint.14[1:(n-minus),]


# WRITE EXCEL
write.csv(Model.sprint.14, "Model.sprint.14.csv")







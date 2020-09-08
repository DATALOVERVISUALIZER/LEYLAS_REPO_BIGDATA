

########################################get necessary packages ############################
#install.packages("cluster",dependencies = TRUE, repos = "https://cran.r-project.org")
#install.packages("Rtsne",dependencies = TRUE, repos = "https://cran.r-project.org")
#install.packages("ggplot2",dependencies = TRUE, repos = "https://cran.r-project.org")
#install.packages("readr",dependencies = TRUE,repos = "https://cran.r-project.org")
#install.packages("dplyr",dependencies = TRUE,repos = "https://cran.r-project.org")
#install.packages("tidyverse",dependencies = TRUE)
#install.packages("rlang",dependencies = TRUE) 
#install.packages("mratios", dependencies = TRUE)
#install.packages("multcomp",dependencies = TRUE)
#install.packages("Rcpp")
#install.packages("caret",dependencies = TRUE)
#packageList <- c("randomForest", "plyr", "sqldf", "shiny", "lattice", "ggplot2", "googleplotvis", "caret", "e1071", "gbm", "glmnet", "kernlab", "ipred", "lda", "tm", "party", "nnet", "rpart", "Snowball", "openNLP", "stringr", "reshape2", "data.table")
#install.packages(packageList, dependencies=TRUE)
#install.packages("tidyverse")
#install.packages('Rcpp', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('data.table', dependencies = TRUE)
#install.packages('lattice', dependencies = TRUE)


rm(list=ls()) # clean all
options(scipen = 999)


"dplyr" %in% rownames(installed.packages()) #whether a package is downloaded
#installed.packages()

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
library(dplyr)



#install.packages("caTools")
library(caTools)

#.libPaths() # get library location 
library()   # see all packages installed 
#search()    # see packages currently loaded
library(caret)
#install.packages("data.table")
library(data.table)


############################################################################################################
########################################GET  DATA ############################
#clean environment
rm(list=ls()) # clean all
options(scipen = 999)



# Data 
setwd("C:/Users/FB006366/Desktop/SPRINTS/SPRINT9/MODEL/R")
getwd()






#get data 
train <- read.csv2("train.csv")


test <- read.csv2("test.csv")



# NAs by features
NAs.train <- as.data.frame(sapply(train, function(x) sum(is.na(x))/length(x))*100)
NAs.test <- as.data.frame(sapply(test, function(x) sum(is.na(x))/length(x))*100)


#remove leading whitespaces
#char_col <- colnames(train)[ sapply (test,is.character)]
#for(i in char_col) set(train,j=i,value = str_trim(train[[i]],side = "left"))
#for(i in char_col) set(test,j=i,value = str_trim(test[[i]],side = "left"))





#test datada operation  tablosu oluşturma ve GELEN TEST DATASINdan predict edilecek kolonları bulma
library(dplyr) 

test_operation_colnames<-c ("MUSTERI_NO","TC_KIMLIK_NO","MUSTERI_MESLEK","MUSTERI_SUBESI_KODU",
                             "MUSTERI_SUBESI_ADI","BHCS_DURUM","SHRT_NM","FULL_NM","CITY_CODE","MUS_CEP_TEL",
                             "MUS_IS_TEL","PORTFOLIOCODE","MUS_EMAIL","MERNIS_ADRES_IL",
                             "MERNIS_ADRES_ILCE","EMAIL_DURUM","GSM_TEL_DURUM","IS_TEL_DURUM",
                             "IZIN_POSTA","IZIN_SMS","IZIN_CM","MUSTERI_NO","VERIKAYNAK_STATU_ID","MUSTERI_TIPI",
                            "MUSTERI_YASI","MUSTERI_SEGMENT","MUSTERI_SINIFI",
                             "MUSTERI_CINSIYET","ONONAYIHT_INITIATIONDATE","ONONAYIHT_LIMIT_GCRLLK_TRH",
                            "ONONAYIHT_MAXCREDITAMOUNT","ONONAYIHT_RESULTTEXT","ONONAY3R_INITIATIONDATE",
                            "ONONAY3R_LIMIT_GCRLLK_TRH","ONONAY3R_MAXCREDITAMOUNT","ONONAY3R_RESULTTEXT",
                            "ONONAY5T_INITIATIONDATE","ONONAY5T_LIMIT_GCRLLK_TRH","ONONAY5T_MAXCREDITAMOUNT",
                            "ONONAY5T_RESULTTEXT","ONONAYIHT_SUBPRODUCTTYPE","KKB_SKORU","KKB_EGILIM_SKORU",
                            "KKB_SORGU_TRH","ALTMIS_GUN_KRD_KULLANDI_FLAG","MIKS_RL3_4_LAST30GUN_FLAG",
                            "PHONECALL_HISTORY_ACIK_FLAG","BRANCH_ACTIVITY_ACIK_FLAG","FIELDSALES_HISTORY_BEKLE_FLAG",
                            "PHONECALL_HISTORY_TMMLN_FLAG","BRANCH_ACTIVITY_TMMLN_FLAG","FIELDSALES_HISTORY_31CALL_FLAG"
                             )     



test_operation<-test %>% dplyr::select(one_of(test_operation_colnames))

test$LAST_ARAMA_RESULT<-NULL
test$LAST_IS_SUCCESSFUL<-NULL
test$LAST_PPINTMNT<-NULL
test$INTERNET_MOBIL_KULLANIMI<-NULL

colnames(test_operation)


#GET TEST 

drop.cols_test<- c ("TC_KIMLIK_NO","MUSTERI_SUBESI_KODU","MUSTERI_SUBESI_ADI",
                    "BHCS_DURUM","MERNIS_ADRES_IL","MUSTERI_NUMARASI","NATIONALIDENTITYNO","MUSTERI_NO",
                    "MERNIS_ADRES_ILCE","EMAIL_DURUM","GSM_TEL_DURUM","IS_TEL_DURUM",
                    "IZIN_POSTA","IZIN_SMS","IZIN_CM","ONONAYIHT_INITIATIONDATE","ONONAYIHT_LIMIT_GCRLLK_TRH",
                    "ONONAYIHT_MAXCREDITAMOUNT","ONONAYIHT_RESULTTEXT","ONONAY3R_INITIATIONDATE","ONONAY3R_LIMIT_GCRLLK_TRH",
                    "ONONAY3R_MAXCREDITAMOUNT","ONONAY3R_RESULTTEXT","ONONAY5T_INITIATIONDATE",
                    "ONONAY5T_LIMIT_GCRLLK_TRH","ONONAY5T_MAXCREDITAMOUNT","ONONAY5T_RESULTTEXT",
                    "ONONAYIHT_SUBPRODUCTTYPE","KKB_SKORU","KKB_EGILIM_SKORU","KKB_SORGU_TRH",
                    "ALTMIS_GUN_KRD_KULLANDI_FLAG","MIKS_RL3_4_LAST30GUN_FLAG",
                    "PHONECALL_HISTORY_ACIK_FLAG","BRANCH_ACTIVITY_ACIK_FLAG","FIELDSALES_HISTORY_BEKLE_FLAG",
                    "PHONECALL_HISTORY_TMMLN_FLAG","BRANCH_ACTIVITY_TMMLN_FLAG","FIELDSALES_HISTORY_31CALL_FLAG",
                    "SHRT_NM", "MUS_EMAIL", "FULL_NM", "PORTFOLIOCODE", "MUS_IS_TEL", "MUS_CEP_TEL","CITY_CODE") 

  
test <- test %>% dplyr::select(-one_of(drop.cols_test))


setdiff(train,test)

test$MUSTERI_NO<-NULL
test$NATIONALIDENTITYNO<-NULL
test$APPNO.1<-NULL

train$MUSTERI_NUMARASI<-NULL
train$MUSTERI_NO<-NULL
train$NATIONALIDENTITYNO<-NULL

#set all missing value as "Missing" 
#train[is.na(train)] <- "Missing"
#test[is.na(test)] <- "Missing"


############################################




table(is.na(train))



# KNOW ABOUT DATA 
#train
nrow(train)  #65045
NCOL(train) #366

#test
nrow(test)  #1001115
NCOL(test) # 366



################################################# DATA TYPES 
#TRAIN
#print(train_datatypes)
train_datatypes<-as.data.frame((lapply(train, class)))  # get train data data types
#test 
#print(train_datatypes)
test.datatypes<-as.data.frame((lapply(test, class)))  # get train data data types



################Numeric and factor values detect

require(MASS)
require(dplyr)

#train
train.factors <- train[, sapply(train, is.factor)] 
colnames(train.factors)
describe(train.factors)

#test
test.factors <- test[, sapply(test, is.factor)] 
colnames(test.factors)
describe(test.factors)




#####Delete unnecessary columns 

drop.cols<- c("MUSTERI_TIPI", "MUSTERI_SEGMENT","MUSTERI_MESLEK","MUSTERI_SINIFI","MUSTERI_CINSIYET","MUSTERI_VERI_KAYNAGI") #musteri meslek daha sonra tekrar dusunulecek.low variance sebebiye silindi
train <- train %>% dplyr::select(-one_of(drop.cols))
test <- test %>% dplyr::select(-one_of(drop.cols))

# MUSTERI_TIPIzeo variance variable 

#################ENCODE 
#TRAIN
dmy_train<-dummyVars("~ MUSTERI_KANALI + MUSTERI_CALISMA_DURUMU+EGITIM_DURUMU+MEDENI_DURUM", data = train, fullRank = T)
df_all_ohe_train<-as.data.frame(predict(dmy_train, newdata = train))
ohe_feats_train<-c('MUSTERI_KANALI','MUSTERI_CALISMA_DURUMU','EGITIM_DURUMU','MEDENI_DURUM')
train<-cbind(train[,-c(which(colnames(train) %in% ohe_feats_train))],df_all_ohe_train)



#tst
dmy_test<-dummyVars("~ MUSTERI_KANALI + MUSTERI_CALISMA_DURUMU+EGITIM_DURUMU+MEDENI_DURUM"
                     , data = test, fullRank = T)
df_all_ohe_test<-as.data.frame(predict(dmy_test, newdata = test))
ohe_feats_test<-c('MUSTERI_KANALI','MUSTERI_CALISMA_DURUMU','EGITIM_DURUMU','MEDENI_DURUM')
test<-cbind(test[,-c(which(colnames(test) %in% ohe_feats_test))],df_all_ohe_test)



#encode 'dan kaynaklı col diference düzeltme
setdiff(train,test)

drop.cols2<- c( "MUSTERI_KANALI.MOBVEN", "MUSTERI_KANALI.BATCH", 
                "MUSTERI_KANALI.TOPLU MÜSTERI KAZANIMI", "MUSTERI_KANALI.ONLINE SAT-ARACKIRA", 
                "MUSTERI_KANALI.ONLINE SAT-TARFIN", "MUSTERI_KANALI.TELE PERFORMANCE", 
                "MUSTERI_KANALI.BIREYSEL INTERNET")
test <- test %>% dplyr::select(-one_of(drop.cols2))





#Tekrar factor kolonlar bulunur
#train
train.factors <- train[, sapply(train, is.factor)] 
colnames(train.factors)


#test
test.factors <- test[, sapply(test, is.factor)] 
colnames(test.factors)





#Encode sonrası numeric olması gerekirken factor gelen feature'lar düzeltme. Bu kolonlarda harf değerleri olabilir tekrar değerlendirilmeli

#train
cols.num_train <- c( colnames(train.factors))

train[cols.num_train] <- sapply(train[cols.num_train],as.numeric)


#test
cols.num.test<-c(colnames(test.factors))

test[cols.num.test]<-sapply(test[cols.num.test],as.numeric)

#check data types 
#TRAIN
#train
train.factors <- train[, sapply(train, is.factor)] 
colnames(train.factors)

#test
test.factors <- test[, sapply(test, is.factor)] 
colnames(test.factors)


# Features & Labels

train$MUSTERI_NUMARASI<-NULL
test$MUSTERI_NUMARASI<-NULL
test$MUSTERI_NO<-NULL 
train$MUSTERI_NO<-NULL
train$CIHAZ_TAKSIT_SAYISI<-NULL
test$CIHAZ_TAKSIT_SAYISI<-NULL



y_train <- train[ ,"KREDI_STATU_ID"] # test edilecek hedef 
x_train <- train
x_train$KREDI_STATU_ID <- NULL 

y_test <- test[ ,"KREDI_STATU_ID"]
x_test <- test
x_test$KREDI_STATU_ID <- NULL


setdiff(x_train, x_test)


##MODEL

data_train_xgb<-xgb.DMatrix(data = as.matrix(x_train), label = y_train)

model_sprint8<-xgboost(data=data_train_xgb, max.depth=6, eta=0.01, nrounds=1000, objective="binary:logistic")


predictions<-predict(model_sprint8, newdata = as.matrix(x_test))

predictions<-as.data.frame(predictions)

Model.sprint9<-cbind(test_operation, predictions)


#WRITE EXCEL 
#install.packages("xlsx")
library("xlsx")
require(xlsx, quietly = TRUE)

write.csv(Model.sprint8, "Model.sprint9.csv")



######################################################### Check performance

#check rmse

#OR 
residuals = y_train - predictions


RMSE = sqrt(mean((residuals)^2))

#0.324536

#train-error:0.098790   

# importance
importance_matrix <- xgb.importance(model =model_sprint8)
print(importance_matrix)


#accuracy








#########################BOOSTING 

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


#convert data frame to data table
setDT(train)
setDT(test)


#dependent variable
labels <- y_train #train$target Traindaki target dependent variable 
ts_label <- y_test # test$target tstteki dependent variable 

#convert continuous dependent variable to categorical dependent variable
labels<-ifelse(labels >0.5, "1" ,"0")
labels<-as.numeric(labels)
ts_label<-ifelse(ts_label >0.5, "1" ,"0")
ts_label<-as.numeric(ts_label)


library("xgboost")

#convert regular data to xgboost data 
dtrain <- xgb.DMatrix(data = train,label = labels)
dtest <- xgb.DMatrix(data = test,label=ts_label)



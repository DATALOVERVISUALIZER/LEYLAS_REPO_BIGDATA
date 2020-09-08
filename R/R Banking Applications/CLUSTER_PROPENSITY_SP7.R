

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


#
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


#install.packages("caTools")
library(caTools)

.libPaths() # get library location 
library()   # see all packages installed 
#search()    # see packages currently loaded
library(caret)
#install.packages("data.table")
library(data.table)


############################################################################################################
########################################GET MODEL TRAIN DATA ############################

setwd("C://Users//FB006366//Desktop//SPRINTS//SPRINT7//MODEL//R//SALES_MODEL")
getwd()


#get  data
ModelData <- read.table("TRAIN_DATA.csv", header =TRUE,sep = ";")
saveRDS(ModelData, file = "ModelData.rds") #SC: salary customer detail data
#str(ModelData)
str(ModelData, list.len=ncol(ModelData))
head(ModelData)

#convert data frame to data table
setDT(ModelData)


#check missing values 
table(is.na(ModelData))
sapply(ModelData, function(x) sum(is.na(x))/length(x))*100




################################################Delete unnessay columns##################### 

ModelData$EGITIM_DURUMU
#delete unnessay columsn 
ModelData$KKBSKORHESAPLANMAMANEDENKODU<-NULL 
ModelData$MKKBSCOREREASONCODE<-NULL
ModelData$KKBSKORVERSIYON<-NULL
ModelData$BBEINDEX<-NULL
ModelData$BBEEXCCODE<-NULL
ModelData$KAYITVARFLAG<-NULL




## Bu kolonlara FEature Engineering yapılmalı mı?  yada şimdilik silmeli miyim? 
##ModelData$FIBAHRCENYENIIHTACLSTRH<-NULL
##ModelData$FFIBAHRCSONACLKARTTRH<-NULL
##ModelData$FFIBAHRCENYENIKMHACLSTRH<-NULL
##ModelData$FLSTKLT8TRHSNRSIACLKRDEKTODMDRM<-NULL
##ModelData$FLSTKLT8TRHSNRSINDAACLKRDSAY<-NULL
##ModelData$FFIBAHRCENYENIKONUTACLSTRH<-NULL
##ModelData$FTUMKRDSONKLT8TRH<-NULL
##ModelData$FFIBAHRCENYENITSTACLSTRH<-NULL
## ModelData$FFIBAHRCSONTKPTRH<-NULL 



ModelData$IZIN_CM<-NULL
ModelData$IZIN_POSTA<-NULL
ModelData$IZIN_SMS<-NULL
ModelData$CIHAZ_TAKSIT_SAYISI<-NULL
ModelData$INTERNET_MOBIL_KULLANIMI<-NULL




#MAKE NUMBER
#make factor data types number- continues data should be number 
ModelData$NORMALIZED_CHZ_KRD_AMT<-as.numeric(as.character(ModelData$NORMALIZED_CHZ_KRD_AMT))
ModelData$TUMKRDENKOTUODMDURUMUTUMZMNLAR<-as.numeric(as.character(ModelData$TUMKRDENKOTUODMDURUMUTUMZMNLAR))
ModelData$TUMKRDENKOTUODMDURUMUSON3AY<-as.numeric(as.character(ModelData$TUMKRDENKOTUODMDURUMUSON3AY))
ModelData$TUMKRDENKOTUODMDURUMUSON6AY<-as.numeric(as.character(ModelData$TUMKRDENKOTUODMDURUMUSON6AY))
ModelData$TUMKRDENKOTUODMDURUMUSON12AY<-as.numeric(as.character(ModelData$TUMKRDENKOTUODMDURUMUSON12AY))
ModelData$TUMKRDENKOTUODMDURUMUSON24AY<-as.numeric(as.character(ModelData$TUMKRDENKOTUODMDURUMUSON24AY))
ModelData$LSTKLT8TRHSNRSIACLKRDEKTODMDRM<-as.numeric(as.character(ModelData$LSTKLT8TRHSNRSIACLKRDEKTODMDRM))
ModelData$SORGUSUNDADIGERBANKAKRDSIVAREH<-as.numeric(as.character(ModelData$SORGUSUNDADIGERBANKAKRDSIVAREH))
ModelData$KFLTENDGNENKOTUGCKDURUMU<-as.numeric(as.character(ModelData$KFLTENDGNENKOTUGCKDURUMU))
ModelData$ENKOTUODEMESON24AY<-as.numeric(as.character(ModelData$ENKOTUODEMESON24AY))
ModelData$ENKOTUODEMESON12AY<-as.numeric(as.character(ModelData$ENKOTUODEMESON12AY))
ModelData$ENKOTUODEMESON6AY<-as.numeric(as.character(ModelData$ENKOTUODEMESON6AY))
ModelData$ENKOTUODEMESON3AY<-as.numeric(as.character(ModelData$ENKOTUODEMESON3AY))
ModelData$ENKOTUODEMESONAY<-as.numeric(as.character(ModelData$ENKOTUODEMESONAY))
ModelData$ENKOTUODEMEBIRESYELKRDSON24AY<-as.numeric(as.character(ModelData$ENKOTUODEMEBIRESYELKRDSON24AY))
ModelData$ENKOTUODEMEBIREYSELKRDSON12AY<-as.numeric(as.character(ModelData$ENKOTUODEMEBIREYSELKRDSON12AY))
ModelData$ENKOTUODEMEBIREYSELKRDSON6AY<-as.numeric(as.character(ModelData$ENKOTUODEMEBIREYSELKRDSON6AY))
ModelData$ENKOTUODEMEBIREYSELKRDSON3AY<-as.numeric(as.character(ModelData$ENKOTUODEMEBIREYSELKRDSON3AY))
ModelData$ENKOTUODEMEBIREYSELKRDSONAY<-as.numeric(as.character(ModelData$ENKOTUODEMEBIREYSELKRDSONAY))
ModelData$ENKOTUODEMEKKVEKMHSON24AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKKVEKMHSON24AY))
ModelData$ENKOTUODEMEKKVEKMHSON12AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKKVEKMHSON12AY))
ModelData$ENKOTUODEMEKKVEKMHSON6AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKKVEKMHSON6AY))
ModelData$ENKOTUODEMEKKVEKMHSON3AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKKVEKMHSON3AY))
ModelData$ENKOTUODEMEKKVEKMHSONAY<-as.numeric(as.character(ModelData$ENKOTUODEMEKKVEKMHSONAY))
ModelData$ENKOTUODEMEKONUTKRDSON24AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKONUTKRDSON24AY))
ModelData$ENKOTUODEMEKONUTKRDSON12AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKONUTKRDSON12AY))
ModelData$ENKOTUODEMEKONUTKRDSON6AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKONUTKRDSON6AY))
ModelData$ENKOTUODEMEKONUTKRDSON3AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKONUTKRDSON3AY))
ModelData$ENKOTUODEMEKONUTKRDSONAY<-as.numeric(as.character(ModelData$ENKOTUODEMEKONUTKRDSONAY))
ModelData$ENKOTUODEMETSTKRDSON24AY<-as.numeric(as.character(ModelData$ENKOTUODEMETSTKRDSON24AY))
ModelData$ENKOTUODEMETSTKRDSON12AY<-as.numeric(as.character(ModelData$ENKOTUODEMETSTKRDSON12AY))
ModelData$ENKOTUODEMETSTKRDSON6AY<-as.numeric(as.character(ModelData$ENKOTUODEMETSTKRDSON6AY))
ModelData$ENKOTUODEMETSTKRDSON3AY<-as.numeric(as.character(ModelData$ENKOTUODEMETSTKRDSON3AY))
ModelData$ENKOTUODEMETSTKRDSONAY<-as.numeric(as.character(ModelData$ENKOTUODEMETSTKRDSONAY))



ModelData$KLT8HRCENKOTUODMSTU<-as.numeric(as.character(ModelData$KLT8HRCENKOTUODMSTU))
ModelData$KEFILENKOTUODMSTU<-as.numeric(as.character(ModelData$KEFILENKOTUODMSTU))
ModelData$FIBAHRCGNCEKODRISKIOLAN<-as.numeric(as.character(ModelData$FIBAHRCGNCEKODRISKIOLAN))
ModelData$FIBAHRCL6MEKODRISKIOLAN<-as.numeric(as.character(ModelData$FIBAHRCL6MEKODRISKIOLAN))
ModelData$FIBAHRCL12MEKODRISKIOLAN<-as.numeric(as.character(ModelData$FIBAHRCL12MEKODRISKIOLAN))
ModelData$ENKOTUODEMEKKSON24AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKKSON24AY))
ModelData$ENKOTUODEMEKKSON12AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKKSON12AY))
ModelData$ENKOTUODEMEKKSON6AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKKSON6AY))
ModelData$ENKOTUODEMEKKSON3AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKKSON3AY))
ModelData$ENKOTUODEMEKKSONAY<-as.numeric(as.character(ModelData$ENKOTUODEMEKKSONAY))
ModelData$ENKOTUODEMEKMHSON24AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKMHSON24AY))
ModelData$ENKOTUODEMEKMHSON12AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKMHSON12AY))
ModelData$ENKOTUODEMEKMHSON6AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKMHSON6AY))
ModelData$ENKOTUODEMEKMHSON3AY<-as.numeric(as.character(ModelData$ENKOTUODEMEKMHSON3AY))
ModelData$ENKOTUODEMEKMHSONAY<-as.numeric(as.character(ModelData$ENKOTUODEMEKMHSONAY))


#################################################ENCOING###########################
dmy<-dummyVars("~ MUSTERI_CALISMA_DURUMU +  MUSTERI_SEGMENT + MUSTERI_CINSIYET + EGITIM_DURUMU +MEDENI_DURUM "
               , data = ModelData, fullRank = T)
df_all_ohe<-as.data.frame(predict(dmy, newdata = ModelData))
ohe_feats<-c('MUSTERI_CALISMA_DURUMU',  'MUSTERI_SEGMENT','MUSTERI_CINSIYET','EGITIM_DURUMU','MEDENI_DURUM' )
ModelDataEncoded<-cbind(ModelData[,-c(which(colnames(ModelData) %in% ohe_feats))],df_all_ohe)



##################################################################REMOVE CORRELATED DATA 
#remove data that has high correlation
#Hih

ncol(ModelDataEncoded)  
descrCorr <- cor(ModelDataEncoded)
highCorr <- findCorrelation(descrCorr, 0.90)
ModelDataEncoded <- ModelDataEncoded[, -highCorr]
testDescr <- testDescr[, -highCorr]
ncol(ModelDataEncoded)


str(ModelData)



###Variance'ı sıfıra yakın olanları eliyoruz //PCA gibi
nzv <- nearZeroVar(ModelDataEncoded, saveMetrics= TRUE)
nzv<-nzv[nzv$nzv,][1:10,]
ModelDataEncodedFiltered <- ModelDataEncoded[,-nzv]


#delete first variance columns (variance az olan kolonlar silini)
ModelDataEncoded$TUMKRD2DNMGCKLIKRDSAY <-NULL      
ModelDataEncoded$TUMKRD3DNMGCKLIKRDSAY <-NULL         
ModelDataEncoded$TUMKRD4VEFZLDNMGCKLIKRDSAY <-NULL    
ModelDataEncoded$TOPTKPKRDSAYKLT8 <-NULL             
ModelDataEncoded$TOPTKPKRDTKPBAKKLT8  <-NULL       
ModelDataEncoded$TUMKRD1DNMGCKLIBKY <-NULL          
ModelDataEncoded$TUMKRD2DNMGCKLIBKY <-NULL            
ModelDataEncoded$TUMKRD3DNMGCKLIBKY <-NULL            
ModelDataEncoded$TUMKRD4VEFZLDNMGCKLIBKY  <-NULL      
ModelDataEncoded$TUMKRDKLT8STATULUBKY <-NULL     


ModelDataEncodedFiltered<-ModelDataEncoded






#####################MODEL################################################################


split <- sample.split(ModelDataEncodedFiltered$KREDI_STATU_ID, SplitRatio = 0.70)
data_train<- subset(ModelDataEncodedFiltered , split == TRUE)
data_test<- subset(ModelDataEncodedFiltered , split == FALSE)




###Downsample , UNSAMPLİNG , TEST DATASINI undersample etmeyız
set.seed(9560)
sum(data_train$KREDI_STATU_ID==1)
data_train$KREDI_STATU_ID<-as.factor(data_train$KREDI_STATU_ID)
down_train <- downSample(x =data_train[,-9],y= data_train$KREDI_STATU_ID)


down_train<-as.data.frame(down_train)


#str(down_train, list.len=ncol(down_train)) # encode edilmiş data


down_train$KREDI_STATU_ID<-NULL
label<-down_train$Class
label<-as.numeric(as.character(label))


#delete unnesayy columns 
down_train$Class<-NULL
down_train$KREDI_STATU_ID<-NULL
down_train$MUSTERI_NO<-NULL
down_train$KKBQUERYID<-NULL
down_train$KKBSCOREREASONCODE<-NULL

down_train$IZIN_CM<-NULL
down_train$IZIN_POSTA<-NULL
down_train$IZIN_SMS<-NULL
down_train$CIHAZ_TAKSIT_SAYISI<-NULL
down_train$INTERNET_MOBIL_KULLANIMI<-NULL


data_train_xgb<-xgb.DMatrix(data = as.matrix(down_train), label = label)

model_sprint5<-xgboost(data=data_train_xgb, max.depth=6, eta=0.01, nrounds=1000, objective="binary:logistic")


#model tree buyuk oldugundan tree plot etmeye gerek yok
#dimensions<-dimnames(data_train_NoFlag)[[2]]
#xgb.plot.tree(feature_names = dimensions, model = model_sprint4)



###########################################FEATURE IMPORTANCE###################
imp<-xgb.importance(model=model_sprint5)

####Accuracy


data_test_NoFlag<-data_test
data_test_NoFlag$KREDI_STATU_ID<-NULL
data_test_NoFlag$MUSTERI_NO<-NULL
data_test_NoFlag$KKBQUERYID<-NULL
data_test_NoFlag$KKBSCOREREASONCODE<-NULL

#delete unnecessary columns 
data_test_NoFlag$IZIN_CM<-NULL
data_test_NoFlag$IZIN_POSTA<-NULL
data_test_NoFlag$IZIN_SMS<-NULL
data_test_NoFlag$CIHAZ_TAKSIT_SAYISI<-NULL
data_test_NoFlag$INTERNET_MOBIL_KULLANIMI<-NULL


library(pROC)

predictions<-predict(model_sprint5, newdata = as.matrix(data_test_NoFlag))
predictions<-as.data.frame(predictions)

result.roc <- roc(data_test$KREDI_STATU_ID, predictions$predictions) # Draw ROC curve. V1'den emin değilim

result.roc[["auc"]]  #0.8905

##Confusion matrix

True_or_not<-ifelse(predictions >0.5, "1" ,"0")
p_class_v1<-as.factor(as.character(True_or_not))

real_values<-as.factor(as.character(data_test$KREDI_STATU_ID))


data_test$KREDI_STATU_ID<-as.factor(as.character(data_test$KREDI_STATU_ID))
results_v1<-confusionMatrix(p_class_v1, real_values)
results_v1<-as.data.frame(as.matrix(results_v1))
TRUE_FALSE<-results_v1[2,2]/(results_v1[1,2]+results_v1[2,2]) 

mean(predictions$predictions)
mean(as.numeric(as.character(real_values)))
#########################################GET LEAD  DATA #############################

LeadListOriginal<-read.table("SPRINT5_LEAD_DATA.csv", header =TRUE,sep = ";")

str(LeadListOriginal,list.len=ncol(LeadListOriginal))
saveRDS(LeadListOriginal, file = "LeadListOriginal.rds") #SC: salary customer detail data



#delete unrelevant columns 

LeadListOriginal$KKBQUERYID<-NULL
LeadListOriginal$KKBSCOREREASONCODE<-NULL
LeadListOriginal$KKBSKORHESAPLANMAMANEDENKODU<-NULL 
LeadListOriginal$ModelDataKKBSCOREREASONCODE<-NULL
LeadListOriginal$KKBSKORVERSIYON<-NULL
LeadListOriginal$BBEINDEX<-NULL
LeadListOriginal$BBEEXCCODE<-NULL
LeadListOriginal$KAYITVARFLAG<-NULL
LeadListOriginal$KREDI_STATU_ID <-NULL 



#CONVERT NUMBER FROM FACTORS
LeadListOriginal$NORMALIZED_CHZ_KRD_AMT<-as.numeric(as.character(LeadListOriginal$NORMALIZED_CHZ_KRD_AMT))
LeadListOriginal$TUMKRDENKOTUODMDURUMUSON3AY<-as.numeric(as.character(LeadListOriginal$TUMKRDENKOTUODMDURUMUSON3AY))
LeadListOriginal$TUMKRDENKOTUODMDURUMUSON6AY<-as.numeric(as.character(LeadListOriginal$TUMKRDENKOTUODMDURUMUSON6AY))
LeadListOriginal$TUMKRDENKOTUODMDURUMUSON12AY<-as.numeric(as.character(LeadListOriginal$TUMKRDENKOTUODMDURUMUSON12AY))
LeadListOriginal$TUMKRDENKOTUODMDURUMUSON24AY<-as.numeric(as.character(LeadListOriginal$TUMKRDENKOTUODMDURUMUSON24AY))
LeadListOriginal$KFLTENDGNENKOTUGCKDURUMU<-as.numeric(as.character(LeadListOriginal$KFLTENDGNENKOTUGCKDURUMU))
LeadListOriginal$ENKOTUODEMESON24AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMESON24AY))
LeadListOriginal$ENKOTUODEMESON12AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMESON12AY))
LeadListOriginal$ENKOTUODEMESON6AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMESON6AY))
LeadListOriginal$ENKOTUODEMESON3AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMESON3AY))
LeadListOriginal$ENKOTUODEMESONAY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMESONAY))
LeadListOriginal$ENKOTUODEMEBIREYSELKRDSON6AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEBIREYSELKRDSON6AY))
LeadListOriginal$ENKOTUODEMEBIREYSELKRDSON3AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEBIREYSELKRDSON3AY))
LeadListOriginal$ENKOTUODEMEBIREYSELKRDSONAY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEBIREYSELKRDSONAY))
LeadListOriginal$ENKOTUODEMEKKVEKMHSON24AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKKVEKMHSON24AY))
LeadListOriginal$ENKOTUODEMEKKVEKMHSON12AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKKVEKMHSON12AY))
LeadListOriginal$ENKOTUODEMEKKVEKMHSON6AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKKVEKMHSON6AY))
LeadListOriginal$ENKOTUODEMEKKVEKMHSON3AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKKVEKMHSON3AY))
LeadListOriginal$ENKOTUODEMEKKVEKMHSONAY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKKVEKMHSONAY))
LeadListOriginal$ENKOTUODEMEKONUTKRDSON24AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKONUTKRDSON24AY))
LeadListOriginal$ENKOTUODEMEKONUTKRDSON12AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKONUTKRDSON12AY))
LeadListOriginal$ENKOTUODEMEKONUTKRDSON6AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKONUTKRDSON6AY))
LeadListOriginal$ENKOTUODEMEKONUTKRDSON3AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKONUTKRDSON3AY))
LeadListOriginal$ENKOTUODEMEKONUTKRDSONAY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKONUTKRDSONAY))
LeadListOriginal$ENKOTUODEMETSTKRDSON24AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMETSTKRDSON24AY))
LeadListOriginal$ENKOTUODEMETSTKRDSON12AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMETSTKRDSON12AY))
LeadListOriginal$ENKOTUODEMETSTKRDSON6AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMETSTKRDSON6AY))
LeadListOriginal$ENKOTUODEMETSTKRDSON3AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMETSTKRDSON3AY))
LeadListOriginal$ENKOTUODEMETSTKRDSONAY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMETSTKRDSONAY))
LeadListOriginal$KLT8HRCENKOTUODMSTU<-as.numeric(as.character(LeadListOriginal$KLT8HRCENKOTUODMSTU))
LeadListOriginal$KEFILENKOTUODMSTU<-as.numeric(as.character(LeadListOriginal$KEFILENKOTUODMSTU))
LeadListOriginal$FIBAHRCGNCEKODRISKIOLAN<-as.numeric(as.character(LeadListOriginal$FIBAHRCGNCEKODRISKIOLAN))
LeadListOriginal$FIBAHRCL6MEKODRISKIOLAN<-as.numeric(as.character(LeadListOriginal$FIBAHRCL6MEKODRISKIOLAN))
LeadListOriginal$FIBAHRCL12MEKODRISKIOLAN<-as.numeric(as.character(LeadListOriginal$FIBAHRCL12MEKODRISKIOLAN))
LeadListOriginal$ENKOTUODEMEKKSON24AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKKSON24AY))
LeadListOriginal$ENKOTUODEMEKKSON12AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKKSON12AY))
LeadListOriginal$ENKOTUODEMEKKSON6AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKKSON6AY))
LeadListOriginal$ENKOTUODEMEKKSON3AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKKSON3AY))
LeadListOriginal$ENKOTUODEMEKKSONAY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKKSONAY))
LeadListOriginal$ENKOTUODEMEKMHSON24AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKMHSON24AY))
LeadListOriginal$ENKOTUODEMEKMHSON12AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKMHSON12AY))
LeadListOriginal$ENKOTUODEMEKMHSON6AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKMHSON6AY))
LeadListOriginal$ENKOTUODEMEKMHSON3AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKMHSON3AY))
LeadListOriginal$ENKOTUODEMEKMHSONAY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEKMHSONAY))
LeadListOriginal$ENKOTUODEMEBIRESYELKRDSON24AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEBIRESYELKRDSON24AY))
LeadListOriginal$ENKOTUODEMEBIREYSELKRDSON12AY<-as.numeric(as.character(LeadListOriginal$ENKOTUODEMEBIREYSELKRDSON12AY))
LeadListOriginal$LSTKLT8TRHSNRSIACLKRDEKTODMDRM<-as.numeric(as.character(LeadListOriginal$LSTKLT8TRHSNRSIACLKRDEKTODMDRM))
LeadListOriginal$SORGUSUNDADIGERBANKAKRDSIVAREH<-as.numeric(as.character(LeadListOriginal$SORGUSUNDADIGERBANKAKRDSIVAREH))
LeadListOriginal$TUMKRDENKOTUODMDURUMUTUMZMNLAR<-as.numeric(as.character(LeadListOriginal$TUMKRDENKOTUODMDURUMUTUMZMNLAR))


#ENCODE DATA 
dmy<-dummyVars("~ MUSTERI_CALISMA_DURUMU+  MUSTERI_SEGMENT+ MUSTERI_CINSIYET + EGITIM_DURUMU+MEDENI_DURUM"
               , data = LeadListOriginal, fullRank = T)
df_all_ohe<-as.data.frame(predict(dmy, newdata = LeadListOriginal))
ohe_feats<-c('MUSTERI_CALISMA_DURUMU',  'MUSTERI_SEGMENT','MUSTERI_CINSIYET','EGITIM_DURUMU','MEDENI_DURUM')
LeadListEncoded<-cbind(LeadListOriginal[,-c(which(colnames(LeadListOriginal) %in% ohe_feats))],df_all_ohe)


str(LeadListEncoded,list.len=ncol(LeadListEncoded))
str(LeadListOriginal,list.len=ncol(LeadListOriginal))


saveRDS(LeadListEncoded, file = "LeadListEncoded.rds") #SC: salary customer detail data


#*******************************************DELETE OPERATIONAL COLUMNS FROM ORIGINAL DATA **********************************************
LeadListEncoded$MUSTERI_NO<-NULL 
LeadListEncoded$TC_KIMLIK_NO<-NULL 
LeadListEncoded$MUSTERI_MESLEK<-NULL 
LeadListEncoded$MUSTERI_SUBESI_KODU<-NULL 
LeadListEncoded$MUSTERI_SUBESI_ADI<-NULL 
LeadListEncoded$BHCS_DURUM<-NULL 
LeadListEncoded$SHRT_NM<-NULL 
LeadListEncoded$FULL_NM<-NULL 
LeadListEncoded$CITY_CODE<-NULL 
LeadListEncoded$MUS_CEP_TEL<-NULL 
LeadListEncoded$MUS_IS_TEL<-NULL 
LeadListEncoded$PORTFOLIOCODE<-NULL 
LeadListEncoded$MUS_EMAIL<-NULL 
LeadListEncoded$MERNIS_ADRES_IL<-NULL 
LeadListEncoded$MERNIS_ADRES_ILCE<-NULL 
LeadListEncoded$EMAIL_DURUM<-NULL 
LeadListEncoded$GSM_TEL_DURUM<-NULL 
LeadListEncoded$IS_TEL_DURUM<-NULL 
LeadListEncoded$ONONAY_LAST_ALT_URUN_TIPI<-NULL 
LeadListEncoded$LAST_ONONAY_LIMITI<-NULL 
LeadListEncoded$LAST_ONONAY_TARIHI<-NULL 
LeadListEncoded$LAST_ONNY_LIMTGCRLLKTRH<-NULL 
LeadListEncoded$LAST_ARAMA_KANAL<-NULL 
LeadListEncoded$LAST_ARAMA_TARIHI<-NULL 
LeadListEncoded$LAST_ARAMA_RESULT<-NULL 
LeadListEncoded$LAST_IS_SUCCESSFUL<-NULL 
LeadListEncoded$LAST_PPINTMNT<-NULL 
LeadListEncoded$LAST_CAMPAIGN_NAME<-NULL 
LeadListEncoded$LAST_KKB_EGILIM_SKORU<-NULL 


#This columns sadece lead listte 
LeadListEncoded$TOPTKPKRDSAYKLT8<-NULL
LeadListEncoded$TOPTKPKRDTKPBAKKLT8<-NULL
LeadListEncoded$TUMKRD1DNMGCKLIBKY<-NULL
LeadListEncoded$TUMKRD2DNMGCKLIBKY<-NULL
LeadListEncoded$TUMKRD2DNMGCKLIKRDSAY<-NULL
LeadListEncoded$TUMKRD3DNMGCKLIBKY<-NULL
LeadListEncoded$TUMKRD3DNMGCKLIKRDSAY<-NULL
LeadListEncoded$TUMKRD4VEFZLDNMGCKLIBKY<-NULL
LeadListEncoded$TUMKRD4VEFZLDNMGCKLIKRDSAY<-NULL
LeadListEncoded$TUMKRDKLT8STATULUBKY<-NULL


#unnecessay columns 
LeadListEncoded$IZIN_CM<-NULL
LeadListEncoded$IZIN_POSTA<-NULL
LeadListEncoded$IZIN_SMS<-NULL
LeadListEncoded$CIHAZ_TAKSIT_SAYISI<-NULL
LeadListEncoded$INTERNET_MOBIL_KULLANIMI<-NULL


#Predictiona'a giren datalar 
str(data_test_NoFlag, list.len=ncol(data_test_NoFlag)) # encode edilmiş data -- 377

str(LeadListEncoded, list.len=ncol(LeadListEncoded)) # encode edilmiş data --385


#########################PREDICT 
LeadList_sprint5<-predict(model_sprint5, newdata = as.matrix(LeadListEncoded)) 

LeadList_sprint5<-as.data.frame(LeadList_sprint5)

# 
# #add customer bilgi to propensity
# BusinessResultData<-cbind(LeadListOriginal$MUSTERI_NO,
#                            LeadListOriginal$TC_KIMLIK_NO,
#                            LeadListOriginal$MUSTERI_MESLEK,
#                            LeadListOriginal$MUSTERI_SUBESI_KODU, 
#                            LeadListOriginal$MUSTERI_SUBESI_ADI,
#                            LeadListOriginal$BHCS_DURUM,
#                            LeadListOriginal$SHRT_NM,
#                            LeadListOriginal$FULL_NM,
#                            LeadListOriginal$CITY_CODE,
#                            LeadListOriginal$MUS_CEP_TEL, 
#                            LeadListOriginal$MUS_IS_TEL,
#                            LeadListOriginal$PORTFOLIOCODE,
#                            LeadListOriginal$MUS_EMAIL,
#                            LeadListOriginal$MERNIS_ADRES_IL,
#                            LeadListOriginal$MERNIS_ADRES_ILCE,
#                            LeadListOriginal$EMAIL_DURUM,
#                            LeadListOriginal$GSM_TEL_DURUM,
#                            LeadListOriginal$IS_TEL_DURUM,
#                            LeadListOriginal$ONONAY_LAST_ALT_URUN_TIPI,
#                            LeadListOriginal$LAST_ONONAY_LIMITI,
#                            LeadListOriginal$LAST_ONONAY_TARIHI,
#                            LeadListOriginal$LAST_ONNY_LIMTGCRLLKTRH,
#                            LeadListOriginal$LAST_ARAMA_KANAL,
#                            LeadListOriginal$LAST_ARAMA_TARIHI,
#                            LeadListOriginal$LAST_ARAMA_RESULT,
#                            LeadListOriginal$LAST_IS_SUCCESSFUL,
#                            LeadListOriginal$LAST_PPINTMNT,
#                            LeadListOriginal$LAST_CAMPAIGN_NAME,
#                            LeadListOriginal$LAST_KKB_EGILIM_SKORU,
#                            LeadListOriginal$MUSTERI_NO,
#                            LeadListOriginal$VERIKAYNAK_STATU_ID, 
#                            LeadListOriginal$IZIN_POSTA,
#                            LeadListOriginal$IZIN_SMS, 
#                            LeadListOriginal$IZIN_CM,  
#                            LeadListOriginal$INTERNET_MOBIL_KULLANIMI,  
#                            LeadListOriginal$MUSTERI_YASI,
#                            LeadListOriginal$MUSTERI_CALISMA_DURUMU,
#                            LeadListOriginal$MUSTERI_SEGMENT,
#                            LeadListOriginal$MUSTERI_CINSIYET,  
#                            LeadListOriginal$EGITIM_DURUMU,
#                            LeadListOriginal$MEDENI_DURUM,
#                            LeadListOriginal$MUSTERILILIK_YASI,
#                            LeadListOriginal$KREDI_STATU_ID,  
#                            LeadListOriginal$normalized_CHZ_KRD_AMT , # normalized edilmiştir. Aylık ortalama kredi tutarına bölündü.
#                            LeadListOriginal$CIHAZ_TAKSIT_SAYISI,
#                            LeadListOriginal$KMH_LIMIT_AMOUNT_TL,
#                            LeadListOriginal$KMH_AVG_TOTAL_RISK
#                            ) 


                      
Model_Data_With_PROPENSITY<-cbind(LeadListOriginal, LeadList_sprint5)



#WRITE EXCEL 
install.packages("xlsx")
library("xlsx")
require(xlsx, quietly = TRUE)

write.csv(Model_Data_With_PROPENSITY, "SPRINT5_Model_Data_With_PROPENSITYV2.csv")

#write.table(Model_Data_With_PROPENSITY, file = "Model_Data_With_PROPENSITY2.txt", sep = "\t",row.names = TRUE, col.names = NA)




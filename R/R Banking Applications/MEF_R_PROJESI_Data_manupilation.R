#Data Cleaning & Manipulation
#R_Coders
#December 20, 2018
install.packages("readxl")
install.packages("plyr")
install.packages("tidyverse", repos = "https://cran.r-project.org")
install.packages("stringr")


library(tidyverse)
library(plyr)
library("readxl")
library(stringr)
Download Raw Data
Download export and import raw excel files and put them in temp files and read them.
And remove the temp files after reading.

Export Data

# Create a temporary file
tmp=tempfile(fileext=".xls")
# Download file from repository to the temp file
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Excel)/export_1996_2018.xls?raw=true",destfile=tmp,mode='wb')
# Read that excel file.
export_data <- read_excel(tmp)
# Remove the temp file
file.remove(tmp)
Import Data

# Create a temporary file
tmp=tempfile(fileext=".xls")
# Download file from repository to the temp file
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Excel)/import_1996_2018.xls?raw=true",destfile=tmp,mode='wb')
# Read that excel file.
import_data <- read_excel(tmp)
# Remove the temp file
file.remove(tmp)
Download the files from local computer. For example:

setwd("C:\\Users\\ozenm\\Documents\\GitHub\\gpj18-r_coders\Data_Sources\Excel_Files")
export_data <- read_excel("export_1996_2018.xls")
import_data <- read_excel("import_1996_2018.xls")
Download the rds files from our github repository.

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Rds)/Producer_Inflation.rds?raw=true?raw=true",destfile=tmp,mode = 'wb')
producer_inf_data<-read_rds(tmp)
file.remove(tmp)

githubURL_data <- ("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Rds)/Consumer_Inflation.rds?raw=true")
download.file(githubURL_data,"Consumer_Inflation.rds", method="curl")
cons_inf_data<-readRDS("Consumer_Inflation.rds")


tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/import_data/Consumer_Inflation.rds?raw=true",destfile=tmp,mode = 'wb')
cons_inf_data<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/import_data/Enflasyon_Data.rds?raw=true",destfile=tmp,mode = 'wb')
enflasyon_data<-read_rds(tmp)
file.remove(tmp)
Format Data

#Change the column names of data

colnames(export_data) <- c("Year","Sector_Type_Code","Sector_Name","Total_Amount","January","February","March","April","May","June","July","August","September","October","November","December")
colnames(import_data) <- c("Year","Sector_Type_Code","Sector_Name","Total_Amount","January","February","March","April","May","June","July","August","September","October","November","December")
Remove the NA rows

export_data_without_NA <- export_data[rowSums(is.na(export_data)) != ncol(export_data),]
import_data_without_NA <- import_data[rowSums(is.na(import_data)) != ncol(import_data),]
Analyze Data

class(export_data_without_NA)
dim(export_data_without_NA)
summary(export_data_without_NA)

#Total_Amount is not numeric!!! Check some values
hist(export_data_without_NA$Total_Amount)
Remove the total amount columns

#Drop Total Amount Data
export_data_without_NA_Total <- within(export_data_without_NA, rm(Total_Amount))
#Drop Total Amount Data
import_data_without_NA_Total <- within(import_data_without_NA, rm(Total_Amount))
Convert char data type to numeric data type for January column

str(export_data_without_NA_Total)
x <-suppressWarnings(as.numeric(export_data_without_NA_Total$January))
Control data type for January column

str(x)
sapply(export_data_without_NA_Total, is.numeric) 
hist(as.numeric(export_data_without_NA_Total$January))
Convert char data types to numeric data types for other columns

cols = c(4:15);    
export_data_without_NA_Total[,cols] = suppressWarnings(apply(export_data_without_NA_Total[,cols], 2, function(x) as.numeric(as.character(x))));
import_data_without_NA_Total[,cols] = suppressWarnings(apply(import_data_without_NA_Total[,cols], 2, function(x) as.numeric(as.character(x))));
Control data type for all columns

str(export_data_without_NA_Total)
str(import_data_without_NA_Total)
Do some tests on data

export_data_without_NA_Total$Year <- str_trim(export_data_without_NA_Total$Year)
head(export_data_without_NA_Total)

#outlier detection. -- Not use it.
out_export_data <- boxplot.stats(export_data_without_NA_Total$January)$out;
out_export_data
Check for the total number of missing values in the entire data

sum(is.na(export_data_without_NA_Total)) #1586
sum(is.na(import_data_without_NA_Total)) #1586
Shorter data name

exp_data <- export_data_without_NA_Total
str(exp_data)
imp_data <- import_data_without_NA_Total
str(imp_data)
Fill the empty years

v_year <- 2017
for (row in 1:nrow(exp_data)) {
  year <- exp_data[row, "Year"]
  if(!is.na(year) & year == v_year){
    #print(paste("YEAR-ROW",year))
    #print(paste("row-i",row))
    v_year <- v_year - 1
  }
  exp_data[row, "Year"] <- v_year + 1
  if (v_year==2008){
    break
  }
}

v_year <- 2017
for (row in 1:nrow(imp_data)) {
  year <- imp_data[row, "Year"]
  if(!is.na(year) & year == v_year){
    #print(paste("YEAR-ROW",year))
    #print(paste("row-i",row))
    v_year <- v_year - 1
  }
  imp_data[row, "Year"] <- v_year + 1
  if (v_year==2008){
    break
  }
}
Remove the unnecessary rows

exp_data_v2 <- exp_data %>%
  slice(6:391)%>% filter(Sector_Name != "Toplam -Total")

imp_data_v2 <- imp_data %>%
  slice(6:391)%>% filter(Sector_Name != "Toplam -Total")
Show data

tail(exp_data_v2)
tail(imp_data_v2)
exp_data_v3 <-
  exp_data_v2 %>%
  gather(key=Month,value=Amount,-Year,-Sector_Type_Code,-Sector_Name)

imp_data_v3 <-
  imp_data_v2 %>%
  gather(key=Month,value=Amount,-Year,-Sector_Type_Code,-Sector_Name)
Replace the month names with the month numbers

exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="January","01")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="February","02")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="March","03")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="April","04")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="May","05")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="June","06")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="July","07")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="August","08")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="September","09")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="October","10")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="November","11")
exp_data_v3$Month <- replace(exp_data_v3$Month,exp_data_v3$Month=="December","12")


imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="January","01")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="February","02")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="March","03")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="April","04")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="May","05")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="June","06")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="July","07")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="August","08")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="September","09")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="October","10")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="November","11")
imp_data_v3$Month <- replace(imp_data_v3$Month,imp_data_v3$Month=="December","12")
Convert about date columns as lubridate

exp_data_v4 <- exp_data_v3 %>% mutate(Date = lubridate::as_date(paste(Year, 
                                                                      as.integer(Month), as.integer("01"), sep = "-")))

imp_data_v4 <- imp_data_v3 %>% mutate(Date = lubridate::as_date(paste(Year, 
                                                                      as.integer(Month), as.integer("01"), sep = "-")))
Remove unnecessary columns

exp_data_v5 <- exp_data_v4 %>% select ( Date, Sector_Type_Code, Sector_Name, Amount)
imp_data_v5 <- imp_data_v4 %>% select ( Date, Sector_Type_Code, Sector_Name, Amount)
Show data structure

str(exp_data_v5)
str(imp_data_v5)
Save as rds files

saveRDS(exp_data_v5,file="exp_data.rds")
#export_data_rds <- readRDS("exp_data.rds")
saveRDS(imp_data_v5,file="imp_data.rds")
#import_data_rds <- readRDS("imp_data.rds")
Download new rds files

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Rds)/imp_data.rds?raw=true",destfile=tmp,mode = 'wb')
imp_data<-read_rds(tmp)
file.remove(tmp)

tmp<-tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-r_coders/blob/master/Data_Sources(Rds)/exp_data.rds?raw=true",destfile=tmp,mode = 'wb')
exp_data<-read_rds(tmp)
file.remove(tmp)
Data transformation

exp_data_without_NA <- exp_data
exp_data_without_NA[is.na(exp_data_without_NA <- exp_data)] <- 0
exp_data_without_NA


imp_data_without_NA <- imp_data
imp_data_without_NA[is.na(imp_data_without_NA <- imp_data)] <- 0
imp_data_without_NA
Convert date column POSIXct to Date and check data types

str(us_rate_data)
us_rate_data$Date <- as.Date(as.character(as.POSIXct(us_rate_data$Date)))
str(us_rate_data)

str(cons_inf_data)
cons_inf_data$Date <- as.Date(as.character(as.POSIXct(cons_inf_data$Date)))
str(cons_inf_data)

str(cons_inf_data)
producer_inf_data$Date <- as.Date(as.character(as.POSIXct(producer_inf_data$Date)))
str(cons_inf_data)
Join data

install.packages("tidyverse")
library("tidyverse")
exp_data_v2 <-
exp_data_without_NA %>%
  group_by(Date) %>%
  summarise(Export_Total_Amount=sum(Amount))

imp_data_v2 <-
  imp_data_without_NA %>%
  group_by(Date) %>%
  summarise(Import_Total_Amount=sum(Amount))

exp_data_v3 <- inner_join(exp_data_v2,cons_inf_data, by=c("Date" = "Date"))

exp_data_v4 <- inner_join(exp_data_v3,us_rate_data, by=c("Date" = "Date"))

imp_data_v3 <- inner_join(imp_data_v2,cons_inf_data, by=c("Date" = "Date"))

imp_data_v4 <- inner_join(imp_data_v3,us_rate_data, by=c("Date" = "Date"))
Change column names

colnames(exp_data_v4) <- c("Date","Export_Total_Amount","Consumer_Price_Index_Yearly_Change","Consumer_Price_Index_Monthly_Change","USD_Rate")

colnames(imp_data_v4) <- c("Date","Export_Total_Amount","Consumer_Price_Index_Yearly_Change","Consumer_Price_Index_Monthly_Change","USD_Rate")
Save as RDS files

saveRDS(imp_data_v4,file='imp_data_final.rds')
saveRDS(exp_data_v4,file='exp_data_final.rds')
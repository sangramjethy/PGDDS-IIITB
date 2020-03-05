##------------BFS Capstone Project Assignment ---------------------##
## Group Members
## Raju Kumar (Group Facilitator)
## Sangram Jethy
## Manish Mani
## Sidharth Mahapatra


# Problem Statement:
#-------------------------------------------------------------------------------------------------------------------	
# CredX is a leading credit card provider that gets thousands of credit card applicants every year. 
# But in the past few years, it has experienced an increase in credit loss.
# 
# Business Objective
# ---------------------------------------------------------------------------------------------------------------------	
# The CEO believes that the best strategy to mitigate credit risk is to 'acquire the right customers'.
#----------------------------------------------------------
# The standard process followed in analytic projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#--------------------------------Install  required libraries---------------------------------#
# Install the libraries if not available
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("Information")
# install.packages("fuzzyjoin")
#-------------------------------- Load the required libraries---------------------------------#
# Load the libraries 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(Information)
library(fuzzyjoin)
#----------------------------------------------------------
# The standard process followed in analytic projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#-------------------------------------------------------
## Business Understanding:
#-------------------------------------------------------
#Business Objective : During Customer acquisition process for credit cards, help CredX identify the right customers. 
#Determine the factors affecting credit risk, create strategies to mitigate the acquisition risk 
#and assess financial benefit

#-------------------------------------------------------

#####################################################################################################################
########################### DATA UNDERSTANDING ######################################################################
#####################################################################################################################

# ---------------------------------------------------------------------------------------------------------------------	
#   Demographic/application data: This is obtained from the information provided by the applicants 
#   at the time of credit card application. It contains customer-level information on age, gender, income, marital status, etc.
# 
#   Credit bureau: This is taken from the credit bureau and contains variables such as
#  'number of times 30 DPD or worse in last 3/6/12 months', 'outstanding balance', 'number of trades', etc.
# 
#   Both files contain a performance tag which represents whether the applicant has gone 90 days past due
#   or worse in the past 12-months (i.e. defaulted) after getting a credit card.


## Data Understanding, Cleaning and Preparation
#-------------------------------------------------------

##Load Demographic data
demographic_data<- read.csv("Demographic data.csv")

##Load Credit Bureau Data
credit_bureau_data<- read.csv("Credit Bureau data.csv")

# Checking structure of dataset 

str(demographic_data)
# 71295 obs. of  12 variable

str(credit_bureau_data)
# 71295 obs. of  19 variable

# Summary of dataset

summary(demographic_data)
# Application.ID : Min 1.004e+05 :: Max 1.000e+09
# Age : Min : -3.00 :: Max : 65.00 :: Mean   :44.94 :: Median :45.00
# Gender : F:16837 :: M:54456
# Marital.Status : Married:60730 :: Single :10559
# No.of.dependents : Min.   :1.000 :: Max.   :5.000 :: Mean   :2.865 :: Median :3.000
# Income : Min.   :-0.5 :: Max.   :60.0 :: Mean   :27.2 :: Median :27.0
# Education : Blanks : 119 :: Bachelor :17697 :: Masters :23970 :: Phd : 4549 :: Professional:24839 :: Others :  121
# Profession : Blanks : 14 :: SAL :40439 :: SE :14307 :: SE_PROF:16535
# Type.of.residence : Blanks : 8 :: Company provided : 163 :: Living with Parents: 1818 :: Others : 199 :: Owned :14243 :: Rented :53397 
# No.of.months.in.current.residence: Min. :  3.00 :: Max. :133.00 :: Mean   : 33.96 :: Median : 34.00
# No.of.months.in.current.company: Min. :  3.00 :: Max. :133.00 :: Mean   : 33.96 :: Median : 34.00
# Performance.Tag :  Min. :0.0000 :: Max.   :1.0000 :: Mean   :0.0422 :: Median :0.000 :: NA's   :1425

summary(credit_bureau_data)
# Application.ID : Min.   :1.004e+05 :: Max.   :1.000e+09
# No.of.times.90.DPD.or.worse.in.last.6.months : Min.   :0.0000 :: Max.   :3.0000 :: Mean   :0.2703 :: Median :0.0000
# No.of.times.60.DPD.or.worse.in.last.6.months : Min.   :0.0000 :: Max.   :5.0000 :: Mean   :0.4305 :: Median :0.0000 
# No.of.times.30.DPD.or.worse.in.last.6.month : Min.   :0.0000 :: Max.   :7.0000 :: Mean   :0.5772 :: Median :0.0000
# No.of.times.90.DPD.or.worse.in.last.12.month : Min.   :0.0000 :: Max.   :5.0000 :: Mean   :0.4503 :: Median :0.0000
# No.of.times.60.DPD.or.worse.in.last.12.months : Min.   :0.0000 :: Max.   :7.0000 ::  Mean   :0.6555 :: Median :0.0000
# No.of.times.30.DPD.or.worse.in.last.12.months : Min.   :0.0000 ::  Max.   :9.0000 :: Mean   :0.8009 :: Median :0.0000
# Avgas.CC.Utilization.in.last.12.months : Min.   :  0.0 :: Max.   :113.0 :: Mean   : 29.7 :: Median : 15.0 :: NA's   :1058
# No.of.trades.opened.in.last.6.months : Min.   : 0.000 :: Max.   :12.000 :: Mean   : 2.298 :: Median : 2.000 :: NA's   :1
# No.of.trades.opened.in.last.12.months : Min.   : 0.000 :: Max.   :28.000 :: Mean   : 5.827 :: Median : 5.000 
# No.of.PL.trades.opened.in.last.6.months : Min.   :0.000 :: Max.   :6.000 :: Mean   :1.207 :: Median :1.000
# No.of.PL.trades.opened.in.last.12.months : Min.   : 0.000 :: Max.   :12.000 :: Mean   : 2.397 :: Median : 2.000
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans : Min.   : 0.000 ::  Max.   :10.000 :: Mean   : 1.764 :: Median : 1.000
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans :  Min.   : 0.000 ::  Max.   :20.000 :: Mean   : 3.535 :: Median : 3.000
# Presence.of.open.home.loan : Min.   :0.0000 :: Max.   :1.0000 :: Mean   :0.2564 :: Median :0.0000 :: NA's   :272
# Outstanding.Balance : Min.   :      0 :: Max.   :5218801 :: Mean   :1249163 :: Median : 774992 :: NA's   :272
# Total.No.of.Trades : Min.   : 0.000 :: Max.   :44.00 :: Mean   : 8.187 :: Median : 6.000
# Presence.of.open.auto.loan : Min.   :0.00000 :: Max.   :1.00000 :: Mean   :0.08462 :: Median :0.00000 
# Performance.Tag : Min.   :0.0000 :: Max.   :1.0000 :: Mean   :0.0422 :: Median :0.0000 :: NA's   :1425

#Check for primary key for demographic_data to merge files together as single file
length(unique(demographic_data$Application.ID))
# 71292

#Check for duplicate values for Application.ID variable
sum(duplicated(demographic_data$Application.ID))
# 3 duplicate Application.ID exists

demographic_data[duplicated(demographic_data$Application.ID),]
# Duplicate Application.ID : 765011468 653287861 671989187

#Remove Duplicate Records 
demographic_data <- demographic_data[!duplicated(demographic_data$Application.ID),]
nrow(demographic_data)
# 71292
length(unique(demographic_data$Application.ID))
# 71292

#Check for primary key for credit_bureau_data to merge files together as single file
length(unique(credit_bureau_data$Application.ID))    
# 71292

#Check for duplicate values for Application.ID variable
sum(duplicated(credit_bureau_data$Application.ID))
# 3 duplicate Application.ID exists

credit_bureau_data[duplicated(credit_bureau_data$Application.ID),]
# Duplicate Application.ID : 765011468 653287861 671989187

#Remove Duplicate Records 
credit_bureau_data <- credit_bureau_data[!duplicated(credit_bureau_data$Application.ID),]
nrow(credit_bureau_data)
# 71292
length(unique(credit_bureau_data$Application.ID))
# 71292

#Check if their is any Application.ID which value differs in two data sets
setdiff(demographic_data$Application.ID,credit_bureau_data$Application.ID)  
# 0

#Lets create a master data set will allthe variables belonging to both demographic and credit bureau dataset
master_credit_applicant_data <- merge(demographic_data,credit_bureau_data,by="Application.ID")
str(master_credit_applicant_data)
#71292 obs. of  37 variables

#Check for missing values in master_credit_applicant_data dataset
sum(is.na(master_credit_applicant_data))
# 4456 NA values

# Calculate Column wise NA values
na_count_master_credit_applicant_data <- cbind(as.data.frame(colnames(master_credit_applicant_data)), as.data.frame(colSums(is.na(master_credit_applicant_data))))
na_count_master_credit_applicant_data[na_count_master_credit_applicant_data$`colSums(is.na(master_credit_applicant_data))` > 0,]
# No.of.dependents                                                                  3
# Performance.Tag.x                                                              1425
# Avgas.CC.Utilization.in.last.12.months                                         1058
# No.of.trades.opened.in.last.6.months                                              1
# Presence.of.open.home.loan                                                      272
# Outstanding.Balance                                                             272
# Performance.Tag.y                                                              1425

# Following columns has blanks values as per earlier analysis of data in csv file
# Gender - 2 blank values
# Marital Status (at the time of application) - 6 blank values
# Education - 119 blank values
# Profession - 14 blank values
# Type of residence - 8 blank values

#####################################################################################################################
########################### DATA CLEANING ##########################################################################
#####################################################################################################################

#############################  INVALID DATA TREATMENT ###############################################################
#Treat invalid values. 
#We will treat missing values alone using WOE

# Check how many age are invalid age
master_credit_applicant_data[master_credit_applicant_data$Age <= 0,]$Age

#Replace those age with median value of age
master_credit_applicant_data[master_credit_applicant_data$Age <= 0,]$Age <- 45

#Check how many income are invalid Income
master_credit_applicant_data[master_credit_applicant_data$Income < 0,]$Income

#Replace those age with median value of income
master_credit_applicant_data[master_credit_applicant_data$Income < 0,]$Income <- 27

#############################  OUTLIER DATA TREATMENT #########################################################################

#Outlier detection for numeric variable Age
quantile(master_credit_applicant_data$Age,seq(0,1,0.01))

boxplot(master_credit_applicant_data$Age)
#No Outlier is present

#Outlier detection for numeric variable Income
quantile(master_credit_applicant_data$Income,seq(0,1,0.01))

boxplot(master_credit_applicant_data$Income)
#No Outlier is present

#Outlier detection for numeric variable No.of.months.in.current.residence
quantile(master_credit_applicant_data$No.of.months.in.current.residence,seq(0,1,0.01))

boxplot(master_credit_applicant_data$No.of.months.in.current.residence)
#No Outlier is present

#Outlier detection for numeric variable No.of.months.in.current.company
quantile(master_credit_applicant_data$No.of.months.in.current.company,seq(0,1,0.01))

boxplot(master_credit_applicant_data$No.of.months.in.current.company)
#Outlier is present

# So, capping the No.of.months.in.current.company  at 99% which is 74.0 
master_credit_applicant_data[(which(master_credit_applicant_data$No.of.months.in.current.company>74.0)),]$No.of.months.in.current.company <- 74.0


#Outlier detection for numeric variable Avgas.CC.Utilization.in.last.12.months
quantile(master_credit_applicant_data$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.01),na.rm = T)
boxplot(master_credit_applicant_data$Avgas.CC.Utilization.in.last.12.months)
#Outlier is present
# So, capping the Avgas.CC.Utilization.in.last.12.months  to 2nd highest value which is 103.0 
master_credit_applicant_data[(which(master_credit_applicant_data$Avgas.CC.Utilization.in.last.12.months>103)),]$Avgas.CC.Utilization.in.last.12.months <- 103.0

#Outlier detection for numeric variable Outstanding.Balance
quantile(master_credit_applicant_data$Outstanding.Balance,seq(0,1,0.01),na.rm = T)
boxplot(master_credit_applicant_data$Outstanding.Balance)
#No Outlier is present

#Outlier detection for numeric variable Total.No.of.Trades
quantile(master_credit_applicant_data$Total.No.of.Trades,seq(0,1,0.01))
boxplot(master_credit_applicant_data$Total.No.of.Trades)
# Outlier is present

#Function to find upper outlier values
findUpperOutlier <- function(x.var, df){with(df, quantile(x.var, 0.75) + (1.5 * (quantile(x.var, 0.75) - quantile(x.var, 0.25))))}

#Function to find lower outlier values
findLowerOutlier <- function(x.var, df){with(df, quantile(x.var, 0.25) - (1.5 * (quantile(x.var, 0.75) - quantile(x.var, 0.25))))}


#Upper outliers present. Treat it.
findUpperOutlier(master_credit_applicant_data$Total.No.of.Trades,master_credit_applicant_data)[[1]]
#20.5

# So, capping the Total.No.of.Trades  at upper outlier limit 20 
master_credit_applicant_data[(which(master_credit_applicant_data$Total.No.of.Trades>20 )),]$Total.No.of.Trades <- 20


######################################  BINNING ##########################################################################

#Lets check the unique values for few master_credit_applicant_data fields 
# And covert to factor if number of unique values are less otherwise we need to bin the values before 
# changing to factor

unique(master_credit_applicant_data$No.of.dependents)
# 4    1    3    2    5    <NA>
#Convert No.of.dependents  to factor as it has discrete values.
master_credit_applicant_data$No.of.dependents <- as.factor(master_credit_applicant_data$No.of.dependents)
levels(master_credit_applicant_data$No.of.dependents)
levels(master_credit_applicant_data$No.of.dependents)[1:2] <- "NumOfDependent_LTE2"
levels(master_credit_applicant_data$No.of.dependents)[2:3] <- "NumOfDependent_EQ_3_4"
levels(master_credit_applicant_data$No.of.dependents)[3:4]<- "NumOfDependent_GT4"

unique(master_credit_applicant_data$No.of.times.90.DPD.or.worse.in.last.6.months)
#0 1 2 3
master_credit_applicant_data$No.of.times.90.DPD.or.worse.in.last.6.months <- as.factor(master_credit_applicant_data$No.of.times.90.DPD.or.worse.in.last.6.months)

unique(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.6.months)
#0 1 2 3 4 5
master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.6.months <- as.factor(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.6.months)
levels(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.6.months)
levels(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.6.months)[1:2] <- "60DPD6Mon_LT2"
levels(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.6.months)[2:3] <- "60DPD6Mon_BTW2_3"
levels(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.6.months)[3:4]<- "60DPD6Mon_GT3"

unique(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.6.months)
#0 1 2 3 4 5 6 7
master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.6.months <- as.factor(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.6.months)
levels(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.6.months)
levels(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.6.months)[1:2] <- "30DPD6Mon_LT2"
levels(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.6.months)[2:3] <- "30DPD6Mon_BTW2_3"
levels(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.6.months)[3:4]<- "30DPD6Mon_BTW4_5"
levels(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.6.months)[4:5]<- "30DPD6Mon_GT5"

unique(master_credit_applicant_data$No.of.times.90.DPD.or.worse.in.last.12.months)
#0 1 2 3 4 5
master_credit_applicant_data$No.of.times.90.DPD.or.worse.in.last.12.months <- as.factor(master_credit_applicant_data$No.of.times.90.DPD.or.worse.in.last.12.months)
levels(master_credit_applicant_data$No.of.times.90.DPD.or.worse.in.last.12.months)
levels(master_credit_applicant_data$No.of.times.90.DPD.or.worse.in.last.12.months)[1:2] <- "90DPD12Mon_LT2"
levels(master_credit_applicant_data$No.of.times.90.DPD.or.worse.in.last.12.months)[2:3] <- "90DPD12Mon_BTW2_3"
levels(master_credit_applicant_data$No.of.times.90.DPD.or.worse.in.last.12.months)[3:4]<- "90DPD12Mon_GT3"

unique(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.12.months)
#  0 1 2 3 4 5 6 7
master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.12.months <- as.factor(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.12.months)
levels(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.12.months)
levels(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.12.months)[1:2] <- "60DPD12Mon_LT2"
levels(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.12.months)[2:3] <- "60DPD12Mon_BTW2_3"
levels(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.12.months)[3:4]<- "60DPD12Mon_BTW4_5"
levels(master_credit_applicant_data$No.of.times.60.DPD.or.worse.in.last.12.months)[4:5]<- "60DPD12Mon_GT5"

unique(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.12.months)
# 0 1 2 3 4 5 6 7 8 9
master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.12.months <- as.factor(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.12.months)
levels(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.12.months)
levels(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.12.months)[c(1:3)] <- "30DPD12Mon_LTEQ3"
levels(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.12.months)[2:4] <- "30DPD12Mon_BTW3_5"
levels(master_credit_applicant_data$No.of.times.30.DPD.or.worse.in.last.12.months)[3:6]<- "30DPD12Mon_GTEQ6"

unique(master_credit_applicant_data$No.of.trades.opened.in.last.6.months)
# 1  0  2  3  4  5  9  6  7  8 12 10 11 NA
master_credit_applicant_data$No.of.trades.opened.in.last.6.months <- as.factor(master_credit_applicant_data$No.of.trades.opened.in.last.6.months)
levels(master_credit_applicant_data$No.of.trades.opened.in.last.6.months)
levels(master_credit_applicant_data$No.of.trades.opened.in.last.6.months)[1:3] <- "NumTradesOpen6Mon_LT3"
levels(master_credit_applicant_data$No.of.trades.opened.in.last.6.months)[2:4] <- "NumTradesOpen6Mon_BTW3_5"
levels(master_credit_applicant_data$No.of.trades.opened.in.last.6.months)[3:5]<- "NumTradesOpen6Mon_BTW6_8"
levels(master_credit_applicant_data$No.of.trades.opened.in.last.6.months)[4:7]<- "NumTradesOpen6Mon_GT8"

unique(master_credit_applicant_data$No.of.trades.opened.in.last.12.months)
# 2  0  1  3  4  5  6  7 17  8 19 16 15 20 11 18 12 13  9 10 14 22 21 25 24 23 27 26 28
master_credit_applicant_data$No.of.trades.opened.in.last.12.months <- as.factor(master_credit_applicant_data$No.of.trades.opened.in.last.12.months)
levels(master_credit_applicant_data$No.of.trades.opened.in.last.12.months)
levels(master_credit_applicant_data$No.of.trades.opened.in.last.12.months)[1:5] <- "NumTradesOpen12Mon_LT5"
levels(master_credit_applicant_data$No.of.trades.opened.in.last.12.months)[2:5] <- "NumTradesOpen12Mon_BTW5_8"
levels(master_credit_applicant_data$No.of.trades.opened.in.last.12.months)[3:6]<- "NumTradesOpen12Mon_BTW9_12"
levels(master_credit_applicant_data$No.of.trades.opened.in.last.12.months)[4:7]<- "NumTradesOpen12Mon_BTW13_16"
levels(master_credit_applicant_data$No.of.trades.opened.in.last.12.months)[5:8] <- "NumTradesOpen12Mon_BTW17_20"
levels(master_credit_applicant_data$No.of.trades.opened.in.last.12.months)[6:9]<- "NumTradesOpen12Mon_BTW21_24"
levels(master_credit_applicant_data$No.of.trades.opened.in.last.12.months)[7:10]<- "NumTradesOpen12Mon_BTW25_28"

unique(master_credit_applicant_data$No.of.PL.trades.opened.in.last.6.months)
# 0 1 2 5 4 3 6
master_credit_applicant_data$No.of.PL.trades.opened.in.last.6.months <- as.factor(master_credit_applicant_data$No.of.PL.trades.opened.in.last.6.months)
levels(master_credit_applicant_data$No.of.PL.trades.opened.in.last.6.months)
levels(master_credit_applicant_data$No.of.PL.trades.opened.in.last.6.months)[1:2] <- "NumPLTradesOpen6Mon_LT2"
levels(master_credit_applicant_data$No.of.PL.trades.opened.in.last.6.months)[2:4] <- "NumPLTradesOpen6Mon_BTW2_4"
levels(master_credit_applicant_data$No.of.PL.trades.opened.in.last.6.months)[3:4]<- "NumPLTradesOpen6Mon_GT4"

unique(master_credit_applicant_data$No.of.PL.trades.opened.in.last.12.months)
# 0  1  2  6  7  8  5  4  3  9 11 10 12
master_credit_applicant_data$No.of.PL.trades.opened.in.last.12.months <- as.factor(master_credit_applicant_data$No.of.PL.trades.opened.in.last.12.months)
levels(master_credit_applicant_data$No.of.PL.trades.opened.in.last.12.months)
levels(master_credit_applicant_data$No.of.PL.trades.opened.in.last.12.months)[1:3] <- "NumPLTradesOpen12Mon_LT3"
levels(master_credit_applicant_data$No.of.PL.trades.opened.in.last.12.months)[2:4] <- "NumPLTradesOpen12Mon_BTW3_5"
levels(master_credit_applicant_data$No.of.PL.trades.opened.in.last.12.months)[3:5] <- "NumPLTradesOpen12Mon_BTW6_8"
levels(master_credit_applicant_data$No.of.PL.trades.opened.in.last.12.months)[4:7]<- "NumPLTradesOpen12Mon_GT8"

unique(master_credit_applicant_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
# 0  1  2  3  6  7  5  4  9  8 10
master_credit_applicant_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- as.factor(master_credit_applicant_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
levels(master_credit_applicant_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
levels(master_credit_applicant_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)[1:2] <- "NumOfEnq6MonEHAL_LT2"
levels(master_credit_applicant_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)[2:4] <- "NumOfEnq6MonEHAL_BTW2_4"
levels(master_credit_applicant_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)[3:5] <- "NumOfEnq6MonEHAL_BTW5_7"
levels(master_credit_applicant_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)[4:6]<- "NumOfEnq6MonEHAL_GT7"

unique(master_credit_applicant_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
# 0  1  2  3  4  5  6 11 16 12  9 10  8 13 14  7 17 15 19 18 20
master_credit_applicant_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- as.factor(master_credit_applicant_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
levels(master_credit_applicant_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
levels(master_credit_applicant_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[1:5] <- "NumOfEnq12MonEHAL_LT5"
levels(master_credit_applicant_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[2:5] <- "NumOfEnq12MonEHAL_BTW5_8"
levels(master_credit_applicant_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[3:6] <- "NumOfEnq12MonEHAL_BTW9_12"
levels(master_credit_applicant_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[4:7] <- "NumOfEnq12MonEHAL_BTW13_16"
levels(master_credit_applicant_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)[5:8] <- "NumOfEnq12MonEHAL_GT6"


unique(master_credit_applicant_data$Presence.of.open.home.loan)
# 1  0 NA
master_credit_applicant_data$Presence.of.open.home.loan <- as.factor(master_credit_applicant_data$Presence.of.open.home.loan)

unique(master_credit_applicant_data$Presence.of.open.auto.loan)
# 0 1
master_credit_applicant_data$Presence.of.open.auto.loan <- as.factor(master_credit_applicant_data$Presence.of.open.auto.loan)

#Bin the numeric variables and change it to factor

# Binning the age variable and store it into "binning.age".
summary(master_credit_applicant_data$Age)
master_credit_applicant_data$binning.Age <- as.factor(cut(master_credit_applicant_data$Age, breaks = c(14,25,35,45,55,66)))
levels(master_credit_applicant_data$binning.Age)
levels(master_credit_applicant_data$binning.Age)[1]<- "Age_(14,25]"
levels(master_credit_applicant_data$binning.Age)[2]<- "Age_(25,35]"
levels(master_credit_applicant_data$binning.Age)[3]<- "Age_(35,45]"
levels(master_credit_applicant_data$binning.Age)[4]<- "Age_(45,55]"
levels(master_credit_applicant_data$binning.Age)[5]<- "Age_(55,66]"

# Binning the Income variable and store it into "binning.Income".
summary(master_credit_applicant_data$Income)
master_credit_applicant_data$binning.Income <- as.factor(cut(master_credit_applicant_data$Income, breaks = c(-1,15,28,41,61)))
levels(master_credit_applicant_data$binning.Income)
levels(master_credit_applicant_data$binning.Income)[1]<- "Income_(0,15]"
levels(master_credit_applicant_data$binning.Income)[2]<- "Income_(15,28]"
levels(master_credit_applicant_data$binning.Income)[3]<- "Income_(28,41]"
levels(master_credit_applicant_data$binning.Income)[4]<- "Income_(41,61]"

# Binning the No.of.months.in.current.residence variable and store it into "binning.NumMonthsCurrResidence".
summary(master_credit_applicant_data$No.of.months.in.current.residence)
master_credit_applicant_data$binning.NumMonthsCurrResidence <- as.factor(cut(master_credit_applicant_data$No.of.months.in.current.residence, breaks = c(5,6,12,35,61,90,127)))
levels(master_credit_applicant_data$binning.NumMonthsCurrResidence)
levels(master_credit_applicant_data$binning.NumMonthsCurrResidence)[1]<- "NumMonthsCurrResidence_(5,6]"
levels(master_credit_applicant_data$binning.NumMonthsCurrResidence)[2]<- "NumMonthsCurrResidence_(6,12]"
levels(master_credit_applicant_data$binning.NumMonthsCurrResidence)[3]<- "NumMonthsCurrResidence_(12,35]"
levels(master_credit_applicant_data$binning.NumMonthsCurrResidence)[4]<- "NumMonthsCurrResidence_(35,61]"
levels(master_credit_applicant_data$binning.NumMonthsCurrResidence)[5]<- "NumMonthsCurrResidence_(61,90]"
levels(master_credit_applicant_data$binning.NumMonthsCurrResidence)[6]<- "NumMonthsCurrResidence_(90,127]"

# Binning the No.of.months.in.current.company variable and store it into "binning.NumMonthsCurrCompany".
summary(master_credit_applicant_data$No.of.months.in.current.company)
master_credit_applicant_data$binning.NumMonthsCurrCompany <- as.factor(cut(master_credit_applicant_data$No.of.months.in.current.company, breaks = c(2,17,35,52,75)))
levels(master_credit_applicant_data$binning.NumMonthsCurrCompany)
levels(master_credit_applicant_data$binning.NumMonthsCurrCompany)[1]<- "NumMonthsCurrCompany_(5,6]"
levels(master_credit_applicant_data$binning.NumMonthsCurrCompany)[2]<- "NumMonthsCurrCompany_(6,12]"
levels(master_credit_applicant_data$binning.NumMonthsCurrCompany)[3]<- "NumMonthsCurrCompany_(12,35]"
levels(master_credit_applicant_data$binning.NumMonthsCurrCompany)[4]<- "NumMonthsCurrCompany_(35,61]"
levels(master_credit_applicant_data$binning.NumMonthsCurrCompany)[5]<- "NumMonthsCurrCompany_(61,90]"

summary(master_credit_applicant_data$Avgas.CC.Utilization.in.last.12.months)
master_credit_applicant_data$binning.AvgCCUtil12Months <- as.factor(cut(master_credit_applicant_data$Avgas.CC.Utilization.in.last.12.months, breaks = c(-1,8,16,30,50,80,100,114)))
levels(master_credit_applicant_data$binning.AvgCCUtil12Months)
levels(master_credit_applicant_data$binning.AvgCCUtil12Months)[1]<- "AvgCCUtil12Months_(0,8]"
levels(master_credit_applicant_data$binning.AvgCCUtil12Months)[2]<- "AvgCCUtil12Months_(8,16]"
levels(master_credit_applicant_data$binning.AvgCCUtil12Months)[3]<- "AvgCCUtil12Months_(16,30]"
levels(master_credit_applicant_data$binning.AvgCCUtil12Months)[4]<- "AvgCCUtil12Months_(30,50]"
levels(master_credit_applicant_data$binning.AvgCCUtil12Months)[5]<- "AvgCCUtil12Months_(50,80]"
levels(master_credit_applicant_data$binning.AvgCCUtil12Months)[6]<- "AvgCCUtil12Months_(80,100]"
levels(master_credit_applicant_data$binning.AvgCCUtil12Months)[7]<- "AvgCCUtil12Months_(100,114]"

summary(master_credit_applicant_data$Outstanding.Balance)
master_credit_applicant_data$binning.OutstandingBalance <- as.factor(cut(master_credit_applicant_data$Outstanding.Balance, breaks = c(-1,10000,50000,250000,500000,1000000,1500000,2500000,3500000,4500000,5300000)))
levels(master_credit_applicant_data$binning.OutstandingBalance)
levels(master_credit_applicant_data$binning.OutstandingBalance)[1]<- "OutstandingBalance_(0,1e+04]"
levels(master_credit_applicant_data$binning.OutstandingBalance)[2]<- "OutstandingBalance_(1e+04,5e+04]"
levels(master_credit_applicant_data$binning.OutstandingBalance)[3]<- "OutstandingBalance_(5e+04,2.5e+05]"
levels(master_credit_applicant_data$binning.OutstandingBalance)[4]<- "OutstandingBalance_(2.5e+05,5e+05]"
levels(master_credit_applicant_data$binning.OutstandingBalance)[5]<- "OutstandingBalance_(5e+05,1e+06]"
levels(master_credit_applicant_data$binning.OutstandingBalance)[6]<- "OutstandingBalance_(1e+06,1.5e+06]"
levels(master_credit_applicant_data$binning.OutstandingBalance)[7]<- "OutstandingBalance_(1.5e+06,2.5e+06]"
levels(master_credit_applicant_data$binning.OutstandingBalance)[8]<- "OutstandingBalance_(2.5e+06,3.5e+06]"
levels(master_credit_applicant_data$binning.OutstandingBalance)[9]<- "OutstandingBalance_(3.5e+06,4.5e+06]"
levels(master_credit_applicant_data$binning.OutstandingBalance)[10]<- "OutstandingBalance_(4.5e+06,5.3e+06]"

summary(master_credit_applicant_data$Total.No.of.Trades)
master_credit_applicant_data$binning.TotalNoOfTrades <- as.factor(cut(master_credit_applicant_data$Total.No.of.Trades, breaks = c(-1,4,9,15,21)))
levels(master_credit_applicant_data$binning.TotalNoOfTrades)
levels(master_credit_applicant_data$binning.TotalNoOfTrades)[1]<- "TotalNoOfTrades_(0,4]"
levels(master_credit_applicant_data$binning.TotalNoOfTrades)[2]<- "TotalNoOfTrades_(4,9]"
levels(master_credit_applicant_data$binning.TotalNoOfTrades)[3]<- "TotalNoOfTrades_(9,15]"
levels(master_credit_applicant_data$binning.TotalNoOfTrades)[4]<- "TotalNoOfTrades_(15,21]"

str(master_credit_applicant_data)
			
#################################  MISSING DATA TREATMENT #########################################################################

#Remove columns for which binning is already done
master_credit_applicant_data_with_binning <- dplyr::select(master_credit_applicant_data, -c(Age,Income,No.of.months.in.current.residence,No.of.months.in.current.company,Avgas.CC.Utilization.in.last.12.months,Outstanding.Balance,Total.No.of.Trades))
str(master_credit_applicant_data_with_binning)
#Check NA values again for data set with BIning
na_count_master_credit_applicant_data_with_bin <- cbind(as.data.frame(colnames(master_credit_applicant_data_with_binning)), as.data.frame(colSums(is.na(master_credit_applicant_data_with_binning))))
na_count_master_credit_applicant_data_with_bin[na_count_master_credit_applicant_data_with_bin$`colSums(is.na(master_credit_applicant_data_with_binning))` > 0,]
# No.of.dependents                                                                             3
# Performance.Tag.x                                                                         1425
# No.of.trades.opened.in.last.6.months                                                         1
# Presence.of.open.home.loan                                                                 272
# Performance.Tag.y                                                                         1425
# binning.AvgCCUtil12Months                                                                 1058
# binning.OutstandingBalance                                                                 272

#Check if their is any Performance.Tag which value differs in Performance.Tag.x  and Performance.Tag.y
setdiff(master_credit_applicant_data_with_binning$Performance.Tag.x,master_credit_applicant_data_with_binning$Performance.Tag.y) 
# 0 diff

#Lets remove duplicate variable Performance.Tag.x
master_credit_applicant_data_with_binning <- dplyr::select(master_credit_applicant_data_with_binning, -Performance.Tag.x)
colnames(master_credit_applicant_data_with_binning)[22] <- "Performance.Tag"

# No.of.dependents has 3 NA values. Since its small in number, lets remove it
master_credit_applicant_data_with_binning <- filter(master_credit_applicant_data_with_binning, !is.na(master_credit_applicant_data_with_binning$No.of.dependents))

# No.of.trades.opened.in.last.6.months has 1 NA value. Since its small in number, lets remove it
master_credit_applicant_data_with_binning <- filter(master_credit_applicant_data_with_binning, !is.na(master_credit_applicant_data_with_binning$No.of.trades.opened.in.last.6.months))

# Gender has 2 blank values with Performance.Tag as 0. Since its small in number, lets remove it
master_credit_applicant_data_with_binning <- filter(master_credit_applicant_data_with_binning, as.character(master_credit_applicant_data_with_binning$Gender) != "")

# Marital.Status..at.the.time.of.application. has 6 blank values with Performance.Tag as 0. Since its small in number, lets remove it
master_credit_applicant_data_with_binning <- filter(master_credit_applicant_data_with_binning, as.character(master_credit_applicant_data_with_binning$Marital.Status..at.the.time.of.application.) != "")

# Type.of.residence has 8 blank values with Performance.Tag as 0. Since its small in number, lets remove it
master_credit_applicant_data_with_binning <- filter(master_credit_applicant_data_with_binning, as.character(master_credit_applicant_data_with_binning$Type.of.residence) != "")

# Profession has 12 blank values with Performance.Tag as 0. Since its small in number, lets remove it
master_credit_applicant_data_with_binning <- filter(master_credit_applicant_data_with_binning, as.character(master_credit_applicant_data_with_binning$Profession) != "")


#Performance.Tag has 1425 NA values. Now We need records with target values populated for training and testing model.
# Records with missing Performance Tag are rejected population.
#Lets remove Performance.Tag having 1425 NA values
master_credit_applicant_approved_data <- filter(master_credit_applicant_data_with_binning, !is.na(master_credit_applicant_data_with_binning$Performance.Tag))

#Lets keep Performance.Tag having  NA values in another dataset
master_credit_applicant_rejected_data <- filter(master_credit_applicant_data_with_binning, is.na(master_credit_applicant_data_with_binning$Performance.Tag))
nrow(master_credit_applicant_rejected_data)
# 1425

# Checking default rate of applicants for data set with performance tag populated

default_rate_credit_data <- (sum(master_credit_applicant_approved_data$Performance.Tag)/nrow(master_credit_applicant_approved_data))*100
default_rate_credit_data
# 4.2 %
###################################################################################################################
#############################################  EDA ################################################################
###################################################################################################################


############################################  UNIVARIATE ANALYSIS #################################################

#----------------------Default Rate and Plot Analysis for different variables for Approved data --------------------

str(master_credit_applicant_approved_data)

# Check the default rate in each bucket of different variables

agg_age <- merge(aggregate(Performance.Tag ~ binning.Age, master_credit_applicant_approved_data, mean),aggregate(Performance.Tag~binning.Age, master_credit_applicant_approved_data, sum),by = "binning.Age") 

# Adding total_count
count <- data.frame(table(master_credit_applicant_approved_data$binning.Age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "default_rate","default_count", "total_count")

# Round Off the values

agg_age$default_rate <- format(round(agg_age$default_rate, 2))

agg_age
#Almost all age group default rate is 4% around. So age does not tells much about default behavior
#Only for young age group (< 25 ), default rate is slighly less i.e. 2% around

#Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, total_count,label = default_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5)

# Again we can see that default rate is low for age group (14-25) compared to other age groups.

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag~cat_var, master_credit_applicant_approved_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "default_rate","total_count")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = default_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}


# Checking No.of.dependents
plot_response(master_credit_applicant_approved_data$No.of.dependents, "No.of.dependents")
#Here we can clearly see that all the groups in no. of dependents have same default rate of around 4%.

# Checking Education
plot_response(master_credit_applicant_approved_data$Education, "Education")
#Here we can clearly see that all the groups in Education have same default rate of around 4% except others which is around 7%

# Checking No.of.times.90.DPD.or.worse.in.last.6.months
plot_response(master_credit_applicant_approved_data$No.of.times.90.DPD.or.worse.in.last.6.months, "No.of.times.90.DPD.or.worse.in.last.6.months")
#Here we can clearly see that bins with frequency 2 and 3 in No.of.times.90.DPD.or.worse.in.last.6.months have higher default rate of around 9-11%  

# Checking No.of.times.60.DPD.or.worse.in.last.6.months
plot_response(master_credit_applicant_approved_data$No.of.times.60.DPD.or.worse.in.last.6.months, "No.of.times.60.DPD.or.worse.in.last.6.months")
#Here we can clearly see that  groups in No.of.times.60.DPD.or.worse.in.last.6.months have higher default rate of around 8% for frequency 2-3 and 9 % for higher frequency

# Checking No.of.times.30.DPD.or.worse.in.last.6.months  
plot_response(master_credit_applicant_approved_data$No.of.times.30.DPD.or.worse.in.last.6.months, "No.of.times.30.DPD.or.worse.in.last.6.months")
#Here we can clearly see that  groups in No.of.times.30.DPD.or.worse.in.last.6.months have higher default rate of around 8% for frequency 2-3 and 10 % for frequency 4-5

# Checking No.of.times.90.DPD.or.worse.in.last.12.months  
plot_response(master_credit_applicant_approved_data$No.of.times.90.DPD.or.worse.in.last.12.months, "No.of.times.90.DPD.or.worse.in.last.12.months")
#Here we can clearly see that groups in No.of.times.90.DPD.or.worse.in.last.12.months have higher default rate of around 8% for frequency 2-3 and 11 % for higher frequency

# Checking No.of.times.60.DPD.or.worse.in.last.12.months  
plot_response(master_credit_applicant_approved_data$No.of.times.60.DPD.or.worse.in.last.12.months, "No.of.times.60.DPD.or.worse.in.last.12.months")
#Here we can clearly see that groups in No.of.times.60.DPD.or.worse.in.last.12.months have higher default rate of around 8% for frequency 2-3, 9% for frequency 4-5 and and 11 % for frequency higher than 5

# Checking No.of.times.30.DPD.or.worse.in.last.12.months  
plot_response(master_credit_applicant_approved_data$No.of.times.30.DPD.or.worse.in.last.12.months, "No.of.times.30.DPD.or.worse.in.last.12.months")
#Here we can clearly see that groups in No.of.times.30.DPD.or.worse.in.last.12.months have higher default rate of around 9% for frequency 3-5, 10% for higher than 5

# Checking No.of.trades.opened.in.last.12.months    
plot_response(master_credit_applicant_approved_data$No.of.trades.opened.in.last.12.months  , "No.of.trades.opened.in.last.12.months")
#Here we can clearly see that  groups in No.of.trades.opened.in.last.12.months   have higher default rate of around 6% for frequency 5-8 and 7% for frequency 9-12

# Checking No.of.PL.trades.opened.in.last.6.months    
plot_response(master_credit_applicant_approved_data$No.of.PL.trades.opened.in.last.6.months  , "No.of.PL.trades.opened.in.last.6.months")
#Here we can clearly see that groups in No.of.PL.trades.opened.in.last.6.months   have higher default rate of around 6% for frequency 2-4 and  less than 6% for other frequency

# Checking No.of.PL.trades.opened.in.last.12.months    
plot_response(master_credit_applicant_approved_data$No.of.PL.trades.opened.in.last.12.months  , "No.of.PL.trades.opened.in.last.12.months")
#Here we can clearly see that groups in No.of.PL.trades.opened.in.last.12.months have higher default rate of around 6% for frequency 3-5 and  less than 6% for other frequency

# Checking No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.    
plot_response(master_credit_applicant_approved_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.  , "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")
#Here we can clearly see that groups in No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. have higher default rate of around 6% for frequency 2-4 and less than 6% for other frequency

# Checking No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.    
plot_response(master_credit_applicant_approved_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.  , "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")
#Here we can clearly see that  groups in No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. have higher default rate of around 7% for frequency 5-8 and less than 7% for other frequency

# Checking Presence.of.open.home.loan    
plot_response(master_credit_applicant_approved_data$Presence.of.open.home.loan  , "Presence.of.open.home.loan")
#Here we can clearly see that groups in Presence.of.open.home.loan have higher default rate of around 5% for 0 frequency and 3% for  frequency 1

# Checking Presence.of.open.auto.loan    
plot_response(master_credit_applicant_approved_data$Presence.of.open.auto.loan  , "Presence.of.open.auto.loan")
#Here we can clearly see that all groups in Presence.of.open.home.loan have higher default rate of around 4% 

# Checking binning.Income     
plot_response(master_credit_applicant_approved_data$binning.Income   , "binning.Income")
#Here we can clearly see that  all groups in binning.Income have higher default rate of around 5% and less than 5% for other income groups

# Checking binning.NumMonthsCurrResidence     
plot_response(master_credit_applicant_approved_data$binning.NumMonthsCurrResidence   , "binning.NumMonthsCurrResidence")
#Here we can clearly see that  all groups in binning.NumMonthsCurrResidence have higher default rate of around 8% and less than 5% for other binning.NumMonthsCurrResidence groups

# Checking binning.AvgCCUtil12Months     
plot_response(master_credit_applicant_approved_data$binning.AvgCCUtil12Months, "binning.AvgCCUtil12Months")
#Here we can clearly see that  groups in binning.AvgCCUtil12Months have higher default rate of around 7%  for CC utilization between 30% and 80% and less than 7% for other groups

# Checking binning.OutstandingBalance     
plot_response(master_credit_applicant_approved_data$binning.OutstandingBalance, "binning.OutstandingBalance")
#Here we can clearly see that  groups in binning.OutstandingBalance have higher default rate of around 6%  for outstanding balance between 500000 and 1500000 and less than 6% for other groups

# Checking binning.TotalNoOfTrades     
plot_response(master_credit_applicant_approved_data$binning.TotalNoOfTrades, "binning.TotalNoOfTrades")
#Here we can clearly see that  groups in binning.TotalNoOfTrades have higher default rate of around 7%  for TotalNoOfTrades between 9-15 and less than 7% for other groups


############################################  MULTIVARIATE ANALYSIS #################################################

#Multi Variate Analysis
numeric_attributes <- as.data.frame(dplyr::select(master_credit_applicant_data,c(Age,Income,Avgas.CC.Utilization.in.last.12.months,Outstanding.Balance)))

#Plotting the correlation between the variables 
creditcor <- cor(numeric_attributes)
creditcor
#                                         Age   Income 
# Age                                    1.000000 0.062331    
# Income                                 0.062331 1.000000

#Not much correlation between numeric variables . both remains almost same.

##Scatter plot between Outstanding.Balance and Avgas.CC.Utilization.in.last.12.months which shows a decreasing trend
ggplot(master_credit_applicant_data[!is.na(master_credit_applicant_data$Performance.Tag.x),], aes(x=Outstanding.Balance, y = Avgas.CC.Utilization.in.last.12.months)) + geom_point() + geom_smooth() +  labs(title = "Scatter Plot",subtitle="Outstanding.Balance Vs Avgas.CC.Utilization.in.last.12.months")
#Their is a decreasing trend of Avgas.CC.Utilization.in.last.12.months when Outstanding.Balance increases. 
# When people have very high outstanding balance then CC utilization scope decreases

##Scatter plot between Income and Avgas.CC.Utilization.in.last.12.months which shows a linear relationship with -ve coefficient
ggplot(master_credit_applicant_data[!is.na(master_credit_applicant_data$Performance.Tag.x),], aes(x=Income, y = Avgas.CC.Utilization.in.last.12.months)) + geom_point() + geom_smooth() +  labs(title = "Scatter Plot",subtitle="Income Vs Avgas.CC.Utilization.in.last.12.months")
# Avg CC utilization slightly decreases with increase in income has linear relationship with -ve coefficient

##Scatter plot between Income and Outstanding.Balance which shows a linear relationship like a constant line
ggplot(master_credit_applicant_data[!is.na(master_credit_applicant_data$Performance.Tag.x),], aes(x=Income, y = Outstanding.Balance)) + geom_point() + geom_smooth() +  labs(title = "Scatter Plot",subtitle="Income Vs Outstanding.Balance")
# Income and Outstanding.Balance have a perfect linear relationship

##Scatter plot between age and Income which shows a linear relationship
ggplot(master_credit_applicant_data[!is.na(master_credit_applicant_data$Performance.Tag.x),], aes(x=Age, y = Income)) + geom_point() + geom_smooth() +  labs(title = "Scatter Plot",subtitle="Age Vs Income")
# age and Income does not have any increasing or decreasing trend .Its almost varies up and down with in small range 



#################################### WOE AND INFORMATION VALUE ANALYSIS ##########################################################

#------------ WOE and Information Value analysis ---------------------------

master_credit_final_data <- master_credit_applicant_approved_data

master_credit_IV <- create_infotables(data=master_credit_final_data, y="Performance.Tag", bins=10, parallel=TRUE)
master_credit_IV_Value = data.frame(master_credit_IV$Summary)
master_credit_IV_Value$IV <-  round(master_credit_IV_Value$IV,2)
View(master_credit_IV_Value)
print(master_credit_IV_Value)


#As per IV guidelines Information Value less than 0.02 is not useful for prediction
#So we will consider only those variables with some predictive power for model building

# Variable   IV
# 26                                       binning.AvgCCUtil12Months 0.32
# 27                                      binning.OutstandingBalance 0.23
# 15                           No.of.trades.opened.in.last.12.months 0.22
# 28                                         binning.TotalNoOfTrades 0.22
# 17                        No.of.PL.trades.opened.in.last.12.months 0.18
# 8                     No.of.times.90.DPD.or.worse.in.last.6.months 0.16
# 10                    No.of.times.30.DPD.or.worse.in.last.6.months 0.15
# 12                   No.of.times.60.DPD.or.worse.in.last.12.months 0.14
# 16                         No.of.PL.trades.opened.in.last.6.months 0.13
# 13                   No.of.times.30.DPD.or.worse.in.last.12.months 0.12
# 14                            No.of.trades.opened.in.last.6.months 0.11
# 18  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 0.10
# 24                                  binning.NumMonthsCurrResidence 0.10
# 19 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.10
# 11                   No.of.times.90.DPD.or.worse.in.last.12.months 0.10
# 9                     No.of.times.60.DPD.or.worse.in.last.6.months 0.09
# 23                                                  binning.Income 0.03
# 20                                      Presence.of.open.home.loan 0.02


# Plot IV
plotFrame <- master_credit_IV$Summary[order(-master_credit_IV$Summary$IV), ]
plotFrame$Variable <- factor(plotFrame$Variable,
                             levels = plotFrame$Variable[order(-plotFrame$IV)])
ggplot(plotFrame, aes(x = Variable, y = IV)) +
  geom_bar(width = .35, stat = "identity", color = "darkblue", fill = "white") +
  ggtitle("Information Value") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))


#WOE Values of individual variables
master_credit_IV$Tables$Gender
master_credit_IV$Tables$binning.OutstandingBalance
#To see trend of WOE variables, you can plot them by using plot_infotables function.
plot_infotables(master_credit_IV, "Gender")

#Plot multiple variables individually
names <- names(master_credit_IV$Tables)
plots <- list()
for (i in 1:length(names)){
  plots[[i]] <- plot_infotables(master_credit_IV, names[i])
}
# Showing the top 28 variables
plots[1:28]
# WOE Plot analysis for important  variables
# [2] Gender: Odds of default is higher in F compare to M
# [3] Marital.Status..at.the.time.of.application. :  Odds of default is higher in Single compare to Married 
# [4] "No.of.dependents"                                               
# [5] "Education"                                                      
# [6] "Profession"                                                     
# [7] "Type.of.residence"                                              
# [8] "No.of.times.90.DPD.or.worse.in.last.6.months"                   
# [9] "No.of.times.60.DPD.or.worse.in.last.6.months"                   
# [10] "No.of.times.30.DPD.or.worse.in.last.6.months"                   
# [11] "No.of.times.90.DPD.or.worse.in.last.12.months"                  
# [12] "No.of.times.60.DPD.or.worse.in.last.12.months"                  
# [13] "No.of.times.30.DPD.or.worse.in.last.12.months"                  
# [14] "No.of.trades.opened.in.last.6.months"                           
# [15] "No.of.trades.opened.in.last.12.months"                          
# [16] "No.of.PL.trades.opened.in.last.6.months"                        
# [17] "No.of.PL.trades.opened.in.last.12.months"                       
# [18] "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans." 
# [19] "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."
# [20] "Presence.of.open.home.loan"                                     
# [21] "Presence.of.open.auto.loan"                                     
# [22] "binning.Age"                                                    
# [23] "binning.Income"                                                 
# [24] "binning.NumMonthsCurrResidence"                                 
# [25] "binning.NumMonthsCurrCompany"                                   
# [26] "binning.AvgCCUtil12Months"                                      
# [27] "binning.OutstandingBalance"                                     
# [28] "binning.TotalNoOfTrades" 


#Function to replace original values with WOE values
woe_replace <- function(df_orig, IV) {
  df <- cbind(df_orig)
  df_clmtyp <- data.frame(clmtyp = sapply(df, class))
  df_col_typ <-
    data.frame(clmnm = colnames(df), clmtyp = df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    colmn_nm <- toString(df_col_typ[rownm, "clmnm"])    
    if(colmn_nm %in% names(IV$Tables)){
      column_woe_df <- cbind(data.frame(IV$Tables[[toString(df_col_typ[rownm, "clmnm"])]]))
      if (df_col_typ[rownm, "clmtyp"] == "factor" | df_col_typ[rownm, "clmtyp"] == "character") {
        df <-
          dplyr::inner_join(
            df,
            column_woe_df[,c(colmn_nm,"WOE")],
            by = colmn_nm,
            type = "inner",
            match = "all"
          )
        df[colmn_nm]<-NULL
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm
      } else if (df_col_typ[rownm, "clmtyp"] == "numeric" | df_col_typ[rownm, "clmtyp"] == "integer") {
        column_woe_df$lv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr("\\[", column_woe_df[,colmn_nm]) + 1,
          regexpr(",", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df$uv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr(",", column_woe_df[,colmn_nm]) + 1,
          regexpr("\\]", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df[colmn_nm]<-NULL      
        column_woe_df<-column_woe_df[,c("lv","uv","WOE")]      
        colnames(df)[colnames(df)==colmn_nm]<-"WOE_temp2381111111111111697"      
        df <-
          fuzzy_inner_join(
            df,
            column_woe_df[,c("lv","uv","WOE")],
            by = c("WOE_temp2381111111111111697"="lv","WOE_temp2381111111111111697"="uv"),
            match_fun=list(`>=`,`<=`) 
          )      
        df["WOE_temp2381111111111111697"]<-NULL      
        df["lv"]<-NULL      
        df["uv"]<-NULL      
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm      
      }}
  }
  return(df)
}

# WOE Transformation
#Replace actual values with WOE values
master_credit_woe_data <- woe_replace((dplyr::select(master_credit_final_data, -Application.ID)), master_credit_IV)
master_credit_woe_data <- cbind(master_credit_final_data$Application.ID,master_credit_woe_data)
colnames(master_credit_woe_data)[1] <- "Application.ID"

#Replace the actual values with WOE values for Rejected data also
master_credit_woe_rejected_data <- woe_replace((dplyr::select(master_credit_applicant_rejected_data, -Application.ID)), master_credit_IV)
master_credit_woe_rejected_data <-  cbind(master_credit_applicant_rejected_data$Application.ID,master_credit_woe_rejected_data)
colnames(master_credit_woe_rejected_data)[1] <- "Application.ID"

#Plot correlation between variables with WOE values
creditwoecor <- cor(master_credit_woe_data[,-master_credit_woe_data$Performance.Tag])
View(creditwoecor)
#Here we can see that DPD related variables and No. of trades related variables are positively correlated.

master_credit_woe_final_data <- master_credit_woe_data

#-----------Load Libraries for Model Building ----------------------------------------------  

library(caret)
library(caTools)
library(dummies)
  

#WOE Transformed Data Set
master_credit_woe_final_data$Performance.Tag <- as.factor(ifelse(master_credit_woe_final_data$Performance.Tag == 1, "yes", "no"))

###############################################################################################################
################################## MODEL BUILDING #############################################################
###############################################################################################################

#-------------------Model Building Using Unbalanced Data-----------------------------------------------------
#-------------------Logistic Regression Using Unbalanced Data------------------------------------------------

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(master_credit_woe_final_data$Performance.Tag, SplitRatio = 0.70)

train <- master_credit_woe_final_data[split_indices, ]
test <- master_credit_woe_final_data[!split_indices, ]

## Let's check the count of unique value in the target variable of un-balanced dataset
as.data.frame(table(train$Performance.Tag))
# Var1  Freq
# 1   no 46823
# 2  yes  2064

print(prop.table(table(train$Performance.Tag)))
# no        yes 
# 0.96    0.04

defaultRate <- 2064/46823
defaultRate
#4.4%

nrow(train)/nrow(master_credit_woe_final_data)
# 0.70

nrow(test)/nrow(master_credit_woe_final_data)
#.3

#---------------------------------------------------------    
################################## MODEL BUILDING ###############################################################################
################################## DEMOGRAPHIC DATA - LR - UNBALANCED ###########################################################
			
### Model 1: Logistic Regression using unbalanced Data for only Demographic data

library(MASS)
library(car)

#Use only Demographic data
colnames(train[,c(2:8,23:26)])
logistic_1 <- glm(train$Performance.Tag ~ ., family = "binomial", data =train[,c(2:8,23:26)])
summary(logistic_1)
# AIC: 16824
#--------------------------------------------------------- 

# Using stepwise algorithm for removing insignificant variables 
# Commented it as it takes lot of time for final result

logistic_2 <- stepAIC(logistic_1, direction = "both")
summary(logistic_2)
#AIC: 16819
# stepAIC has removed some variables and only the following ones remain

logistic_2 <- glm(formula = train$Performance.Tag ~ Profession + binning.Age + 
					binning.Income + binning.NumMonthsCurrResidence + binning.NumMonthsCurrCompany, 
				  family = "binomial", data = train[, c(2:8,23:26)])
# AIC: 16819

# checking vif for logistic_2 
sort(vif(logistic_2),decreasing = TRUE)
summary(logistic_2)

#Remove Profession as its not significant
logistic_3 <- glm(formula = train$Performance.Tag ~ binning.Age + 
					binning.Income + binning.NumMonthsCurrResidence + binning.NumMonthsCurrCompany, 
				  family = "binomial", data = train[, c(2:8,23:26)])

# checking vif for logistic_3 
sort(vif(logistic_3),decreasing = TRUE)
summary(logistic_3)

#Remove  `binning.Age`  
logistic_4 <-  glm(formula = train$Performance.Tag ~ binning.Income + 
					 binning.NumMonthsCurrResidence + binning.NumMonthsCurrCompany, 
				   family = "binomial", data = train[, c(2:8,23:26)])

# checking vif for logistic_4 
sort(vif(logistic_4),decreasing = TRUE)
summary(logistic_4)

#This model seems to have good p values with less VIF and following are important predictors as 
# per Logistics regression model where Income and NumMonthsCurrResidence were also part of IV variables

# Coefficients:
#   Estimate Std. Error  z value Pr(>|z|)    
#   (Intercept)                    -3.12186    0.02287 -136.480  < 2e-16 ***
#   binning.Income                  0.75824    0.12609    6.014 1.81e-09 ***
#   binning.NumMonthsCurrResidence  0.95337    0.07086   13.455  < 2e-16 ***
#   binning.NumMonthsCurrCompany    1.13344    0.19696    5.755 8.67e-09 ***

logistic_final_1 <- logistic_4

#Predicting probabilities of responding for the test data
predictions_logit <- predict(logistic_final_1, newdata = test[, c(2:8,23:26)], type = "response")
summary(predictions_logit)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02357 0.03028 0.03811 0.04235 0.05119 0.09952  
#--------------------------------------------------------- 

#---------------------- Model Evaluation: Logistic Regression Using Unbalanced Data -------------------------

# Let's use the probability cutoff of 10%.

predicted_response <- factor(ifelse(predictions_logit >= 0.08, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "yes")

conf
#Accuracy : 0.9388   
#Sensitivity : 0.045249
#Specificity : 0.978123 

# Let's find out the optimal probability cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, as.factor(test$Performance.Tag), positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initializing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{ 
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
#Please keep the plot window large enough to be drawn
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    
#0.1 value is selected based upon trial and error as value less than 0.07 is not giving any cutoff value
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<.05)]
cutoff
#[1] 0.03969697 ~ 0.04 (4%)

# Let's use the probability cutoff of 4%.

predicted_response <- factor(ifelse(predictions_logit >= 0.04, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "yes")

conf
#Accuracy : 0.5644   
#Sensitivity : 0.58937
#Specificity : 0.56331 

			
################################## MODEL BUILDING ##############################################################################
################################## MASTER DATA- LR - UNBALANCED ################################################################

#-------------------------------------- Model Building On Master Data ----------------------------------------------------------

### Model 1: Logistic Regression using unbalanced Data on complete data

logistic_1 <- glm(train$Performance.Tag ~ ., family = "binomial", data = train[,-1])
summary(logistic_1)
# AIC: 16357
#---------------

# Using stepwise algorithm for removing insignificant variables 

# Please Comment lines 1086 and 1087 in case if you want to directly use its output
# mentioned in line 1089-1095 since stepAIC takes lot of time for final result

logistic_2 <- stepAIC(logistic_1, direction = "both")
summary(logistic_2)
# glm(formula = train$Performance.Tag ~ Profession + No.of.times.30.DPD.or.worse.in.last.6.months + 
#       No.of.times.30.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
#       No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
#       No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
#       binning.Age + binning.NumMonthsCurrCompany + binning.AvgCCUtil12Months + 
#       binning.OutstandingBalance + binning.TotalNoOfTrades, family = "binomial", 
#     data = train)
#AIC: 16337
# stepAIC has removed some variables and only the following ones remain

logistic_2 <- glm(formula = train$Performance.Tag ~ Profession + No.of.times.30.DPD.or.worse.in.last.6.months + 
                    No.of.times.30.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                    binning.Age + binning.NumMonthsCurrCompany + binning.AvgCCUtil12Months + 
                    binning.OutstandingBalance + binning.TotalNoOfTrades, family = "binomial", 
                  data = train[,-1])

# checking vif for logistic_2 
sort(vif(logistic_2),decreasing = TRUE)
summary(logistic_2)

#Remove No.of.times.30.DPD.or.worse.in.last.6.months as its not significant
logistic_3 <- glm(formula = train$Performance.Tag ~ Profession +  
                    No.of.times.30.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                    binning.Age + binning.NumMonthsCurrCompany + binning.AvgCCUtil12Months + 
                    binning.OutstandingBalance + binning.TotalNoOfTrades, family = "binomial", 
                  data = train[,-1])

# checking vif for logistic_2 
sort(vif(logistic_3),decreasing = TRUE)
summary(logistic_3)

#Remove   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.  
logistic_4 <-  glm(formula = train$Performance.Tag ~ Profession +  
                     No.of.times.30.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                     binning.Age + binning.NumMonthsCurrCompany + binning.AvgCCUtil12Months + 
                     binning.OutstandingBalance + binning.TotalNoOfTrades, family = "binomial", 
                   data = train[,-1])

# checking vif for logistic_2 
sort(vif(logistic_4),decreasing = TRUE)
summary(logistic_4)

#Remove  binning.Age
logistic_5 <-  glm(formula = train$Performance.Tag ~ Profession +  
                     No.of.times.30.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                     binning.NumMonthsCurrCompany + binning.AvgCCUtil12Months + 
                     binning.OutstandingBalance + binning.TotalNoOfTrades, family = "binomial", 
                   data = train[,-1])

# checking vif for logistic_2 
sort(vif(logistic_5),decreasing = TRUE)
summary(logistic_5)

#Remove  binning.NumMonthsCurrCompany   
logistic_6 <-  glm(formula = train$Performance.Tag ~ Profession +  
                     No.of.times.30.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                     binning.AvgCCUtil12Months + 
                     binning.OutstandingBalance + binning.TotalNoOfTrades, family = "binomial", 
                   data = train[,-1])

# checking vif for logistic_6 
sort(vif(logistic_6),decreasing = TRUE)
summary(logistic_6)

#Remove  No.of.PL.trades.opened.in.last.6.months  
logistic_7 <-  glm(formula = train$Performance.Tag ~ Profession +  
                     No.of.times.30.DPD.or.worse.in.last.12.months + 
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                     binning.AvgCCUtil12Months + 
                     binning.OutstandingBalance + binning.TotalNoOfTrades, family = "binomial", 
                   data = train[,-1])

# checking vif for logistic_2 
sort(vif(logistic_7),decreasing = TRUE)
summary(logistic_7)

#Remove No.of.Inquiries.in.last.6.months..excluding.home...auto.loans
logistic_8 <-  glm(formula = train$Performance.Tag ~ Profession +  
                     No.of.times.30.DPD.or.worse.in.last.12.months + 
                     binning.AvgCCUtil12Months + 
                     binning.OutstandingBalance + binning.TotalNoOfTrades, family = "binomial", 
                   data = train[,-1])

# checking vif for logistic_2 
sort(vif(logistic_8),decreasing = TRUE)
summary(logistic_8)

# Remove  Profession
logistic_9 <-  glm(formula = train$Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.12.months + 
                     binning.AvgCCUtil12Months + 
                     binning.OutstandingBalance + binning.TotalNoOfTrades, family = "binomial", 
                   data = train[,-1])

# checking vif for logistic_2 
sort(vif(logistic_9),decreasing = TRUE)
summary(logistic_9)

#This model seems to have good p values with less VIF

# Coefficients:
#   Estimate Std. Error  z value Pr(>|z|)    
# (Intercept)                                   -3.12219    0.02359 -132.346  < 2e-16 ***
#   No.of.times.30.DPD.or.worse.in.last.12.months  0.43097    0.06225    6.923 4.42e-12 ***
#   binning.AvgCCUtil12Months                      0.62531    0.05715   10.942  < 2e-16 ***
#   binning.OutstandingBalance                     0.28473    0.07164    3.974 7.06e-05 ***
#   binning.TotalNoOfTrades                        0.26800    0.07106    3.771 0.000162 ***

logistic_final_2 <- logistic_9

#Predicting probabilities of responding for the test data
predictions_logit <- predict(logistic_final_2, newdata = test, type = "response")
summary(predictions_logit)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01614 0.02035 0.03085 0.04232 0.06388 0.10974  
#--------------------------------------------------------- 

#----------- Model Evaluation: Logistic Regression ------------------------------------

# Let's use the probability cutoff of 10%.

predicted_response <- factor(ifelse(predictions_logit >= 0.1, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "yes")

conf
#Accuracy : 0.9283   
#Sensitivity : 0.062217
#Specificity : 0.966462 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, as.factor(test$Performance.Tag), positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{ 
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
#Please keep the plot window large enough to be drawn
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    
#0.1 value is selected based upon trial and error as value less than 0.07 is not giving any cutoff value
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<.06)]
cutoff
#[1] 0.04959596 ~ 0.05 (5%)

# Let's use the probability cutoff of 5%.

predicted_response <- factor(ifelse(predictions_logit >= 0.05, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "yes")

conf
#Accuracy : 0.6087   ~ 61%
#Sensitivity : 0.63914 ~ 64%
#Specificity : 0.60732 ~ 61%

################################## MODEL BUILDING  ############################################################################
################################## MASTER DATA- DT- UNBALANCED ################################################################
		
## Model Building- Model 2: Decision Tree using unbalanced data

# Packages Required

#---------------------------------------------------------    
library(caret)
library(caTools)
library(rpart)
library(rattle)

train_dt <- train[,-1]
test_dt <- test[,-1]

#---------------------------------------------------------    

# building a tree with arbitrary minsplit and cp
mastercredit_1 <-  rpart(Performance.Tag ~ ., data=train_dt, method= "class", 
						 control=rpart.control(minsplit=10, cp=0.0001))

plot(mastercredit_1)

# This is clearly an overfitted tree
# Classic decision tree problem

# Increasing the minsplit two fold to 20 
mastercredit_2 <-  rpart(Performance.Tag ~ ., data=train_dt, method= "class",
						 control=rpart.control(minsplit=20, cp=0.0001))

plot(mastercredit_2)

# This one is better, but still looks a little too complex
# install rpart.plot and load the library
#install.packages("rpart.plot")
library(rpart.plot)

rattle::fancyRpartPlot(mastercredit_2)

# Listing the variables by importance: AvgCCUtil12Months, No.of.trades.opened.in.last.12.months, binning.OutstandingBalance are the top 3
mastercredit_2$variable.importance

# mastercredit_2 looks like an acceptable model;  increase further does not give rplot

#---------------------------------------------------------  

## Model Evaluation for mastercredit_1 and mastercredit_2
# using test data from now on
# mastercredit_1
mastercredit_1_pred <- predict(mastercredit_1, test_dt[, -1], type = "class")
confusionMatrix(as.factor(mastercredit_1_pred),as.factor(test_dt$Performance.Tag), positive = "yes")
# Accuracy : 0.9452
# Sensitivity : 0.023756
# Specificity : 0.985798

# using test data from now on
# mastercredit_2
mastercredit_2_pred <- predict(mastercredit_2, test_dt[, -1], type = "class")
confusionMatrix(as.factor(mastercredit_2_pred),as.factor(test_dt$Performance.Tag), positive = "yes")
# Accuracy : 0.9569
# Sensitivity : 0.0033937
# Specificity : 0.9989535

# Sensitivity is again very low here; we can improve the model quite a bit since logistic model has sensitivtiy around 60%
# we should rather build a random forest instead
# It will avoid overwriting 

#--------------------------------------------------------- 
################################## MODEL BUILDING  ############################################################################
################################## MASTER DATA- RF - UNBALANCED ###############################################################
 
# Package required for randomForest algorithm is:
# install randomForest
library(randomForest)
#---------------------------------------------------------    

# Splitting the bank data in 70:30 ratio

train_rf <- train
test_rf <- test

#---------------------------------------------------------    

# Building the model 

mastercredit_rf <- randomForest(Performance.Tag ~., data = train_rf[,-1], proximity = F, do.trace = T, mtry = 5)

# Predict response for test data

rf_pred <- predict(mastercredit_rf, test_rf[, c(-1,-2)], type = "prob")

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.08)]
cutoff_rf
# 0.03969697
# The plot shows that cutoff value of around 4% optimises sensitivity and accuracy

predicted_response_22 <- factor(ifelse(rf_pred[, 2] >= 0.04, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_22, test_rf[, 2], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1]
#0.6210407

# Specificity 
conf_forest$byClass[2]
#0.583894 

# Accuracy 
conf_forest$overall[1]
#0.5854613 
################################## MODEL BUILDING #############################################################################
################################## MASTER DATA+ REJECTED DATA- RF - UNBALANCED ################################################
			
# Final RF important variables
importance <- mastercredit_rf$importance 

importance <- data.frame(importance)
importance

#Lets use Random forest model as the final model which has highest Sensitivity
# We will predict the rejected data model and use this data back to re-train the Random forest model 

rf_rejected_pred <- predict(mastercredit_rf, master_credit_woe_rejected_data[,  c(-1,-2)], type = "prob")
rf_rejected_pred_22 <- factor(ifelse(rf_rejected_pred[, 2] >= 0.045, "yes", "no"))
table(rf_rejected_pred_22)
# no  yes 
# 103 1321 
master_credit_woe_rejected_data$Performance.Tag <- as.factor(rf_rejected_pred_22)
#---------------------------------------------------------  

dim(master_credit_woe_final_data)
# 69838    29
dim(master_credit_woe_rejected_data)
# 1424 29
master_credit_final_data_3 <- rbind(master_credit_woe_final_data,filter(master_credit_woe_rejected_data, master_credit_woe_rejected_data$Performance.Tag != "no"))
dim(master_credit_final_data_3)
#71159    29

set.seed(1)

split_indices <- sample.split(master_credit_final_data_3$Performance.Tag, SplitRatio = 0.70)

train_rf <- master_credit_final_data_3[split_indices, ]
test_rf <- master_credit_final_data_3[!split_indices, ]

#---------------------------------------------------------    

# Building the model 

mastercredit_rf <- randomForest(Performance.Tag ~., data = train_rf[,-1], proximity = F, do.trace = T, mtry = 5)

# Predict response for test data

rf_pred <- predict(mastercredit_rf, test_rf[, c(-1,-2)], type = "prob")

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.03)]
cutoff_rf
#0.05949495
# The plot shows that cutoff value of around 6% optimises sensitivity and accuracy

predicted_response_22 <- factor(ifelse(rf_pred[, 2] >= 0.05949, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_22, test_rf[, 2], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1]
#0.66

# Specificity 
conf_forest$byClass[2]
#0.69 

# Accuracy 
conf_forest$overall[1]
#0.69 

			
#################################################### MODEL BUILDING  #####################################################################
#################################################### MASTER DATA+ REJECTED DATA ----RF-----BALANCE #######################################

#--------------------------Model Building using Balanced Data --------------------------------------------
#Lets use Smote for balancing data as no. of default data is very less
## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification

## Loading DMwr to balance the unbalanced class
#install.packages("DMwR")
library(DMwR)

## Use Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balancedCreditData <- SMOTE(Performance.Tag ~., train_rf, perc.over = 300, perc.under = 300)
test_rf2 <- test_rf

## Let's check the count of unique value in the target variable of balanced dataset
as.data.frame(table(balancedCreditData$Performance.Tag))
# Var1  Freq
# 1   no 27081
# 2  yes 12036

print(prop.table(table(balancedCreditData$Performance.Tag)))
# no       yes 
# 0.6923077 0.3076923 

defaultRate <- 12036/27081
defaultRate
#44.4%

#---------------------------------------------------------    

# Building the RF model using balanced data got from SMOTE

mastercredit_rf2 <- randomForest(Performance.Tag ~., data = balancedCreditData[,-1], proximity = F, do.trace = T, mtry = 5)

# Predict response for test data

rf_pred2 <- predict(mastercredit_rf2, test_rf2[, c(-1,-2)], type = "prob")
table(test_rf2$Performance.Tag)
#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred2[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf2$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.027)]
cutoff_rf
# The plot shows that cutoff value of around 13.8% optimises sensitivity and accuracy

predicted_response_23 <- factor(ifelse(rf_pred2[, 2] >= 0.138, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_23, test_rf2[, 2], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1]
#0.6721311

# Specificity 
conf_forest$byClass[2]
#0.6863009  

# Accuracy 
conf_forest$overall[1]
#0.6854506  
############################################## FINAL MODEL  ###################################################################
###############################################################################################################################			

#Balancing the data does not give much improvement here. Accuracy parameters are on similar lines
# to unbalanced data

# So we will use the following Random forest model generated from Unbalanced data as our final model.

#----------- Final Model -------------------------------------
mastercredit_rf

#----------- Final Predicted Probability ---------------------
rf_pred

cutoff_rf <- 0.05949
# cutoff value of around 0.05949 i.e. 6% optimises sensitivity and accuracy


#---------- Final Predicted Response --------------------------
#predicted_response_22

conf_forest <- confusionMatrix(predicted_response_22, test_rf[, 2], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1]
#0.66 ~ 66%

# Specificity 
conf_forest$byClass[2]
#0.69 ~ 69%

# Accuracy 
conf_forest$overall[1]
#0.69 ~ 69%

#####------  Model Evaluation using Gain and Lift Chart----------------------------------
# Appending the probabilities and response variables to the test data
test_rf$predicted_response <- predicted_response_22
test_rf$predicted_probs <- rf_pred[,2]

#---------------------------------------------------------    

# Creating new dataframe "test_predictions"
test_predictions <- test_rf[, c("Application.ID","Performance.Tag", "predicted_probs", "predicted_response")]

#Adding Outstanding balance per application  id
test_predictions <- merge(test_predictions,master_credit_applicant_data[,c("Application.ID","Outstanding.Balance")],by="Application.ID")

summary(test_predictions$predicted_response)
# no     yes 
# 14186  7169 

response_rate <- table(test_rf$Performance.Tag)[2]/(table(test_rf$Performance.Tag)[1] + table(test_rf$Performance.Tag)[2])
#0.06036057
pred_response_rate <- table(test_rf$predicted_response)[2]/(table(test_rf$predicted_response)[1] + table(test_rf$predicted_response)[2])
#0.3357059

##################################################  GAIN / LIFT #####################################################################

#First we will create lift and gain chart based upon predicted probabilities in descending order to see what cost it incur

#---------------  First way Model Evaluation based upon decreasing predicted probabilities ------------------------------------ 
# sorting the probabilities in decreasing order 

test_predictions <- test_predictions[order(test_predictions$predicted_probs, decreasing = T), ]
table(test_predictions$predicted_response)
# no   yes 
# 14241  7112 
predicted_response_rate <- 7112/(14241+7112)
predicted_response_rate
#33% predicted response is yes
#So here we can assume that top 30% applicant predicted response is yes i.e. defaulted assuming probability cutoff is 6%

nrow(test_predictions[test_predictions$predicted_probs >= 0.0595, ])
#7112
test_predictions_top33_percent <- test_predictions[test_predictions$predicted_probs >= 0.0595, ]
table(test_predictions_top33_percent$Performance.Tag)
# 0    1 
# 6267  845 
actual_response_rate <- 845/(6267+845)
actual_response_rate
# 0.1188133 i.e. 12%

# plotting the lift chart


lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
	summarise_at(vars(labels ), funs(total = n(),
									 totalresp=sum(., na.rm = TRUE))) %>%
	mutate(Cumresp = cumsum(totalresp),
		   Gain=Cumresp/sum(totalresp)*100,
		   Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 

test_predictions$Performance.Tag <- as.factor(ifelse(test_predictions$Performance.Tag=="yes",1,0))

LG = lift(test_predictions$Performance.Tag, test_predictions$predicted_probs, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total Defaulted",ylab = "% of Default Response")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

###################### FINANCIAL BENEFIT ANALYSIS #########################################
#----------------------Benefit in terms of credit loss avoided ----------------------------

# Total Credit Loss which can be avoided
View(LG)

# Top X% of prospects is 30% applicants which can select 63% of total defaulters
# The Cumulative Lift of 2.1 for top 3 deciles,
# means that when selecting 30% of the applicants based on the model, 
# one can expect 2.1 times the total number of applicants (events) found by randomly 
# selecting 50%-of-applicants without a model. 
# Also if we just target 20% of defaulters then we can select 52% of total responders
# with a Cumulative Lift of 2.6 .

### Analysing the Charts: Cumulative gains and lift charts are a graphical 
# representation of the advantage of using a predictive model to choose which 
# customers can default. The lift chart shows how much more likely we are to select
# defaulters than if we select a random sample of customers. For example,
# by identifying only 10% of customers based on the predictive model we will identify 
# 3.8 times as many defaulters as if we use no model.

#Total Outstanding Balance
total_outstanding_bal <- sum(as.numeric(test_predictions$Outstanding.Balance),na.rm = TRUE)
total_outstanding_bal
# total_cost =  26233684841 

#Expected Credit Loss Calculation for top 30% of prospects which gives us 63% of total applicants
first_30_percent_defaulters <- head(test_predictions,0.3*nrow(test_predictions)) 
total_cridit_loss_for_30_percent <- sum(as.numeric(first_30_percent_defaulters$Outstanding.Balance),na.rm = TRUE) 
total_cridit_loss_for_30_percent
# total_cridit_loss_for_30_percent = 8134596132 

fraction_of_total_outstanding_bal <- total_cridit_loss_for_30_percent/total_outstanding_bal
fraction_of_total_outstanding_bal
# 0.3100821
# Avoided predicted credit loss is close to 31% of total outstanding balance which comes around 8134596132 in amount.
# So if we avoid credit loss of around 31% then it can increase the revenue by approx 31%

#Calculation of Actual Credit Loss which can be avoided if we target top 30% applicants
actual_cridit_loss_for_top_30_percent <- sum(as.numeric(first_30_percent_defaulters[first_30_percent_defaulters$Performance.Tag==1,]$Outstanding.Balance),na.rm = TRUE) 
actual_cridit_loss_for_top_30_percent
# 891298152
# Actual Credit Loss which can be avoided is 891298152 if we target top 30% applicants


# We can identify 63% of defaulters with focusing on only first 31% of applicants
# Avoided credit loss is 8134596132
# Avoided credit loss is close to 31% of total outstanding balance which comes around 8134596132 in amount.
# So if we avoid credit loss of around 31% then it can increase the total revenue by approx 31% 
# when we consider that this defaulted amount is not returned back to company by defaulters
# Actual Credit Loss which can be avoided is 891298152 if we target top 30% applicants

#-----------------------Benefit in terms of potential loss of revenue due to rejection of good customers.--------------

rf_rejected_pred24 <- predict(mastercredit_rf, master_credit_woe_rejected_data[,  c(-1,-2)], type = "prob")
predicted_response <- factor(ifelse(rf_rejected_pred24[, 2] >= 0.0595, "yes", "no"))
table(predicted_response)
# no  yes 
# 25 1399 

# Appending the probabilities and response variables to the master_credit_woe_rejected_data data
master_credit_woe_rejected_data$predicted_response <- as.factor(predicted_response)
master_credit_woe_rejected_data$predicted_probs <- rf_rejected_pred24[,2]

#---------------------------------------------------------    

# Creating new dataframe "test_rejected_predictions"
test_rejected_predictions <- master_credit_woe_rejected_data[, c("Application.ID", "predicted_probs", "predicted_response")]

#Adding Outstanding balance per application  id
test_rejected_predictions <- merge(test_rejected_predictions,master_credit_applicant_data[,c("Application.ID","Outstanding.Balance")],by="Application.ID")

summary(test_rejected_predictions$predicted_response)
# no  yes 
# 25 1399

#Total Outstanding Balance for Applicant who got rejected 
#but our model shows that they will probably not default and are good customers
total_good_applicants_outstanding_bal <- sum(as.numeric(test_rejected_predictions[test_rejected_predictions$predicted_response == "no",]$Outstanding.Balance),na.rm = TRUE)
total_good_applicants_outstanding_bal
# total_good_applicants_outstanding_bal =  18204463 

# So Benefit in terms of potential loss of revenue due to rejection of good customers is 18204463 .

###########################################  KS STATISTICS ##########################################################						

library(ROCR)

master_pred <- rf_pred  

#converting predicted output more than cutoff as 1. Else 0
predicted_response <- ifelse(rf_pred[, 2] >= 0.0595 ,1,0) 

#converting all yes as 1. Else 0.
test_rf[, 2] <- ifelse(test_rf[, 2] == "yes",1,0)

model_pred <- prediction(predicted_response,test_rf[, 2] )

model_pred_perf <- performance(model_pred, "tpr", "fpr")

plot(model_pred_perf,col = "red", lab = c(10,10,10))


#KS Statistics

ks_table <- attr(model_pred_perf, "y.values")[[1]] - (attr(model_pred_perf, "x.values")[[1]])

ks_statistics = max(ks_table)

# Maximum KS
ks_statistics 
# 0.35

###############################################################################################
#########################################  SCORE ##############################################
###############################################################################################

# "Applicant id","Actual response"  from the test data
score_data <-  test_rf[, c(1,2)]    # Application.ID, Performance.Tag on Test set
master_pred <- rf_pred  

# probabilities of bad 
score_data$predicted_prob_bad <- master_pred[,2]
#Replace 0 probability with 2nd lst minimum probability to avoid getting infinity odds
score_data[score_data$predicted_prob_bad == 0,]$predicted_prob_bad <- 0.002

# probabilities of good
score_data$predicted_prob_good <- master_pred[,1]

score_data$odds <- score_data$predicted_prob_good/score_data$predicted_prob_bad

score_data$log_odds <- log(score_data$odds)

#Points to double the odds = 20

PDO<-20

#Base Score=400 & odds = 10

BaseScore<-400

Odds<-10

#Calculating Factor & Offset

Factor=PDO/log(2)

Offset=BaseScore-(Factor*log(Odds))

score_data$score <- Offset + (Factor*score_data$log_odds)

# Rounding to the near integer
score_data$score <- round(score_data$score,0)

summary(score_data$score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 319.0   405.0   432.0   440.8   481.0   513.0 

#sort the score_date
score_data <- score_data[order(score_data$score,decreasing = T),]

score_data$Performance.Tag <- ifelse(score_data$Performance.Tag =="yes",1,0)

score_data$predicted_default <- ifelse(score_data$predicted_prob_bad>=0.0595,1,0) 

# Finding the score optimal value
score_data$score[which(score_data$predicted_prob_bad==0.06)] 
# optimal value is 413

scorePlot<- ggplot(score_data , aes(score,predicted_prob_bad),colour="red")+
  geom_line()+  ggtitle("Score v/s probability of bad")+
  geom_hline(yintercept=0.06, col="red")+geom_vline(xintercept= 413)  

scorePlot

# Scores percentile
quantile(score_data$score,seq(0,1,0.1))
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 319  382  398  410  421  432  449  473  493  513  513 

# Score distribution
ggplot(score_data,aes(score))+geom_histogram() +geom_vline(xintercept= 413,col="blue")

# Min Scorecard value is 319
# Max Scorecard value is 513
# Minumm Application Score card value required for credit approval is 413


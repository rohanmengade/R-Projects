
getwd()
adult <- read.csv("C:/Users/Rohan/Desktop/ML2 Project/adult.csv")
View(adult)
#################################
#1 Missing Values ('?') are replaced by most occurring values 
#2 combining marriage and education column values 
#3 income values to 0 and 1
#4 data partition to train, validation, and test data set

################################
head(adult,10)
sum(is.na(adult))
str(adult)
library(dplyr)
library(ggplot2)
library(tidyverse)
library("plyr")
table(adult$workclass)

count(adult, "workclass") # check the count of each attribute in columns
count(adult, "occupation") #table(adult$occupation)
count(adult, "native.country") # table(adult$native.country)

summary(adult)


## replace ? values with most occurring values 

data <- adult %>% mutate(workclass = replace(workclass, workclass == '?', 'Private'), 
                         occupation = replace(occupation, occupation == '?', 'Prof-specialty'),
                         native.country = replace(native.country, native.country == '?', 'United-States'))
head(data)


######
str(data)

#####  Feature engineering, combining Married column 
count(data$marital.status)

data$marital.status <- as.character(data$marital.status)
data$marital.status[data$marital.status == "Married-AF-spouse"] <-"Married"
data$marital.status[data$marital.status == "Married-civ-spouse"] <-"Married"
data$marital.status[data$marital.status == "Married-spouse-absent"] <-"Married"
data$marital.status[data$marital.status == "Divorced"] <-"Other"
data$marital.status[data$marital.status == "Separated"] <-"Other"
data$marital.status[data$marital.status == "Widowed"] <-"Other"
data$marital.status[data$marital.status == "Never-married"] <-"Not Married"
count(data$marital.status)
#### Feature engineering, combining education column
count(data$education)

data$education[data$education == "Preschool"] <-"School"
data$education[data$education == "1st-4th"] <-"School"
data$education[data$education == "5th-6th"] <-"School"
data$education[data$education == "7th-8th"] <-"School"
data$education[data$education == "9th"] <-"School"
data$education[data$education == "10th"] <-"School"
data$education[data$education == "11th"] <-"School"
data$education[data$education == "12th"] <-"School"

data$education[data$education == "HS-grad"] <-"HS Grad"
data$education[data$education == "Some-college"] <-"Higher"
data$education[data$education == "Assoc-voc"] <-"Higher"
data$education[data$education == "Assoc-acdm"] <-"Higher"
data$education[data$education == "Prof-school"] <-"Higher"

count(data$education)

##################### income feature engineering###########
count(data$income)
typeof(data$income)
data$income <- as.character(data$income)
data$income[data$income == "<=50K"] <-"0"
data$income[data$income == ">50K"] <-"1"
typeof(data$income)
count(data$income)

######################### 
data$marital.status <- as.factor(data$marital.status)
data$education <- as.factor(data$education)
data$income <- as.numeric(data$income)
str(data)

########### Data Visualization ################
# Age vs workclass

box_plot <- ggplot(data, aes(x = age, y = workclass, 
                             color = workclass))

box_plot +
  geom_boxplot()

##############################################
# Age Distribution
library(ggplot2)
ggplot(data, aes(age)) + geom_histogram(aes(fill = income), 
                                        color = "white",
                                        binwidth = 2) 

##############################################
# Education and Income Distribution
mosaicplot(data$education~data$income,data=data,
           xlab="Education", ylab="Income",
           col=c("gray","orange"))

###############################################
#our.per.week and Income
boxplot(hours.per.week ~ income, data = data, 
         main = "Hours Per Week distribution for different income levels",
         xlab = "Income", ylab = "Hours Per Week", col = "green")

###############################################
#marital.status and income
ggplot(data) + 
  geom_bar(aes(x = marital.status, fill = factor(income)))


#### Split data into train, validation and test
set.seed(1234)
ind <- sample(3, nrow(data), replace=T, prob = c(0.6,0.2,0.2))
data_train <- data[ind==1,]
data_validation <- data[ind==2,]
data_test <- data[ind==3,]

dim(data_train)
dim(data_validation)
dim(data_test)

head(data_train)
head(data_validation) 
head(data_test)  

############ here on you can use these datasets for model building and predictions


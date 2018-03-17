### Read the input datasets

sessions.data=read.csv(".\\data\\sessions.csv",stringsAsFactors = F);
test.users.data=read.csv(".\\data\\test_users.csv",stringsAsFactors = F);
train.users.data=read.csv(".\\data\\train_users_2.csv",stringsAsFactors = F)

## Install packages and load them

install.packages("dplyr")
library(dplyr)
library(openxlsx)

install.packages("caret")
library(caret)

install.packages("caTools")
library(caTools)

install.packages("ROCR")
library(ROCR)

install.packages("rpart")
library(rpart)


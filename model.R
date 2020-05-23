library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)
library(Metrics)
library(corrplot)

help("cor")
help("corrplot")
help("lm")
help("glm")
help("predict")

#Load the data
dat<- read.csv("R codes/train.csv")

#To remove NA
dat[is.na(dat)]=0

#delete unnecessary columns 
op<-select(dat,-c("X","city","year","weekofyear","week_start_date"))

#Applying correlation function to find out which variables provides best relationship
C<-cor(op)

#Correlation plot
G<-corrplot(C,method = c("square"))
G

#Parameter for splitting dataset and random occurrances
P<-op$total_cases
set.seed(100)

#Splitting data into training and testing
Test<-createDataPartition(P,times=1,p=0.2,list = FALSE)
Train_set<-op %>% slice(-Test)
Test_set<- op %>% slice(Test)

FM<-lm(total_cases~precipitation_amt_mm+reanalysis_air_temp_k+reanalysis_avg_temp_k+reanalysis_dew_point_temp_k+
         reanalysis_max_air_temp_k+reanalysis_min_air_temp_k+precipitation_amt_mm,data = Train_set)

#Load data to be predicted
TT<-read.csv("R codes/dengue_features_test.csv")
TT[is.na(TT)]=0
TTP<-select(TT,-c("city","year","weekofyear","week_start_date"))
TM<-predict(FM,TTP)
TM

#Finding Mean Absolute Error
y_true<-dat$total_cases
ME<-mae((FM$fitted.values),y_true)

Firstc<-select(TT,c("city","year","weekofyear"))
total_cases<- round(TM)
S<- data.frame(Firstc,total_cases)
OPP<-write.csv(S,"R codes/Dengue_op.csv")

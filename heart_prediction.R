rm(list = ls())
ls()

getwd()


## 1. importing the csv files.
library(readr)

heart_dis <- read.csv("heart.csv")
heart_dis
str(heart_dis)

## result #########################################################
'data.frame':	918 obs. of  12 variables:
  $ Age           : int  40 49 37 48 54 39 45 54 37 48 ...
$ Sex           : chr  "M" "F" "M" "F" ...
$ ChestPainType : chr  "ATA" "NAP" "ATA" "ASY" ...
$ RestingBP     : int  140 160 130 138 150 120 130 110 140 120 ...
$ Cholesterol   : int  289 180 283 214 195 339 237 208 207 284 ...
$ FastingBS     : int  0 0 0 0 0 0 0 0 0 0 ...
$ RestingECG    : chr  "Normal" "Normal" "ST" "Normal" ...
$ MaxHR         : int  172 156 98 108 122 170 170 142 130 120 ...
$ ExerciseAngina: chr  "N" "N" "N" "Y" ...
$ Oldpeak       : num  0 1 0 1.5 0 0 0 0 1.5 0 ...
$ ST_Slope      : chr  "Up" "Flat" "Up" "Flat" ...
$ HeartDisease  : int  0 1 0 1 0 0 0 0 1 0 ...
###################################################################

## 1a. removing unwanted columns.
library(dplyr)
df_heart <- select( heart_dis, -c(Sex, ChestPainType, RestingECG, ExerciseAngina,ST_Slope ))
head(df_heart)

## result #######################################################
Age RestingBP Cholesterol FastingBS MaxHR Oldpeak HeartDisease
1  40       140         289         0   172     0.0            0
2  49       160         180         0   156     1.0            1
3  37       130         283         0    98     0.0            0
4  48       138         214         0   108     1.5            1
5  54       150         195         0   122     0.0            0
6  39       120         339         0   170     0.0            0
#################################################################

## 2. understanding the data through plots.
library(ggplot2)

df_heart %>% ggplot(mapping = aes(Age, MaxHR, size=Cholesterol, alpha =0.5)) + geom_point() +
  geom_smooth(method = lm, se=FALSE) 

## 3. transformn HeartDisease from "0"/"1" to "Fail"/"Ok".
df_heart$HeartDisease <- factor(ifelse(df_heart$HeartDisease == 0, "Fail", "Ok" ))
str(df_heart)

## result #########################################################
'data.frame':	918 obs. of  7 variables:
  $ Age         : int  40 49 37 48 54 39 45 54 37 48 ...
$ RestingBP   : int  140 160 130 138 150 120 130 110 140 120 ...
$ Cholesterol : int  289 180 283 214 195 339 237 208 207 284 ...
$ FastingBS   : int  0 0 0 0 0 0 0 0 0 0 ...
$ MaxHR       : int  172 156 98 108 122 170 170 142 130 120 ...
$ Oldpeak     : num  0 1 0 1.5 0 0 0 0 1.5 0 ...
$ HeartDisease: Factor w/ 2 levels "Fail","Ok": 1 2 1 2 1 1 1 1 2 1 ...
###################################################################

## 3. Splitting the dataset (data slicing).
library(caTools)

set.seed(121521)
train_heart <- df_heart[1:642, 1:6]
nrow( train_heart)
## rows=642 ###########

test_heart <- df_heart[643:918, 1:6]
nrow(test_heart)
## rows=276 ###########

## 4. creating training and testing sets.
train_labels <- df_heart[1:642, 7]

test_labels <- df_heart[643:918, 7]

## 5. applying KNN algorithm to train_cancer and train_result.
library(class)

predictions <- knn(train = train_heart, test = test_heart, cl = train_labels, k=23  )

## 6. evaluating model performance.
table(test_labels, predictions)

## result #### k = 23 #########
            predictions
test_labels Fail  Ok
      Fail  121  26
      Ok     66  63
###############################

round(prop.table(table(test_labels, predictions))*100, digits = 1)

## result #### k = 23 #########
              predictions
test_labels   Fail   Ok
        Fail  43.8  9.4
        Ok    23.9 22.8
###############################

## 13. confusion matrix.
library(caret)

confusionMatrix(test_labels,predictions)

## result ###############################
Confusion Matrix and Statistics

Reference
Prediction Fail  Ok
Fail  121  26
Ok     66  63

Accuracy : 0.6667         
95% CI : (0.6077, 0.722)
No Information Rate : 0.6775         
P-Value [Acc > NIR] : 0.6761         

Kappa : 0.3175         

Mcnemar's Test P-Value : 4.782e-05      
                                         
            Sensitivity : 0.6471         
            Specificity : 0.7079         
         Pos Pred Value : 0.8231         
         Neg Pred Value : 0.4884         
             Prevalence : 0.6775         
         Detection Rate : 0.4384         
   Detection Prevalence : 0.5326         
      Balanced Accuracy : 0.6775         
                                         
       'Positive' Class : Fail'
#########################################

## 14. confusion matrix using gmodels.
library(gmodels)

CrossTable(test_labels, predictions)

## result ############################################
Cell Contents
|-------------------------|
  |                       N |
  | Chi-square contribution |
  |           N / Row Total |
  |           N / Col Total |
  |         N / Table Total |
  |-------------------------|
  
  
  Total Observations in Table:  276 


| predictions 
test_labels |      Fail |        Ok | Row Total | 
  -------------|-----------|-----------|-----------|
  Fail |       121 |        26 |       147 | 
  |     4.599 |     9.663 |           | 
  |     0.823 |     0.177 |     0.533 | 
  |     0.647 |     0.292 |           | 
  |     0.438 |     0.094 |           | 
  -------------|-----------|-----------|-----------|
  Ok |        66 |        63 |       129 | 
  |     5.241 |    11.011 |           | 
  |     0.512 |     0.488 |     0.467 | 
  |     0.353 |     0.708 |           | 
  |     0.239 |     0.228 |           | 
  -------------|-----------|-----------|-----------|
  Column Total |       187 |        89 |       276 | 
  |     0.678 |     0.322 |           | 
  -------------|-----------|-----------|-----------|
#######################################################

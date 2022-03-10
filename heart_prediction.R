rm(list = ls())
ls()

getwd()


## 1. importing the csv files.
library(readr)

heart_dis <- read.csv("heart.csv")
heart_dis
str(heart_dis)

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

df_heart %>% ggplot() + geom_point(mapping = aes(Age, MaxHR, size=Cholesterol, alpha =.5)) 

## 3. transformn HeartDisease from "0"/"1" to "Fail"/"Ok".
df_heart$HeartDisease <- factor(ifelse(df_heart$HeartDisease == 0, "Fail", "Ok" ))
str(df_heart)


## 3. Splitting the dataset (data slicing).
library(caTools)

train_heart <- df_heart[1:642, 1:6]
nrow( train_heart)

test_heart <- df_heart[643:918, 1:6]
nrow(test_heart)

## 4. creating training and testing sets.
train_labels <- df_heart[1:642, 7]

test_labels <- df_heart[643:918, 7]

## 5. applying KNN algorithm to train_cancer and train_result.
library(class)

predictions <- knn(train = train_heart, test = test_heart, cl = train_labels, k=26  )

## 6. evaluating model performance.
table(test_labels, predictions)

## result #### k = 19 #########
            predictions
test_labels Fail  Ok
Fail        102  21
Ok          50  57
###############################

round(prop.table(table(test_labels, predictions))*100, digits = 1)

## result #### k = 19 #########
            predictions
test_labels Fail   Ok
Fail        44.3  9.1
Ok          21.7 24.8
###############################

## 13. confusion matrix.
library(caret)

confusionMatrix(test_labels,predictions)


## 14. confusion matrix using gmodels.
library(gmodels)

CrossTable(test_labels, predictions)

        

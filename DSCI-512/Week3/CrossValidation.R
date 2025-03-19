#clear the memory
rm(list = ls())
#install caret package
install.packages("caret")
#load package caret into memory
library(caret)

library(readr)
mtcars <- read_csv("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/mtcars.csv")
View(mtcars)

#LOOCV

#load the library into memory
library(caret)
# define training control by specifying LOOCV
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(mpg~ hp+wt+am, data=mtcars, trControl=train_control, method="lm")
# summarize results
print(model)

#CV

library(caret)
# define training control by specifying LOOCV
train_control <- trainControl(method="CV", number=10)
# train the model
model <- train(mpg~ hp+wt+am, data=mtcars, trControl=train_control, method="lm")
# summarize results
print(model)


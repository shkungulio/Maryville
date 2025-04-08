#clear memory
rm(list = ls())
#install Random forest package
install.packages("randomForest")
#load the library into memory
library(randomForest)
#load the dataset
library(readr)
Boston <- read_csv("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/Boston.csv")
View(Boston)

#set seed for reproducible
set.seed(1)
#split the data into train set and test set
train = sample(1:nrow(Boston), nrow(Boston)/2)
rf.boston = randomForest(medv ~., data = Boston, subset = train, importance = TRUE)

yhat.rf = predict(rf.boston, newdata = Boston[-train,])
Boston.test = Boston[-train, "medv"]
mean((yhat.rf - Boston.test$medv)^2)
#importance  of variable
importance(rf.boston)
#identify the importance of the variable
varImpPlot(rf.boston)
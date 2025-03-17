#load the data set into memory
library(readr)
Boston <- read_csv("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/Boston.csv")
#view the dataset
View(Boston)
#print out the column names
names(Boston)
#build the model
lm.fit = lm(medv ~ lstat + age, data = Boston)
#summarize the model results
summary(lm.fit)
#condiser interaction term in R using * operator
lm.fit2 = lm(medv ~ lstat*age, data = Boston)
#print out the model results
summary(lm.fit2)
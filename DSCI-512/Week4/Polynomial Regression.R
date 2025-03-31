#clear the memory
rm(list = ls())
#load the dataset
library(readr)
Wage <- read_csv("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/Wage.csv")
View(Wage)
#build nonlinear model to forcast wage using age
lm1 = lm(wage ~ poly(age,3), data = Wage)
#print out the resutlts

summary(lm1)
lm2 = lm(wage ~ poly(age,4), data = Wage)


lm3 = lm(wage ~ poly(age,5), data = Wage)
#select the degree of the polynomial using CV


#CV

library(caret)
# define training control by specifying LOOCV
train_control = trainControl(method="CV", number=10)
# train the model
#set the seed for reproducible results
set.seed(2018)
cv1 = train(wage ~ poly(age,3), data=Wage, trControl=train_control, method="lm")
# summarize results
print(cv1)
cv2 = train(wage ~ poly(age,2), data=Wage, trControl=train_control, method="lm")
print(cv2)
cv3 <- train(wage ~ poly(age,4), data=Wage, trControl=train_control, method="lm")
print(cv3)


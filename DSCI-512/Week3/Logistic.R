#clear the memory
rm(list = ls())

#load the Excel workbook into R
library(readxl)
Smarket <- read_excel("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/Smarket.xlsx")
View(Smarket)
#look at the columns of the data
names(Smarket)
#convert directon to factor
Smarket$Direction = factor(Smarket$Direction)
#Run the logistic regression to forecast Dirction
#uisng predictors:Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
#look at the model results using summary function
summary(glm.fit)

#split the data into training set from year 2001 to 2004
#test set from year 2005
train = Smarket$Year <2005
Smarket.test = Smarket[!train, ]

#build the logistic model using training data  set  and glm() function by specify the formula, data, famil and subset 
glm.fit =glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket,
                       family = binomial, subset = train)
#forcast the market dirction using the model and test dataset by calling the predict () fucunction and specify the model result,
#dataset, and type with response
glm.probs= predict(glm.fit, Smarket.test, type = "response")
#Finally compare the predictions for 2005  to the actual movements of the market in 2005
glm.pred = rep("Down", nrow(Smarket.test))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Smarket.test$Direction)
mean(glm.pred == Smarket.test$Direction)

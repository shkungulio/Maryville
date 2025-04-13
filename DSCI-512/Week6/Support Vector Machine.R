#install package
install.packages("e1071")
#load the library
library(e1071)
#load the data set
library(readr)
mtcars <- read_csv("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/mtcars.csv")
View(mtcars)
#convert am to factor
mtcars$am = factor(mtcars$am)
##split the data into train set and test set
#fix the seed, so everybody has the same results
set.seed(1)
train = sample(1:nrow(mtcars), nrow(mtcars)/2)

#buid a support vector machine to predict am: 0 or 1
#using mpg, cyl, disp, hp, wt
#cost can changes to avoid the overfitting in the training data set.
#the larger the cost, the smaller the training error.
svm.fit = svm(am ~ mpg + cyl+disp+hp+wt, data= mtcars[train, ], kernel = "radial", gamma =1, cost =1)

#print out model results
summary(svm.fit)

#select the parameters uisng   a grid search
#we train many models for the different combination of cost and gamma, and choose the best model

tune.out = tune(svm, am ~ mpg + cyl+disp+hp+wt, data= mtcars[train, ], kernel = "radial", 
                ranges = list(cost = c(0.1,1, 10, 100, 1000), gamma= c(0.5, 1, 2, 3, 4)))
#find the optimal parameters
summary(tune.out)

#forecast the am using test dataset
pred = predict(tune.out$best.model, newdata = mtcars[-train, ])
#get the true observation of am of  the test dataset
trueObservation= mtcars[-train, "am"]
#compute the test error by construct confusion matrix
table(trueObservation$am, pred)
#################################
# Project week 6 solution       #
# Class BDAT 640, Fall 2023     #
# Maryville University          #
# Instructor: Chris Shannon     #
# Email: cshannon@maryville.edu #
# Date: 3 December 2023         #
#################################

# 1. Load the dataset bike.csv. Preview the document  into
#    memory. Convert holiday to a factor using factor() 
#    function. Then split the data into training set 
#    containing 2/3 of the original data (test set 
#    containing remaining 1/3 of the original data).
Bike <- read.csv("Bike.csv")

# Holiday must be converted, otherwise, the support vector
# machine will think it is a continuous variable and
# give a continuous result instead of a classification result.
# Bike$holiday <- factor(Bike$holiday, levels=c(0,1), 
#                        labels=c("non-holiday", "holiday"))
Bike$holiday <- factor(Bike$holiday)

# Nota Bene: if instead of the above line, you typed
# 
#    holiday <- factor(Bike$holiday)
#
# Then what will happen is that when using the Bike dataframe
# as a data source, the column "holiday" in Bike did NOT
# get converted to a factor. What has happened is that 
# a variable named "holiday" has been created in R's default
# namespace, and this "holiday" is NOT the same as the "holiday"
# that is a member of the Bike dataframe. Because svm is going
# to look to Bike for its data, it is going to use the version
# in Bike instead of the new one just created. The svm function
# will then be used as a regression machine instead of a
# classification machine. See the documentation by typing
# ?svm at the command prompt.

# Coding for this is optional because it is already in 0,1 format.
# But it's a good idea to do so.
Bike$workingday <- factor(Bike$workingday, levels=c(0,1), 
                          labels=c("non-workingday","workingday"))

# # Optional to convert weather, since we don't use it in our model.
# Bike$weather <- factor(Bike$weather, levels=c(1,2,3,4),
#                        labels=c("Clear to Partly Cloudy",
#                                 "Misty",
#                                 "Light Precipitation",
#                                 "Heavy Precipitation"))

# This must be converted to a factor, or you have a
# mis-specified model.
Bike$season <- factor(Bike$season, levels=c(1,2,3,4),
                      labels=c("Spring","Summer","Fall","Winter"))

# Split the data here.
set.seed(1) # this is so the exact sample can be repeated.
train <- sample(1:nrow(Bike), nrow(Bike)*2/3)


# 
# 2. Build a support vector machine model.
#    a. The response is holiday and the predictors are: 
#       season, workingday, casual, and registered.
#       Please use svm() function with radial kernel 
#       and gamma=10 and cost = 100.
library(e1071)
f <- formula(holiday ~ season + workingday + casual + registered)

bike.svm <- svm(f,
                kernel="radial",gamma=10, cost=100, 
                data=Bike, subset=train)
summary(bike.svm)

#    b. Perform a grid search to find the best model with 
#       potential cost: 1, 10, 50, 100 and potential 
#       gamma: 1, 3, and 5 and using radial kernel
#       and training dataset.

#       Note that the parameter kernel="radial" does not need
#       to be set, because according to the documentation
#       (type ?svm at the command prompt), the radial kernel
#       is the default if no kernel is set.
bike.tune <- tune(svm, f, data=Bike[train,],
                  ranges=list(
                    cost=c(1,10,50,100),
                    gamma=c(1,3,5),kernel="radial"))

# Note: you  can include the switch 'kernel="radial"' in the
# ranges list. If you do, you cal also specify different kernels
# to try. If you type ?svm at the command prompt, you will display
# the help file for svm, and it will tell you about the different
# kernels you can use. In addition to radial, which is the default
# (i.e. you don't have to even specify it as a parameter), there
# are these other kernels: linear, polynomial, and sigmoid. So,
# if you wanted to compare the performance of different kernels,
# you could do something like this:
#
# bike.tune <- tune(svm, f, data=Bike[train,],
#                   ranges=list(
#                     cost=c(1,10,50,100),
#                     gamma=c(1,3,5),
#                     kernel=c("radial","linear","polynomial")))
#
# So, by placing kernel as a paramter within the grid, you can also
# test different kernels.

#    c. Print out the model results. What's the best model parameters?
# Ans:
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost gamma kernel
# 100     1 radial
# 
# - best performance: 0.02797245 
# 
# - Detailed performance results:
#   cost gamma kernel      error  dispersion
# 1     1     1 radial 0.02879909 0.003578686
# 2    10     1 radial 0.02866154 0.003164643
# 3    50     1 radial 0.02824812 0.003627397
# 4   100     1 radial 0.02797245 0.004158739
# 5     1     3 radial 0.02921269 0.003096545
# 6    10     3 radial 0.02893702 0.003947184
# 7    50     3 radial 0.03003971 0.004246814
# 8   100     3 radial 0.03003952 0.004041429
# 9     1     5 radial 0.02866154 0.003230569
# 10   10     5 radial 0.03045293 0.004517214
# 11   50     5 radial 0.03059048 0.002954565
# 12  100     5 radial 0.03169260 0.002893269
summary(bike.tune)

#    d. Forecast holiday using the test dataset and the 
#       best model found in c).
bike.svm.pred <- predict(bike.tune$best.model, 
                         newdata=Bike[-train,])

#    e. Get the true observations of holiday in the test dataset.
bike.test <- Bike[-train, "holiday"]

#    f. Compute the test error by constructing the confusion matrix.
#       Is it a good model?
#
# Ans: Test error = 1 - accuracy = 1 - 0.9724442 = 0.0275558
#
# BUT: Not a good model. Although it has high accuracy (0.9724),
#      it has low sensitivity (0.0385). In common-sense terms,
#      it got 96% of the holiday predictions wrong, even if
#      if got more than 97% of the non-holiday predictions
#      right. So, that's a bad model.
(t <- table(predicted=bike.svm.pred, actual=bike.test))

#              actual
# predicted     non-holiday holiday
#   non-holiday        3530      90
#   holiday               2       7

# Make sure you know which cells have your true positives (TP).
# true negatives (TN), false positives (FP) and false negatives (FN).
# If you had switched the predicted and actual parameters, your
# FPs and FNs would be in different positions.
#
# For example:
# 
# t2 <- table(actual=bike.test, predicted=bike.svm.pred)
# t2
# 
#              predicted
# actual        non-holiday holiday
#   non-holiday        3530       2
#   holiday              90       7

# Notice how the positions of "2" and "90" have
# changed across the two confusion matrices.
#
# What has changed is that because actual represents
# the rows and predicted now represents the columns,
# The FPs have now moved from the upper right-hand cell
# to the lower left-hand one, and the FNs have moved
# from the lower left-hand cell to the upper right-hand one.
# Basically, your FPs and FNs have switched places, though
# your TPs and TNs have not.


# Accuracy    = (TP + TN)/n    = (3530 + 7)/3629  = 0.9746
# Sensitivity = TP / (FN + TP) = 7/(90 + 7)       = 0.0722
# Specificity = TN / (FP + TN) = 3530/(3530 + 2)  = 0.9994
# Precision   = TP / (TP + FP) = 7/(7 + 2)        = 0.7778
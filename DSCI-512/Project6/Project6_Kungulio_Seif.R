################################################################################
#                                                                              #
# Author:     Seif Kungulio                                                    #
# Date:       04/20/2025                                                       #
# Subject:    Project 6                                                        #
# Class:      DSCI 512                                                         #
# Section:    01W                                                              #
# Instructor: Dr. Nengbing Tao                                                 #
# File Name:  Project6_Kungulio_Seif.R                                         #
#                                                                              #
################################################################################


# DEVELOPMENT ENVIRONMENT PREPARATION
#===============================================================================

# Install necessary packages if not installed
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

# Load necessary libraries
library(e1071)
library(caret)


# SOLUTIONS
#===============================================================================

# 1. Load the dataset bike.csv into memory. Convert holiday to a factor using 
###  factor() function. Then split the data into training set containing 2/3 of 
###  the original data (test set containing remaining 1/3 of the original data).

# Set the working directory
setwd("C:/PROJECTS/Maryville/DSCI-512/Project6")

# Read the dataset into memory
Bike.df <- read.csv("data/Bike.csv")

# Display the dimensions of the data frame (number of rows and columns)
dim(Bike.df)

# Display the column names of the data frame
colnames(Bike.df)

# Display the first six rows of the data frame to understand its structure
head(Bike.df)

# Convert categorical variables to factors
Bike.df$season = factor(Bike.df$season,
                        levels = c(1, 2, 3, 4),
                        labels = c("Spring", "Summer", "Fall", "Winter")
)
Bike.df$holiday <- factor(Bike.df$holiday, 
                          levels = c(0,1), 
                          labels = c("Non-Holiday", "Holiday")
)
Bike.df$workingday <- factor(Bike.df$workingday,
                             levels = c(0,1), 
                             labels = c("Non-Workingday", "Workingday")
)
Bike.df$weather <- factor(Bike.df$weather,
                          levels = c(1, 2, 3, 4),
                          labels = c("Clear", "Misty_cloudy",
                                     "Light_snow", "Heavy_rain")
)

# Set seed for reproducibility
set.seed(1)

# Split the data into a training set (2/3) and test set (1/3)
trainIdx = sample(1:nrow(Bike.df), size = 2/3 * nrow(Bike.df))

# Create a training dataset out of "Bike.df"
trainData = Bike.df[trainIdx, ]

# Create a testing dataset from the remaining dataset of "Bike.df"
testData  <- Bike.df[-trainIdx, ]

# Check the dimension of training dataset
dim(trainData)

# Check the dimension of testing dataset
dim(testData)


# 2. Build a support vector machine model.
#### a) The response is holiday and the predictors are: season, workingday, 
####    casual, and registered. Please use svm() function with radial kernel 
####    and gamma=10 and cost = 100.

svm.model <- svm(holiday ~ season + workingday + casual + registered,
                 data = trainData,
                 kernel = "radial",
                 gamma = 10,
                 cost = 100)
summary(svm.model)


#### b) Perform a grid search to find the best model with potential 
####    cost: 1, 10, 50, 100 and potential gamma: 1, 3, and 5 and using 
####    radial kernel and training dataset.

tuned.results <- tune(svm, holiday ~ season + workingday + casual + registered, 
                     data = trainData, kernel = "radial", 
                     ranges = list(cost = c(1, 10, 50, 100), 
                                   gamma = c(1, 3, 5)
                     ))
summary(tuned.results)


#### c) Print out the model results. What’s the best model parameters?

best_tuned.model <- tuned.results$best.model
summary(best_tuned.model)


#### d) Forecast holiday using the test dataset and the best model found in c).

predictions <- predict(best_tuned.model, newdata = testData)
summary(predictions)


#### e) Get the true observations of holiday in the test dataset.

#observations <- testData$holiday
actuals <- Bike.df[-trainIdx, "holiday"]
summary(actuals)


#### f) Compute the test error by constructing the confusion matrix.
####    Is it a good model?

table(actuals, predictions)
#conf.matrix <- confusionMatrix(observations, predictions)
conf.matrix <- confusionMatrix(predictions, actuals)
print(conf.matrix)


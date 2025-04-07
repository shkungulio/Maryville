################################################################################
#                                                                              #
# Author:     Seif Kungulio                                                    #
# Date:       04/13/2025                                                       #
# Subject:    Project 5                                                        #
# Class:      DSCI 512                                                         #
# Section:    01W                                                              #
# Instructor: Dr. Nengbing Tao                                                 #
# File Name:  Project5_Kungulio_Seif.R                                         #
#                                                                              #
################################################################################


# DEVELOPMENT ENVIRONMENT PREPARATION
#===============================================================================

# Install necessary packages if not installed
if (!requireNamespace("tree", quietly = TRUE)) {
  install.packages("tree")
}
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}

# Load necessary libraries
library(tree)
library(randomForest)


# SOLUTIONS
#===============================================================================

# 1. Load the dataset bike.csv into memory. Then split the data into a training 
###  set containing 2/3 of the original data (test set containing 
###  remaining 1/3 of the original data).

# Set the working directory
setwd("C:/PROJECTS/Maryville/DSCI-512/Project5")

# Read the dataset into memory
Bike.df <- read.csv("data/Bike.csv")

# Display the dimensions of the data frame (number of rows and columns)
dim(Bike.df)

# Display the first six rows of the data frame to understand its structure
head(Bike.df)

# Display the column names of the data frame
colnames(Bike.df)

# Convert categorical variables into factor
Bike.df$season <- factor(Bike.df$season)
Bike.df$weather <- factor(Bike.df$weather)
Bike.df$holiday <- factor(Bike.df$holiday)
Bike.df$workingday <- factor(Bike.df$workingday)

# Split the data into a training set and test set
set.seed(123)
trainData <- sample(1:nrow(Bike.df), 2 * nrow(Bike.df) / 3)
testData  <- (1:nrow(Bike.df))[-trainData]



# 2. Build a tree model using function tree().
#### a). The response is count and the predictors are season, holiday, 
####     workingday, temp, atemp, humidity, windspeed, casual, and registered.

fit <- formula(count ~ season + holiday + workingday + temp +
               atemp + humidity + windspeed + casual + registered)



#### b). Perform cross-validation to choose the best tree by calling cv.tree().


#### c). Plot the model results of b) and determine the best size of the 
####     optimal tree.


#### d). Prune the tree by calling prune.tree() function with the best size 
####     found in c).


#### e). Plot the best tree model.


#### f). Compute the test error using the test data set.





# 3. Build a random forest model using function randomForest()
#### a). The response is count and the predictors are season, holiday, 
####     workingday, temp, atemp, humidity, windspeed, casual, and registered.


#### b). Compute the test error using the test data set.


#### c). Extract variable importance measure using importance() function.


#### d). Plot the variable importance using function varImpPlot(). 
####     Which are the top 2 important predictors in this model?

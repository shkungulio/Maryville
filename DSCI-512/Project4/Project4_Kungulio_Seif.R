#########################################################
#                                                       #
# Author:     Seif Kungulio                             #
# Date:       04/06/2025                                #
# Subject:    Project 4                                 #
# Class:      DSCI 512                                  #
# Section:    01W                                       #
# Instructor: Dr. Nengbing Tao                          #
# File Name:  Project3_Kungulio_Seif.R                  #
#                                                       #
#########################################################


# DEVELOPMENT ENVIRONMENT PREPARATION
#===============================================================================

# Set the working directory
setwd("C:/PROJECTS/Maryville/DSCI-512/Project4")

# Install necessary packages if not installed
if (!requireNamespace("GAM", quietly = TRUE)) {
  install.packages("GAM")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

# Load necessary libraries
library(caret)
library(GAM)


# SOLUTIONS
#===============================================================================

# 1. Read the dataset in Boston.csv into R. Call the loaded data Boston. 
###  Make sure that you have the directory set to the correct location 
###  for the data.

# Read the dataset into memory
Boston <- read.csv("data/Boston.csv")

# Display the data frame dimensions
dim(Boston)

# Display column names of the data frame
colnames(Boston)



# 2. The response is nox and the predictor is dis. Use the poly() function 
###  to fit a cubic polynomial regression to predict nox using dis. 
###  Report the regression output.



# 3. Your assistant data scientist, Tom Johnson, is considering predicting nox 
###  using dis as a predictor. He proposes models from degree 5, degree 4, 
###  and degree 3, and degree 2 polynomial regression. Please perform 
###  cross-validation using caret package to select the optimal degree for 
###  the polynomial and justify your answer.



# 4. Tom just took the DSCI 512. You recommend that he perform the following 
###  GAM analysis.
##### a) Predict nox using a smoothing spline of degree 3 in dis and a smoothing 
#####    spline of degree 2 in medv.


##### b) Predict nox using a smoothing spline of degree 2 in dis and a smoothing 
#####    spline of degree 1 in medv.


##### c) Perform anova analysis. Recommend the best model and justify your answer.

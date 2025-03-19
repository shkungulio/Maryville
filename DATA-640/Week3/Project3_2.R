#########################################################
#                                                       #
# Author:     Seif Kungulio                             #
# date:       11/10/2024                                #
# Subject:    Project 3                                 #
# Class:      DATA 640                                  #
# Section:    01W                                       #
# Instructor: Chris Shannon                             #
# File Name:  Project3_Kungulio_Seif.R                  #
#                                                       #
#########################################################

#clear the memory
rm(list = ls())

setwd("C:/Users/SHKungulio/Desktop/DATA-640 Predictive Models/Week 3")


# Install necessary packages if not installed
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
if (!requireNamespace("bestglm", quietly = TRUE)) {
  install.packages("bestglm")
}
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

# Load necessary libraries
library(readxl)
library(caret)
library(bestglm)
library(MASS)

###########
# Part II #
###########

# Load the dataset
bike.df <- read.csv("Bike.csv")

#
head(bike.df)

# Display dimension of the dataframe
dim(bike.df)

#
colnames(bike.df)

# Convert datetime to Date format if needed
bike.df$datetime <- as.POSIXct(bike.df$datetime, format = "%Y-%m-%d %H:%M:%S")

# 1.	Build a linear model to forecast number of total rentals (count) using
# potential predictors, season, holiday, workingday, weather, atemp, and
# registered. Note here the linear model is not ideal for predicting count. We
# can work around this drawback by rounding up or rounding down the predictions.
# Please read the attached paper for the classical regression models for count
# data in R.

# Convert categorical variables to factors
bike.df$season = factor(bike.df$season,
                        levels = c(1, 2, 3, 4),
                        labels = c("Spring", "Summer", "Fall", "Winter")
)
bike.df$holiday <- factor(bike.df$holiday, 
                          levels = c(0,1), 
                          labels = c("No", "Yes")
)
bike.df$workingday <- factor(bike.df$workingday,
                             levels = c(0,1), 
                             labels = c("No", "Yes")
)
bike.df$weather <- factor(bike.df$weather,
                          levels = c(1, 2, 3, 4),
                          labels = c("Clear", "Misty_cloudy",
                                     "Light_snow", "Heavy_rain")
)

# Linear model for count prediction
linear_model <- lm(count ~ season + holiday + workingday +
                     weather + atemp + registered,
                   data = bike.df)

# Display the statistical summary of the model
summary(linear_model)



# 2.	Perform best subset selection using bestglm() function based on BIC.
# What's the best model based on BIC?

# Prepare data for bestglm (needs to be a dataframe with only predictors
# and response)
#model_data <- bike.df[, c("count", "season", "holiday",
                          #"workingday", "weather", "atemp", "registered")]
model_data <- model.matrix(~ season + holiday + workingday + weather + 
                             atemp + registered + count, data = bike.df)

# Remove the first column from the model_data dataset
model_data <- model_data[,-1]

# Display the first few rows
head(model_data)

# Convert model_data to dataframe
model_data.df <- data.frame(model_data)

# Check to see if model_data.df is a dataframe
class(model_data.df)

# Find the best model based on the BIC.
best_bic_model <- bestglm(model_data.df, IC = "BIC", family = gaussian, TopModels = 5)

# Display the best model based on the BIC
best_bic_model

# Display the statistical summary of the best model of the best_bic_model
summary(best_bic_model$BestModel)


# 3.	Compute the test error of the best model based on BIC using LOOCV.

# Test error using LOOCV
loocv_control <- trainControl(method = "LOOCV")
loocv_model <- train(
  count ~ season + holiday + workingday + weather + atemp + registered,
  data = bike.df,
  method = "lm",
  trControl = loocv_control
)

# Access the Root Mean Squared Error (RMSE) value from the
# results of a Leave-One-Out Cross-Validation (LOOCV) model
loocv_model$results$RMSE

loocv_test_error <- loocv_model$results$RMSE
loocv_test_error


# 4.	Calculate the test error of the best model based on BIC using 10-fold CV.

# Test error using 10-fold CV
cv_control <- trainControl(method = "cv", number = 10)

cv_model <- train(
  count ~ season + holiday + workingday +
    weather + atemp + registered,
  data = bike.df,
  method = "lm",
  trControl = cv_control
)

# Access the Root Mean Squared Error (RMSE) value from the
# results of a Cross-Validation model
cv_model$results$RMSE


# 5.	Perform best subset selection using bestglm() function based on CV.
# What's the best model based on CV?

# Best subset selection based on CV
best_cv_model <- bestglm(
  model_data.df,
  IC = "CV",
  CVArgs = list(Method = "HTF", K = 10),
  family = gaussian
)

# Display the statistical summary of the best model of the best_cv_model
summary(best_cv_model$BestModel)


# 6.	Perform the backward stepwise selection using stepAIC() function. What's
# the best model?

# Backward stepwise selection using stepAIC
stepwise_model <- stepAIC(linear_model, direction = "backward")

summary(stepwise_model)
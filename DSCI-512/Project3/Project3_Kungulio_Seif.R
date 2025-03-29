#########################################################
#                                                       #
# Author:     Seif Kungulio                             #
# Date:       03/30/2025                                #
# Subject:    Project 3                                 #
# Class:      DSCI 512                                  #
# Section:    01W                                       #
# Instructor: Dr. Nengbing Tao                          #
# File Name:  Project3_Kungulio_Seif.R                  #
#                                                       #
#########################################################


# DEVELOPMENT ENVIRONMENT PREPARATION
#===============================================================================

# Set the working directory
setwd("C:/PROJECTS/Maryville/DSCI-512/Project3")

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



# PART I.
#===============================================================================

# 1.	Load the dataset mtcars.xlsx into memory and convert column am to a factor
# using factor() function.
mtcars.df <- read_excel("data/mtcars.xlsx")

#mtcars.df$am <- factor(mtcars.df$am)
mtcars.df$am <- factor(mtcars.df$am,
                       levels = c(0, 1),
                       labels = c("automatic", "manual"))


# 2.	Split the data into training set and test set. The training set contains
# the first 35 observations, the test set containing the remaining observations.

train_dataset <- mtcars.df[1:35, ] # Select the first 35 rows of the dataset
test_dataset <- mtcars.df[-(1:35), ] # Use the remaining rows of the dataset

# View the training set and testing set on another tab
View(train_dataset)
View(test_dataset)


# 3.	Build a logistic regression model with the response is am and the
# predictors are mpg, cyl, hp, and wt using glm() function
model.fit <- glm(am ~ mpg + cyl + hp + wt,
             data = train_dataset,
             family = binomial)

# Display the statistical summary of the model
summary(model.fit)


# 4.	Compute the test error on the test data set using a confusion matrix.
# Is it a good model based on test error?
test_predictions <- predict(model.fit, 
                            newdata = test_dataset, 
                            type = "response")


test_pred_class <- ifelse(test_predictions > 0.5, "manual", "automatic")

# Create the confusion matrix
conf_matrix <- confusionMatrix(factor(test_pred_class), 
                               test_dataset$am)

# Print the confusion matrix
print(conf_matrix)



# Part II.
#===============================================================================

# Load the dataset
bike.df <- read.csv("data/Bike.csv")

# Display first few rows
head(bike.df)

# Display dimension of the dataframe
dim(bike.df)

# Display column names
colnames(bike.df)

# Convert datetime to Date format if needed
bike.df$datetime <- as.POSIXct(bike.df$datetime, format = "%Y-%m-%d %H:%M:%S")


# 1.	Build a linear model to forecast number of total rentals (count) using
# potential predictors, season, holiday, workingday, weather, atemp, and
# registered.

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

# Generate predictions using the model and round the predictions
predictions <- predict(linear_model, bike.df)
rounded_predictions <- round(predictions)

# Show first 15 rounded predictions
head(rounded_predictions, 15)


# 2.	Perform best subset selection using bestglm() function based on BIC.
# What's the best model based on BIC?

# Prepare data for bestglm (needs to be a dataframe with only predictors
# and response)
model_data <- model.matrix(~ season + holiday + workingday + weather + 
                             atemp + registered + count, data = bike.df)

# Remove the first column from the model_data dataset
model_data <- model_data[,-1]

# Convert model_data to dataframe
model_data.df <- data.frame(model_data)

# Find the best model based on the BIC.
best_bic_model <- bestglm(model_data.df, IC = "BIC", 
                          family = gaussian)

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

# Print the loocv_model results
print(loocv_model)

# Access the Root Mean Squared Error (RMSE) value from the
# results of a Leave-One-Out Cross-Validation (LOOCV) model
loocv_model$results$RMSE


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

# Print the results of the cv_model
print(cv_model)

# Access the Root Mean Squared Error (RMSE) value from the
# results of a Cross-Validation model
cv_model$results$RMSE


# 5.	Perform best subset selection using bestglm() function based on CV.
# What's the best model based on CV?

# Best subset selection based on CV
best_cv_model <- bestglm(
  Xy = model_data.df,
  family = gaussian,
  IC = "CV",
  CVArgs = list(Method = "HTF", K = 10, REP = 1)
)

# Print the results of the best_cv_model
#print(best_cv_model)

# Display the statistical summary of the best model of the best_cv_model
summary(best_cv_model$BestModel)


# 6.	Perform the backward stepwise selection using stepAIC() function. What's
# the best model?

# Full model for count prediction
full_model <- lm(count ~ season + holiday + workingday +
                     weather + atemp + registered,
                   data = bike.df)

# Backward stepwise selection using stepAIC
stepwise_model <- stepAIC(full_model, direction = "backward")

# Display the statistical summary of the best model
summary(stepwise_model)


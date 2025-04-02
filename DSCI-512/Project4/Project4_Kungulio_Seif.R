################################################################################
#                                                                              #
# Author:     Seif Kungulio                                                    #
# Date:       04/06/2025                                                       #
# Subject:    Project 4                                                        #
# Class:      DSCI 512                                                         #
# Section:    01W                                                              #
# Instructor: Dr. Nengbing Tao                                                 #
# File Name:  Project4_Kungulio_Seif.R                                         #
#                                                                              #
################################################################################


# DEVELOPMENT ENVIRONMENT PREPARATION
#===============================================================================

# Install necessary packages if not installed
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
if (!requireNamespace("gam", quietly = TRUE)) {
  install.packages("gam")
}

# Load necessary libraries
library(caret)
library(gam)


# SOLUTIONS
#===============================================================================

# 1. Read the dataset in Boston.csv into R. Call the loaded data Boston. 
###  Make sure that you have the directory set to the correct location 
###  for the data.

# Set the working directory
setwd("C:/PROJECTS/Maryville/DSCI-512/Project4")

# Read the dataset into memory
Boston <- read.csv("data/Boston.csv")

# Display the dimensions of the data frame (number of rows and columns)
dim(Boston)

# Display the first six rows of the data frame to understand its structure
head(Boston)

# Display the column names of the data frame
colnames(Boston)



# 2. The response is nox and the predictor is dis. Use the poly() function 
###  to fit a cubic polynomial regression to predict nox using dis. 
###  Report the regression output.

# Fit a cubic polynomial regression model
Boston.fit <- lm(nox ~ poly(dis, 3), data = Boston)

# Display the summary statistics of the fitted model
summary(Boston.fit)



# 3. Your assistant data scientist, Tom Johnson, is considering predicting nox 
###  using dis as a predictor. He proposes models from degree 5, degree 4, 
###  and degree 3, and degree 2 polynomial regression. Please perform 
###  cross-validation using caret package to select the optimal degree for 
###  the polynomial and justify your answer.

# Create a function to predict "nox" using "dis" as a predictor 
# with various degrees
polynomial_cv <- function(data, response, predictor, 
                          degrees = 2:5, seed = 1) {
  
  # Set random seed for reproducibility
  set.seed(seed)
  
  # Initialize an empty data frame to store results
  cv_results <- data.frame(Degree = integer(), RMSE = numeric())
  
  # Loop through different polynomial degrees
  for (degree in degrees) {
    # Define polynomial regression formula dynamically
    fit <- as.formula(paste(response, "~ poly(", predictor, ",", degree, ")"))
    
    # Set up 10-fold cross-validation
    train_control <- trainControl(method = "CV", number = 10)
    
    # Train the polynomial regression model using linear regression
    poly_model <- train(fit, data = data, 
                        method = "lm", trControl = train_control)
    
    # Store the degree and corresponding RMSE in the results dataframe
    cv_results <- rbind(cv_results, 
                        data.frame(Degree = degree, 
                                   RMSE = round(poly_model$results$RMSE, 4)))
  }
  
  # Identify the polynomial degree with the lowest RMSE
  best_degree <- cv_results$Degree[which.min(cv_results$RMSE)]
  
  # Print cross-validation results
  cat("\nDegrees for polynomial regression with corresponding RMSE value \n")
  print(cv_results)
  
  # Return the optimal polynomial degree with the lowest RMSE
  return(cat("\nOptimal degree for polynomial regression is", best_degree,
             "with RMSE value of", min(cv_results$RMSE), "\n"))
}

# Perform cross-validation to find the best polynomial degree
polynomial_cv(data = Boston, response = "nox", predictor = "dis")



# 4. Tom just took the DSCI 512. You recommend that he perform the following 
###  GAM analysis.
##### a) Predict nox using a smoothing spline of degree 3 in dis and a smoothing 
#####    spline of degree 2 in medv.

# Fit the first GAM model
gam1_fit <- gam(nox ~ s(dis, 3) + s(medv, 2), data = Boston)

# Display summary of the first model
summary(gam1_fit)


##### b) Predict nox using a smoothing spline of degree 2 in dis and a smoothing 
#####    spline of degree 1 in medv.

# Fit the second GAM model
gam2_fit <- gam(nox ~ s(dis, 2) + s(medv, 1), data = Boston)

# Display summary of the second model
summary(gam2_fit)


##### c) Perform anova analysis. Recommend the best model and justify your answer.

# Conduct ANOVA to compare model fit using Chi-Square test
anova(gam2_fit, gam1_fit, test = "Chisq")


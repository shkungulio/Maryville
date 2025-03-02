################################################################################
#                                                                              #
# Student:    Seif Kungulio                                                    #
# Date:       03/09/2025                                                       #
# Subject:    Project 8                                                        #
# Class:      DSCI 502                                                         #
# Section:    01W                                                              #
# Instructor: Sean Yang                                                        #
# File Name:  Project8_Kungulio_Seif.R                                         #
#                                                                              #
################################################################################


## 1. Load the dataset in breast_cancer_data.csv into R. Call the loaded data 
##    breast_cancer_data. Make sure that you have the directory set to the 
##    correct location for the data.

# Load necessary libraries
library(ggplot2)  # For visualization
library(dplyr)    # For data manipulation
library(pscl)     # For McFadden's R-squared calculation

# Set the working directory to the correct location for the dataset.
setwd("C:/PROJECTS/Maryville/DSCI 502/Week8")

# Load the data from breast_cancer_data.csv
breast_cancer_data <- read.csv("breast_cancer_data.csv")

# Display the dimensions (rows and columns) of the dataframe
dim(breast_cancer_data) # Shows the number of rows and columns in the dataset.

# Display column names
colnames(breast_cancer_data)

# Displays the structure of the breast_cancer_data object.
str(breast_cancer_data)



## 2. Define a user defined function BoxplotPredictorOnTarget with two 
##    arguments, the target and one predictor to plot the box plot of predictor 
##    based on different category of the target. Then use this user defined 
##    function to generate the box plot:

# Convert Diagnosis to a factor
breast_cancer_data$diagnosis <- factor(breast_cancer_data$diagnosis, 
                                       levels = c("B", "M"), 
                                       labels = c("Benign", "Malignant"))

# Define the user-defined function for boxplot
BoxplotPredictorOnTarget <- function(target, predictor) {
  ggplot(breast_cancer_data, 
         aes_string(x = target, y = predictor, fill = target)) +
    geom_boxplot() + theme_test() +
    labs(title = paste("Boxplot of", predictor, "by", target),
         x = target, y = predictor)
}

##### a) area_mean against diagnosis
# Boxplot of area_mean against diagnosis
BoxplotPredictorOnTarget("diagnosis", "area_mean")

##### b) area_se against diagnosis
# Boxplot of area_se against diagnosis
BoxplotPredictorOnTarget("diagnosis", "area_se")

##### c) texture_mean against diagnosis
# Boxplot of texture_mean against diagnosis
BoxplotPredictorOnTarget("diagnosis", "texture_mean")



## 3. Build the following logistic models to forecast the Diagnosis and 
##    recommend the best model based on McFadden/pseudo R squared to 
##    the management.

##### a) forecast Diagnosis using area_mean
model1 <- glm(diagnosis ~ area_mean, 
              data = breast_cancer_data, 
              family = binomial
              )

##### b) forecast the Diagnosis using area_mean and area_se
model2 <- glm(diagnosis ~ area_mean + area_se, 
              data = breast_cancer_data, 
              family = binomial
              )

##### c) forecast the Diagnosis using area_mean, area_se and texture_mean
model3 <- glm(diagnosis ~ area_mean + area_se + texture_mean, 
              data = breast_cancer_data, 
              family = binomial
              )

##### d) forecast the Diagnosis using area_mean, area_se, texture_mean and 
#####    concavity_worst
model4 <- glm(diagnosis ~ area_mean + area_se + texture_mean + concavity_worst, 
              data = breast_cancer_data, 
              family = binomial
              )

##### e) forecast the Diagnosis using area_mean, area_se, texture_mean, 
#####    concavity_worst and concavity_mean
model5 <- glm(diagnosis ~ area_mean + area_se + texture_mean + 
                concavity_worst + concavity_mean, 
              data = breast_cancer_data, 
              family = binomial
              )

# Function to compute McFadden's R-squared for each model
r_squared <- function(model) {
  1 - (logLik(model)[1] / 
         logLik(glm(diagnosis ~ 1, # Null model with only intercept
                    data = breast_cancer_data,
                    family = binomial))[1])
  }

# Store all models in a list
models <- list(model1, model2, model3, model4, model5)
names(models) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")

# Compute McFadden's R-squared for each model
r_squared_values <- sapply(models, r_squared)

# Display the R-squared values, rounded to 4 decimal places
round(r_squared_values, 4)

# Recommend the best model based on the highest R-squared value
best_model <- names(models)[which.max(r_squared_values)]
paste("The best model based on McFadden's R-squared is", best_model)

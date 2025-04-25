################################################################################
#                                                                              #
# Author:     Seif Kungulio                                                    #
# Date:       05/04/2025                                                       #
# Subject:    Project 8                                                        #
# Class:      DSCI 512                                                         #
# Section:    01W                                                              #
# Instructor: Dr. Nengbing Tao                                                 #
# File Name:  Project8_Kungulio_Seif.R                                         #
#                                                                              #
################################################################################


# DEVELOPMENT ENVIRONMENT PREPARATION
#===============================================================================

# Install necessary packages if not installed
if (!requireNamespace("neuralnet", quietly = TRUE)) {
  install.packages("neuralnet")
}

# Load necessary libraries
library(neuralnet)


# SOLUTIONS
#===============================================================================

# 1. Load the dataset wine.csv into memory.

# Set the working directory
setwd("C:/PROJECTS/Maryville/DSCI-512/Project8")

# Read the dataset into memory
wine <- read.csv("data/wine.csv", header = T)

# Display the dimensions of the data frame (number of rows and columns)
dim(wine)

# Display the column names of the data frame
colnames(wine)


# 2. Preprocess the inputs
###  a. Standardize the inputs using the scale() function.

# scale all columns except the "quality" column
scaled.wine <- scale(wine[, -ncol(wine)])

# Display column names of the standardized inputs
colnames(scaled.wine)


###  b. Convert the standardized inputs to a data frame using the 
###     as.data.frame() function.
standard_data <- as.data.frame(scaled.wine)


###  c. Split the data into a training set containing 3/4 of the original data
###     (test set containing the remaining 1/4 of the original data).

# Add back the quality column
standard_data$quality <- wine$quality

# Split the data into training set and testing set
set.seed(1) # For reproducibility
index <- sample(1:nrow(standard_data), 0.75 * nrow(standard_data))
training_data <- standard_data[index, ]
testing_data <- standard_data[-index, ]

# Display the training dataset dimensions
dim(training_data)

# Display the testing dataset dimensions
dim(testing_data)


# 3. Build a neural networks model
###  a. The response is quality and the inputs are: volatile.acidity, density, 
###     pH, and alcohol. Please use 1 hidden layer with 1 neuron.

# Build the neural network model
nn <- neuralnet(quality ~ volatile.acidity + density + pH + alcohol,
                data = training_data, hidden = c(1))


###  b. Plot the neural networks.
plot(nn)


###  c. Forecast the wine quality in the test dataset.
predicted <- predict(nn, newdata = testing_data)

# Display the statistical summary of the the predicted values
summary(predicted)


###  d. Get the observed wine quality of the test dataset.
actual <- testing_data$quality

# Display the statistical summary of the observed wine quality
summary(actual)


###  e. Compute test error (MSE).
mean_squared_error <- mean((actual - predicted)^2)
mean_squared_error

#########################################################
#                                                       #
# Author:     Seif Kungulio                             #
# Date:       10/31/2024                                #
# Subject:    Project 2                                 #
# Class:      DATA 640                                  #
# Section:    01W                                       #
# Instructor: Chris Shannon                             #
# File Name:  Project2_Kungulio_Seif.R                  #
#                                                       #
#########################################################

# Install necessary packages if not installed
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

#load the data set into memory
library(readxl)

# Set the working directory
setwd("C:/Users/shkungulio/Desktop/DATA-640 Predictive Models/Week_2")

# Load the mtcars dataset
mtcars <- read_excel("mtcars.xlsx", sheet = "mtcars")

# View the dataset
View(mtcars)

# Print out the column names
names(mtcars)

# Build the model
simple_model = lm(mpg ~ hp, data = mtcars)

#print out the model results
summary(simple_model)

# Predict mpg for hp = 100 with confidence interval
predicted_mpg <- predict(simple_model, data.frame(hp = 100), 
                         interval = "confidence", level = 0.95)

# Print the predicted mpg
predicted_mpg

# Plot the response and predictor with the regression line
plot(mtcars$hp, mtcars$mpg, main = "MPG vs Horsepower",
     xlab = "Horsepower (hp)", ylab = "Miles per Gallon (mpg)")
abline(simple_model, lwd = 2, col = "blue")


# Perform multiple linear regression
multiple_model <- lm(mpg ~ cyl + disp + hp + wt + vs + gear, data = mtcars)

# Print the summary of the model
summary(multiple_model)


# Fit a model with interaction effects between hp and wt
interaction_model <- lm(mpg ~ hp * wt, data = mtcars)

# Print the summary of the interaction model
summary(interaction_model)

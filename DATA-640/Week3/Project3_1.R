#clear the memory
rm(list = ls())

setwd("C:/Users/SHKungulio/Desktop/DATA-640 Predictive Models/Week 3")


# Install necessary packages if not installed
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

# Load necessary libraries
library(readxl)
library(caret)

# Load the dataset and convert 'am' column to a factor
mtcars.df <- read_excel("mtcars.xlsx")

# Display the dimension of the dataset
dim(mtcars.df)

# View the full dataset on another tab
View(mtcars.df)

# Convert the 'am' column to a factor
mtcars.df$am <- factor(mtcars.df$am)

# Split the data into training and test sets
train_set <- mtcars.df[1:35, ]
test_set <- mtcars.df[-(1:35), ]

# View the 'train_set' and 'test_set' on another tabs
View(train_set)
View(test_set)

# Build the logistic regression model
model <- glm(am ~ mpg + cyl + hp + wt, data = train_set, family = binomial)

# Predict on the test set and calculate the confusion matrix
test_predictions <- predict(model, newdata = test_set, type = "response")
test_pred_class <- ifelse(test_predictions > 0.5, 1, 0)

# Create the confusion matrix
conf_matrix <- confusionMatrix(factor(test_pred_class), test_set$am)
print(conf_matrix)

# Print test error rate
test_error_rate <- 1 - sum(diag(conf_matrix$table)) / sum(conf_matrix$table)
print(paste("Test Error Rate:", round(test_error_rate, 3)))

# Assess model quality based on test error rate
if (test_error_rate < 0.2) {
  print("The model is reasonably accurate.")
} else {
  print("The model has room for improvement.")
}

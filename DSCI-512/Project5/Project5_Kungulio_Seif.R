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
                           labels = c("No", "Yes")
)
Bike.df$workingday <- factor(Bike.df$workingday,
                              levels = c(0,1), 
                              labels = c("No", "Yes")
)
Bike.df$weather <- factor(Bike.df$weather,
                           levels = c(1, 2, 3, 4),
                           labels = c("Clear", "Misty_cloudy",
                                      "Light_snow", "Heavy_rain")
)

# Set seed for reproducibility
set.seed(123)

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




# 2. Build a tree model using function tree().
#### a). The response is count and the predictors are season, holiday, 
####     workingday, temp, atemp, humidity, windspeed, casual, and registered.
tr.model = tree(
  count ~ season + holiday + workingday + temp +
    atemp + humidity + windspeed + casual + registered,
  data = trainData
)

# Display the statistical summary of the tree
summary(tr.model)


#### b). Perform cross-validation to choose the best tree by calling cv.tree().
CV_Bike = cv.tree(tr.model)


#### c). Plot the model results of b) and determine the best size of the 
####     optimal tree.
plot(CV_Bike$size, CV_Bike$dev, type = "b",
     xlab = "Tree Size",
     ylab = "Deviance",
     main = "Cross Validation Results")


#### d). Prune the tree by calling prune.tree() function with the best size 
####     found in c).
prunedTree = prune.tree(tr.model, best = 4)


#### e). Plot the best tree model.
plot(prunedTree)
text(prunedTree, pretty = 0)


#### f). Compute the test error using the test data set.
tr.predictions = predict(prunedTree, newdata = testData)

#compute and display the MSE
tr.mse <- mean((tr.predictions - testData$count)^2)
tr.mse




# 3. Build a random forest model using function randomForest()
#### a). The response is count and the predictors are season, holiday, 
####     workingday, temp, atemp, humidity, windspeed, casual, and registered.
set.seed(123)  # Set seed for reproducibility
rf.model = randomForest(count ~ season + holiday + workingday + temp + 
                          atemp + humidity + windspeed + casual + registered,
                        data = trainData, importance = TRUE)

# Print the random forest model result
print(rf.model)


#### b). Compute the test error using the test data set.
rf.predictions = predict(rf.model, newdata = testData)

# Calculate the mean squared error
rf.mse <- mean((rf.predictions - testData$count)^2)
rf.mse


#### c). Extract variable importance measure using importance() function.
importance(rf.model)


#### d). Plot the variable importance using function varImpPlot(). 
####     Which are the top 2 important predictors in this model?
varImpPlot(rf.model)


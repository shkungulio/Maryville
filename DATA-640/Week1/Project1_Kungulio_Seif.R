#########################################################
#                                                       #
# Author:     Seif Kungulio                             #
# Date:       10/22/2024                                #
# Subject:    Project 1                                 #
# Class:      DATA 640                                  #
# Section:    01W                                       #
# Instructor: Chris Shannon                             #
# File Name:  Project1_Kungulio_Seif.R                  #
#                                                       #
#########################################################

#== QUESTION No. 1 =============================================================
# Read the dataset in Boston.csv Download Boston.csv into R. 
# Call the loaded data Boston. Make sure that you have the directory set to 
# the correct location for the data.

# Set the working directory
setwd("C:/Users/shkungulio/Desktop/DATA-640 Predictive Models/Week_1")

# Load the Boston dataset
Boston <- read.csv("Boston.csv")


#== QUESTION No. 2 =============================================================
# How many rows are in the data frame? How many columns? What do the rows 
# and columns represent?

# Number of rows and columns
num_rows <- nrow(Boston)
num_cols <- ncol(Boston)
cat("Number of rows:", num_rows, "\n")
cat("Number of columns:", num_cols, "\n")


#== QUESTION No. 3 =============================================================
# Select the 1st, 100th, and 500th rows with columns tax and medv.

# Extract 1st, 100th, and 500th rows with columns 'tax' and 'medv'
selected_rows <- Boston[c(1, 100, 500), c("tax", "medv")]
print(selected_rows)


#== QUESTION No. 4 =============================================================
# Look at the data using cor function. Are any of the predictors associated 
# with per capita crime rate? If so, explain the relationship based on 
# correlation coefficients.

# Correlation analysis to identify associations with crime rate
cor_matrix <- cor(Boston)

print(cor_matrix["crim", ])


#== QUESTION No. 5 =============================================================
# Make some pairwise scatterplots of the predictors, crim, rad, tax, indus, 
# and lstat in this data set. Describe your findings.

# Pairwise scatter plots for selected predictors
pairs(Boston[, c("crim", "rad", "tax", "indus", "lstat")], 
      main = "Pairwise Scatterplots of Selected Predictors")


#== QUESTION No. 6 =============================================================
# Do any of the suburbs of Boston appear to have particularly high crime rates 
# by looking at the histogram of crime? What is the range of crime by 
# using range() function in R?

# Histogram of 'crim'
hist(
  Boston$crim,
  main = "Histogram of Crime Rate (crim)",
  xlab = "Crime Rate",
  labels = TRUE,
  col = "salmon",
  border = "black"
)

# Range of 'crim'
crime_range <- range(Boston$crim)
cat("Range of crime:", crime_range, "\n")


#== QUESTION No. 7 =============================================================
# How many of the suburbs in this dataset bound the Charles River?

# Number of suburbs bordering the Charles River
charles_suburbs <- sum(Boston$chas == 1)
cat("Number of suburbs bordering the Charles River:",
    charles_suburbs, "\n")


#== QUESTION No. 8 =============================================================
# What is the median pupil-teach ratio among the towns in this dataset? 
# What’s the mean?

# Median and mean pupil-teacher ratio
median_ptratio <- median(Boston$ptratio)
mean_ptratio <- mean(Boston$ptratio)
cat("Median pupil-teacher ratio:", median_ptratio, "\n")
cat("Mean pupil-teacher ratio:", mean_ptratio, "\n")


#== QUESTION No. 9 =============================================================
# In this dataset, how many of the suburbs average more than seven rooms 
# per dwelling? More than eight rooms per dwelling? Comment on the suburbs 
# that average more than eight rooms per dwelling.

# Number of suburbs averaging more than 7 and 8 rooms per dwelling
more_than_7_rooms <- sum(Boston$rm > 7)
more_than_8_rooms <- sum(Boston$rm > 8)
cat("Suburbs averaging more than 7 rooms:", more_than_7_rooms, "\n")
cat("Suburbs averaging more than 8 rooms:", more_than_8_rooms, "\n")


#== QUESTION No. 10 ============================================================
# Convert chas to a factor. Boxplot the medv against chas. Are houses around 
# the Charles River more expensive?

# Convert 'chas' to a factor and plot boxplot of 'medv' against 'chas'
Boston$chas <- as.factor(Boston$chas)
boxplot(
  medv ~ chas,
  data = Boston,
  main = "Boxplot of Median Value (medv) vs Charles River (chas)",
  xlab = "Charles River (chas)",
  ylab = "Median Value (medv)",
  col = c("lightblue", "lightgreen")
)
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

# Set the working directory to the correct location for the dataset.
setwd("C:/PROJECTS/Maryville/DSCI 502/Week8")

# Load the data from loan.csv
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

##### a) area_mean against Diagnosis



##### b) area_se against Diagnosis



##### c) texture_mean against Diagnosis




## 3. Build the following logistic models to forecast the Diagnosis and 
##    recommend the best model based on McFadden/pseudo R squared to 
##    the management.

##### a) forecast Diagnosis using area_mean



##### b) forecast the Diagnosis using area_mean and area_se



##### c) forecast the Diagnosis using area_mean, area_se and texture_mean



##### d) forecast the Diagnosis using area_mean, area_se, texture_mean and 
#####    concavity_worst



##### e) forecast the Diagnosis using area_mean, area_se, texture_mean, 
#####    concavity_worst and concavity_mean



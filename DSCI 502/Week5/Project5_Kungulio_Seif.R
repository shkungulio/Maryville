#########################################################
#                                                       #
# Student:    Seif Kungulio                             #
# Date:       02/16/2025                                #
# Subject:    Project 5                                 #
# Class:      DSCI 502                                  #
# Section:    01W                                       #
# Instructor: Sean Yang                                 #
# File Name:  Project5_Kungulio_Seif.R                  #
#                                                       #
#########################################################


## 1. Read the dataset in loan.csv into R. Call the loaded data, loan. 
###   Make sure that you have the directory set to the correct location 
###   for the data.

# Set the working directory to the correct location for the dataset.
setwd("C:/PROJECTS/Maryville/DSCI 502/Week5")

# Import necessary libraries
# (Optional) Load any necessary libraries, e.g., dplyr, tidyr if needed.

# Load the data from loan.csv
loan <- read.csv("loan.csv", stringsAsFactors = TRUE)

# Display the dimensions (rows and columns) of the dataframe
dim(loan) # Shows the number of rows and columns in the dataset.



## 2. Please plot the histogram and density of the loan_amnt using basic 
###   graphics.




## 3. Please plot the histogram and density of the loan_amnt and add the 
###   vertical line denoting the mean using ggplot2.




## 4. Please scatter plot of loan_amnt (y-axis) against annual_inc (x-axis) 
###   and add the trend line using basic graphics.




## 5. Please scatter plot of loan_amnt (y-axis) against annual_inc (x-axis) 
###   and add the trend line using ggplot2.




## 6. Please plot the barplot of term and grade on the same barplot using 
###   basic graphics




## 7. Please plot the barplot of term and grade on the same barplot 
###   using ggplot2




## 8. Please boxplot loan_amnt (y-axis) against term (x-axis) 
###   and save the graph in a file, loanterm.jpg, using basic graphics.




## 9. Please boxplot loan_amnt (y-axis) against term (x-axis) and 
###   save the graph in a file, loanterm.jpg, using ggplot2. 
###   Are there any differences in loan amount with respect to term?




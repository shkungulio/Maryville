#########################################################
#                                                       #
# Student:    Seif Kungulio                             #
# Date:       01/15/2025                                #
# Subject:    Project 1                                 #
# Class:      DSCI 502                                  #
# Section:    01W                                       #
# Instructor: Sean Yang                                 #
# File Name:  Project1_Kungulio_Seif.R                  #
#                                                       #
#########################################################

# 1. Read the dataset in CarInsurances.xlsx into R. Call the loaded data 
#    Insurance. Make sure that you have the directory set to the correct 
#    location for the data.

# Set the working directory
setwd("C:/Projects/DSCI 502/Week 1")

# Import necessary libraries
library(readxl)

# Import the data set
Insurance <- read_excel("CarInsurances.xlsx")

# Display the dimension of the data frame
dim(Insurance)


# 2. How many rows in the data set?

# Display the number of rows
cat("There are", nrow(Insurance), "number of rows\n")


# 3. How many columns in the data set?

# Display the number of columns
cat("There are", ncol(Insurance), "number of columns\n")


# 4. Assign the first eight rows of the data set to a variable: 
#    first.eight.rows and print it out using print() function.

# Assign the first eight rows to first.eight.rows variable
first.eight.rows <- head(Insurance, 8)

# Print the first eight rows
print(first.eight.rows)


# 5. Assign the last five rows of the data set to a variable: 
#    five.rows and print it out using print() function.

# Assign the last five rows to five.rows variable
five.rows <- tail(Insurance, 5)

# Print the last five rows
print(five.rows)


# 6. List all objects in the memory using two methods.

# Use ls() method to list all the objects
ls()

# Use objects() method to list all the objects
objects()


# 7. We want to summarize the data. To do it, we may use the summary function. 
# Before asking others for help, it’s generally a good idea for you to try to 
# help yourself either using help() function or Google it. Please help yourself 
# and summarize the data first. Then answer the following questions:

# Remove the first row which represents the Averages
Insurance <- Insurance[-1, ]

# Display the statistical summary of the "Insurance" data frame
summary(Insurance)

### 7.1. What is the mean of MRC (annual premium of Minimum Required Coverage)?

# Display mean of MRC
round(summary(Insurance$MRC)["Mean"], 1)

### 7.2. What is the mean of FC (annual premium of Full Coverage)?

# Display mean of FC
round(summary(Insurance$FC)["Mean"], 0)

### 7.3. What is the mean of AD (annual premium differences between MRC and FC)?

# Display mean of AD
round(summary(Insurance$AD)["Mean"], 1)
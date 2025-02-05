#########################################################
#                                                       #
# Student:    Seif Kungulio                             #
# Date:       01/29/2025                                #
# Subject:    Project 3                                 #
# Class:      DSCI 502                                  #
# Section:    01W                                       #
# Instructor: Sean Yang                                 #
# File Name:  Project3_Kungulio_Seif.R                  #
#                                                       #
#########################################################

# 1. Read the data set in BlackFriday.xlsx into R. Call the loaded data 
##   BlackFriday.xlxs. Make sure that you have the directory set to the 
##   correct location for the data.

# Set the working directory to the correct location for the dataset.
setwd("C:/Projects/DSCI 502/Week 3")

# Import necessary libraries
library(readxl)

# Load the data from BlackFriday.xlsx
BlackFriday.xlsx <- read_excel("BlackFriday.xlsx")

# Display the dimensions (rows and columns) of the dataframe
dim(BlackFriday.xlsx)

# Verify the structure of the loaded data
str(BlackFriday.xlsx)



# 2. Find the average of purchase amount using for loop.

# Initialize total purchase amount variable
total <- 0

# Get the number of rows in the dataset
n <- nrow(BlackFriday.xlsx)

# Loop through each row and sum the purchase amounts
for (i in 1:n) {
  total <- total + BlackFriday.xlsx$Purchase[i]
}

# Compute the average purchase amount
average_purchase <- total / n

# Print the rounded result
print(round(average_purchase), 2)



# 3. Find the average of purchase amount using while loop.

# Initialize total and counter variables
total <- 0
count <- 1
n <- nrow(BlackFriday.xlsx)

# Loop using while condition until all rows are processed
while (count <= n) {
  total <- total + BlackFriday.xlsx$Purchase[count]
  count <- count + 1
}

# Compute and print the rounded average purchase amount
average_purchase <- total / n
print(round(average_purchase, 2))



# 4. Find the average of purchase amount using repeat loop.

# Initialize total and counter variables
total <- 0
count <- 1
n <- nrow(BlackFriday.xlsx)

# Loop using repeat until manually broken
repeat {
  total <- total + BlackFriday.xlsx$Purchase[count]
  count <- count + 1
  
  # Break when count exceeds the number of rows
  if (count > n) {
    break
  }
}

# Compute and print the rounded average purchase amount
average_purchase <- total / n
print(round(average_purchase, 2))



# 5. Find the average of purchase amount for female shoppers using for loop.

# Extract the subset of data for female shoppers
female_purchases <- BlackFriday.xlsx[BlackFriday.xlsx$Gender == "F", ]

# Initialize total purchase amount variable for females
total <- 0

# Get the number of female shoppers
n <- nrow(female_purchases)

# Loop through female shoppers' purchases and sum them
for (i in 1:n) {
  total <- total + female_purchases$Purchase[i]
}

# Compute and print the rounded average purchase amount for female shoppers
female_avg <- total / n
print(round(female_avg, 2))



# 6. Find the average of purchase amount for female shoppers using while loop.

# Initialize total and counter variables
total <- 0
count <- 1
n <- nrow(female_purchases)

# Loop using while condition until all female shoppers' purchases are summed
while (count <= n) {
  total <- total + female_purchases$Purchase[count]
  count <- count + 1
}

# Compute and print the rounded average purchase amount for female shoppers
female_avg <- total / n
print(round(female_avg, 2))



# 7. Find the average of purchase amount for female shoppers using repeat loop.

# Initialize total and counter variables
total <- 0
count <- 1
n <- nrow(female_purchases)

# Loop using repeat until manually broken
repeat {
  total <- total + female_purchases$Purchase[count]
  count <- count + 1
  
  # Break when count exceeds the number of female shoppers
  if (count > n) {
    break
  }
}

# Compute and print the rounded average purchase amount for female shoppers
female_avg <- total / n
print(round(female_avg, 2))



# 8. Find the differences between the average of purchase amount for female 
##   and male shoppers.

# Extract the subset of data for male shoppers
male_purchases <- BlackFriday.xlsx[BlackFriday.xlsx$Gender == "M", ]

# Initialize total purchase amount variable for males
total <- 0

# Get the number of male shoppers
n <- nrow(male_purchases)

# Loop through male shoppers' purchases and sum them
for (i in 1:n) {
  total <- total + male_purchases$Purchase[i]
}

# Compute and print the rounded average purchase amount for male shoppers
male_avg <- total / n
print(round(male_avg, 2))

# Compute the difference between female and male average purchase amounts
difference <- female_avg - male_avg

# Print the absolute value of the difference (ensuring it's always positive)
print(abs(round(difference, 2)))

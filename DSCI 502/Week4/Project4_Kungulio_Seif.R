#########################################################
#                                                       #
# Student:    Seif Kungulio                             #
# Date:       02/09/2025                                #
# Subject:    Project 4                                 #
# Class:      DSCI 502                                  #
# Section:    01W                                       #
# Instructor: Sean Yang                                 #
# File Name:  Project4_Kungulio_Seif.R                  #
#                                                       #
#########################################################

## 1. Read the dataset in loan.csv into R. Call the loaded data, loan. 
###   Make sure that you have the directory set to the correct location 
###   for the data.

# Set the working directory to the correct location for the dataset.
setwd("C:/PROJECTS/Maryville/DSCI 502/Week4")

# Import necessary libraries
#library(readxl)

# Load the data from loan.csv
loan <- read.csv("loan.csv", stringsAsFactors = TRUE)

# Display the dimensions (rows and columns) of the dataframe
dim(loan)



## 2. Which variables (columns) are continuous/numerical variables? Which 
###   columns are factors (categorical variables)?

# Identify variable types
str(loan)

# Identify continuous (numerical) and categorical (factor) variables
numerical_vars <- sapply(loan, is.numeric)
categorical_vars <- sapply(loan, is.factor)

numerical_columns <- names(numerical_vars[numerical_vars])
cat("Numerical Variables:\n", numerical_columns, "\n\n")

categorical_columns <- names(categorical_vars[categorical_vars])
cat("Categorical Variables:\n", categorical_columns, "\n\n")



## 3. Calculate the minimum, maximum, mean, median, standard deviation and 
###   three quartiles (25th, 50th and 75th percentiles) of loan_amnt.

# 
cat("Minimum of loan_amnt:", min(loan$loan_amnt), "\n")

# 
cat("Maximum of loan_amnt:", max(loan$loan_amnt), "\n")

# 
cat("Mean of loan_amnt:", mean(loan$loan_amnt),"\n")

# 
cat("Median of loan_amnt:", median(loan$loan_amnt), "\n")

# 
cat("Satandard deviation of loan_amnt:", sd(loan$loan_amnt, na.rm = TRUE), "\n")

# 
quantile(loan$loan_amnt, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)



## 4. Calculate the minimum, maximum, mean, median, standard deviation and 
###   three quartiles (25th, 50th and 75th percentiles) of int_rate.

summary(loan$int_rate)

# 
cat("Minimum of int_rate:", summary(loan$int_rate)["Min."], "\n")

# 
cat("Maximum of int_rate:", summary(loan$int_rate)["Max."], "\n")

# 
cat("Mean of int_rate:", summary(loan$int_rate)["Mean"],"\n")

# 
cat("Median of int_rate:", summary(loan$int_rate)["Median"], "\n")

# 
cat("Satandard deviation of int_rate:", sd(loan$int_rate, na.rm = TRUE), "\n")

# 
percentile <- quantile(loan$int_rate, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
percentile

# 
cat("25% of int_rate:", percentile[1], "\n")

# 
cat("50% of int_rate:", percentile["50%"], "\n")

# 
cat("75% of int_rate:", percentile[3], "\n")



## 5. Calculate the correlation coefficient of the two variables: int_rate 
###   and installment. Do they have a strong relationship?

# Correlation coefficient between int_rate and installment
cor(loan$int_rate, loan$installment, use = "complete.obs")



## 6. Calculate the frequency table of term? What’s the mode of term variable?

# Frequency table of term and mode
term_table <- table(loan$term)
mode_term <- names(term_table[term_table == max(term_table)])
print(term_table)
cat("Mode of term:", mode_term, "\n")



## 7. Calculate the proportion table of loan_status? What’s the mode of 
###   loan_status variable?

# Proportion table of loan_status and mode
loan_status_table <- prop.table(table(loan$loan_status))
mode_loan_status <- names(loan_status_table[loan_status_table == 
                                              max(loan_status_table)])
print(loan_status_table)
cat("Mode of loan_status:", mode_loan_status, "\n")



## 8. Calculate the cross table of term and loan_status. Then produce 
###   proportions by row and column respectively.

# Cross table of term and loan_status with proportions
table_term_status <- table(loan$term, loan$loan_status)

# Row proportions
cross_table_row <- prop.table(table_term_status, margin = 1)
print(cross_table_row)

# Column proportions
cross_table_col <- prop.table(table_term_status, margin = 2)
print(cross_table_col)



## 9. The data is stored in the data frame, loan. Please summarize all the 
###   variables using one command.

# Summarize all variables in one command
summary(loan)

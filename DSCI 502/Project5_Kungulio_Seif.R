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
# (Optional) Load any necessary libraries, e.g., dplyr, tidyr if needed.

# Load the data from loan.csv
loan <- read.csv("loan.csv", stringsAsFactors = TRUE)

# Display the dimensions (rows and columns) of the dataframe
dim(loan) # Shows the number of rows and columns in the dataset.


## 2. Which variables (columns) are continuous/numerical variables? Which 
###   columns are factors (categorical variables)?

# Identify variable types
str(loan) # Displays the structure of the dataset including variable types.

# Identify continuous (numerical) and categorical (factor) variables
# Checks which variables are numerical.
numerical_vars <- sapply(loan, is.numeric)

# Checks which variables are categorical.
categorical_vars <- sapply(loan, is.factor)

# Extract the names of numerical variables
numerical_columns <- names(numerical_vars[numerical_vars])
cat("Numerical Variables:\n", numerical_columns, "\n\n")

# Extract the names of categorical variables
categorical_columns <- names(categorical_vars[categorical_vars])
cat("Categorical Variables:\n", categorical_columns, "\n\n")


## 3. Calculate the minimum, maximum, mean, median, standard deviation and 
###   three quartiles (25th, 50th and 75th percentiles) of loan_amnt.

# Calculate and display the minimum value of loan_amnt
cat("Minimum of loan_amnt:", min(loan$loan_amnt, na.rm = TRUE), "\n")

# Calculate and display the maximum value of loan_amnt
cat("Maximum of loan_amnt:", max(loan$loan_amnt, na.rm = TRUE), "\n")

# Calculate and display the mean value of loan_amnt
cat("Mean of loan_amnt:", mean(loan$loan_amnt, na.rm = TRUE), "\n")

# Calculate and display the median value of loan_amnt
cat("Median of loan_amnt:", median(loan$loan_amnt, na.rm = TRUE), "\n")

# Calculate and display the standard deviation of loan_amnt
cat("Standard deviation of loan_amnt:", sd(loan$loan_amnt, na.rm = TRUE), "\n")

# Calculate the quartiles of loan_amnt
percent <- quantile(loan$loan_amnt, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

# Display the 25th percentile of loan_amnt
cat("25% of loan_amnt:", percent[1], "\n")

# Display the 50th percentile of loan_amnt (median)
cat("50% of loan_amnt:", percent[2], "\n")

# Display the 75th percentile of loan_amnt
cat("75% of loan_amnt:", percent[3], "\n")


## 4. Calculate the minimum, maximum, mean, median, standard deviation and 
###   three quartiles (25th, 50th and 75th percentiles) of int_rate.

# Summary statistics for int_rate
summary(loan$int_rate)

# Extract minimum value of int_rate
cat("Minimum of int_rate:", summary(loan$int_rate)["Min."], "\n")

# Extract maximum value of int_rate
cat("Maximum of int_rate:", summary(loan$int_rate)["Max."], "\n")

# Extract mean value of int_rate
cat("Mean of int_rate:", summary(loan$int_rate)["Mean"], "\n")

# Extract median value of int_rate
cat("Median of int_rate:", summary(loan$int_rate)["Median"], "\n")

# Calculate and display the standard deviation of int_rate
cat("Standard deviation of int_rate:", sd(loan$int_rate, na.rm = TRUE), "\n")

# Calculate the quartiles of int_rate
percentile <- quantile(loan$int_rate, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

# Display the 25th percentile of int_rate
cat("25% of int_rate:", percentile["25%"], "\n")

# Display the 50th percentile of int_rate (median)
cat("50% of int_rate:", percentile["50%"], "\n")

# Display the 75th percentile of int_rate
cat("75% of int_rate:", percentile["75%"], "\n")


## 5. Calculate the correlation coefficient of the two variables: int_rate 
###   and installment. Do they have a strong relationship?

# Compute the correlation coefficient between int_rate and installment
correlation_value <- cor(loan$int_rate, loan$installment, use = "complete.obs")

# Display the correlation coefficient
cat("Correlation between int_rate and installment:", correlation_value, "\n")


## 6. Calculate the frequency table of term? What’s the mode of term variable?

# Create a frequency table for the term variable
term_table <- table(loan$term)

# Identify the mode of the term variable
mode_term <- names(term_table[term_table == max(term_table)])

# Print the frequency table
print(term_table)

# Display the mode of the term variable
cat("Mode of term:", mode_term, "\n")


## 7. Calculate the proportion table of loan_status? What’s the mode of 
###   loan_status variable?

# Compute the proportion table for loan_status
loan_status_table <- prop.table(table(loan$loan_status))

# Identify the mode of the loan_status variable
mode_loan_status <- names(loan_status_table[loan_status_table == 
                                              max(loan_status_table)])

# Print the proportion table
print(loan_status_table)

# Display the mode of the loan_status variable
cat("Mode of loan_status:", mode_loan_status, "\n")


## 8. Calculate the cross table of term and loan_status. Then produce 
###   proportions by row and column respectively.

# Compute the cross table of term and loan_status
table_term_status <- table(loan$term, loan$loan_status)

# Compute and print row proportions
cross_table_row <- prop.table(table_term_status, margin = 1)
cat("Row proportions of term and loan_status:\n")
print(cross_table_row)

# Compute and print column proportions
cross_table_col <- prop.table(table_term_status, margin = 2)
cat("Column proportions of term and loan_status:\n")
print(cross_table_col)


## 9. The data is stored in the data frame, loan. Please summarize all the 
###   variables using one command.

# Generate summary statistics for all variables in the dataset
summary(loan)

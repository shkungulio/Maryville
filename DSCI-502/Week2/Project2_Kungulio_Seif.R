#########################################################
#                                                       #
# Student:    Seif Kungulio                             #
# Date:       01/20/2025                                #
# Subject:    Project 2                                 #
# Class:      DSCI 502                                  #
# Section:    01W                                       #
# Instructor: Sean Yang                                 #
# File Name:  Project2_Kungulio_Seif.R                  #
#                                                       #
#########################################################

# 1. Read the dataset in SP500.xls into R. Call the loaded data SP500.
#    Make sure that you have the directory set to the correct location 
#    for the data.

# Set the working directory
setwd("C:/Projects/DSCI 502/Week 2")

# Import necessary libraries
library(readxl)

# Import the data set
SP500 <- read_excel("SP500.xls")

# Display the dimension of the dataframe
dim(SP500)

# Verify that the file was loaded correctly
str(SP500)



# 2. How many rows are in the data set? How many columns are in the data set?

# Display the number of rows
cat("There are", nrow(SP500), "number of rows\n")

# Display the number of columns
cat("There are", ncol(SP500), "number of columns\n")

  
  
# 3. Select the following three columns: SP500, CPI, and Rate.

# Create a variable to hold the selected three columns
three_cols.df <- SP500[,c("SP500", "CPI", "Rate")]

# Print the 'three_cols.df' dataframe showing the first six rows
head(three_cols.df)



# 4. Select the 10th, 100th, 500th, and 1500th rows.

# Create a variable to hold the selected four rows
four_rows.df <- SP500[c(10, 100, 500, 1500), ]

# Print the 'four_rows.df' dataframe
print(four_rows.df)



# 5. Select all the observations such that SP500 is greater than 2000 or 
#    CPI is less than 100.

# Filter data according the prescribed condition
obs1.df <- SP500[(SP500$SP500 > 2000) | (SP500$CPI < 100), ]

dim(obs1.df) # Check the dimension of the resultant dataframe
head(obs1.df) # Display the first six rows of the dataframe
tail(obs1.df) # Display the last six rows of the dataframe



# 6. Select the data such that Earnings greater than 50 and Rate 
#    less than 3 with columns SP500 and Dividend only.

# Filter data according the prescribed condition
obs2.df <- SP500[(SP500$Earnings > 50) & (SP500$Rate < 3), ]
#View(obs2.df) # View the dataframe in another tab

# Filter to return the required columns only
obs2.df <- obs2.df[, c("SP500", "Dividend")]

dim(obs2.df) # Check the dimension of the resultant dataframe
head(obs2.df) # Display the first six rows of the dataframe



# 7. Remove the entire column of Rate.

# Create a variable to hold the dataframe 
without_rate.df <- SP500[, -6]

# Display the first six rows
head(without_rate.df)



# 8. Real price is the inflation adjusted price, which is given by 
#    the following formula:
#    Real Price at time t = (SP 500 price at time t)* CPI(t)/CPI(2018.09), 
#    where CPI(2018.09) is the latest consumer price index in the data set. 
#    Based on formula above, you need to add one more column, RealPrice.

# Extract the latest CPI using tail() function
latest_CPI <- tail(SP500$CPI, n = 1)
#print(latest_CPI)

# Calculate Real Price and assign to RealPrice column
SP500$RealPrice <- SP500$SP500 * SP500$CPI / latest_CPI

head(SP500) # Display the first six rows of the dataframe



# 9. Real earnings are the inflation adjusted earnings, which is given by 
#    the following formula:
#    Real earnings at time t = (earnings at time t)* CPI(t)/CPI(2018.09), 
#    where CPI(2018.09) is the latest consumer price index in the data set. 
#    Based on formula above, you need to add one more column, RealEarnings.

# Calculate Real Earnings and assign to RealEarnings column
SP500$RealEarnings <- SP500$Earnings * SP500$CPI / latest_CPI

head(SP500) # Display the first six rows of the dataframe



# 10. Price to earnings ratio is given by the following formula:
#     P/E Ratio = RealPrice/RealEarnings
#     Based on the formula above, please add one more column, PERatio.

# Calculate P/E Ratio and assign to PERatio column
SP500$PERatio <- SP500$RealPrice / SP500$RealEarnings

head(SP500) # Display the first six rows of the dataframe

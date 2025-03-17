#########################################################
#                                                       #
# Author:     Seif Kungulio                             #
# Date:       03/16/2025                                #
# Subject:    Project 0                                 #
# Class:      DSCI 512                                  #
# Section:    01W                                       #
# Instructor: Dr. Nengbing Tao                          #
# File Name:  Project0_Kungulio_Seif.R                  #
#                                                       #
#########################################################


# 1. Read the dataset in titanic2.csv into R. Call the loaded data titanic. 
#    Make sure that you have the directory set to the correct location for 
#    the data.

# Set the working directory
setwd("C:/PROJECTS/Maryville/DSCI-512/Week0")

# Load the Boston dataset
titanic <- read.csv("titanic2.csv")

# View the first few rows
head(titanic)

# Get the number of rows and columns
dim(titanic)  # This returns (rows, columns)


# 2. How many rows are in the data frame? How many columns? What do the rows 
#    and columns represent?

#### a) Number of rows in the data frame
num_rows <- nrow(titanic)
print(num_rows)

#### b) Number of columns in the data frame
num_columns <- ncol(titanic)
print(num_columns)


# 3. Select the 1st, 5th, and 10th rows with columns Class and Age.

# Selecting 1st, 5th, and 10th rows with columns "Class" and "Age"
selected_rows <- titanic[c(1, 5, 10), c("Class", "Age")]
print(selected_rows)


# 4. Regress Survived and Died on the predictors Class, Sex and Age using 
#    logistic regression with the two-column form for the dependent variables.
#    Are any of the predictors associated with survival? If so, explain the 
#    relationship based on the t-statistics. Explain the chances of survival 
#    in terms of odds, giving the precise numbers and giving in terms a 
#    non-expert can understand.

# Load necessary package
library(MASS)

# Perform logistic regression
logit_model <- glm(cbind(Survived, Died) ~ Class + Sex + Age, 
                   data = titanic, 
                   family = binomial)

# Summary of the model
summary(logit_model)


# 5. What are the odds of a male 3rd-class child surviving the Titanic?
#    Show the precise odds and explain in a way a non-expert can understand.

# Extract coefficients
coefs <- coef(logit_model)

# Compute the odds for a male 3rd-class child (Age < 18)
odds_male_3rd_child <- exp(coefs["(Intercept)"] + 
                             coefs["Class3rd"] + 
                             coefs["SexMale"] + 
                             coefs["AgeChild"])

odds_male_3rd_child



# 6. Show a histogram of the Chi-Square distribution with 8 (upper) and 
#    13 (lower) degrees of freedom. In the graphic, draw a horizontal 
#    dotted line to show the critical test value (0.05 significance level). 
#    Make sure your graphic is properly titled and labeled.

# Load ggplot2 for visualization
library(ggplot2)

# Create sequence for chi-square distribution
x_vals <- seq(0, 30, length=100)

# Compute density
chi_8 <- dchisq(x_vals, df=8)
chi_13 <- dchisq(x_vals, df=13)

# Find critical value at 0.05 significance level
crit_value <- qchisq(0.95, df=8)

# Create the plot
ggplot(data.frame(x=x_vals, y=chi_8), aes(x, y)) +
  geom_line(color="blue", size=1) +
  geom_line(data=data.frame(x=x_vals, y=chi_13), aes(x, y), color="red", size=1) +
  geom_hline(yintercept=dchisq(crit_value, df=8), linetype="dotted") +
  labs(title="Chi-Square Distribution (8 & 13 df)",
       x="Chi-Square Value", 
       y="Density") +
  theme_test()



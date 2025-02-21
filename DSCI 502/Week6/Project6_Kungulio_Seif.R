################################################################################
#                                                                              #
# Student:    Seif Kungulio                                                    #
# Date:       02/23/2025                                                       #
# Subject:    Project 6                                                        #
# Class:      DSCI 502                                                         #
# Section:    01W                                                              #
# Instructor: Sean Yang                                                        #
# File Name:  Project6_Kungulio_Seif.R                                         #
#                                                                              #
################################################################################


## 1. Load the dataset in kc_house_data.csv into R. Call the loaded data 
##    kc_house_data. Make sure that you have the directory set to the correct 
##    location for the data.

# Set the working directory to the correct location for the dataset.
setwd("C:/PROJECTS/Maryville/DSCI 502/Week6")

# Import necessary libraries
library(ggplot2)

# Load the data from loan.csv
# kc_house_data <- read.csv("kc_house_data.csv", stringsAsFactors = TRUE)
kc_house_data <- read.csv("kc_house_data.csv")

# Display the dimensions (rows and columns) of the dataframe
dim(kc_house_data) # Shows the number of rows and columns in the dataset.

# Displays the structure of the kc_house_data object.
str(kc_house_data)

# Display the statistical summary of the kc_house_data object.
summary(kc_house_data)



## 2. Build a linear model to forecast the price using bedrooms, bathrooms, 
##    and sqft_living.

# Linear model to forecast the price
qn2.fit <- lm(price ~ bedrooms + bathrooms + sqft_living, data = kc_house_data)

#
summary(qn2.fit)

##### a. Then write down the corresponding math formula.

# Math formula: 
# price = b0 + b1*bedrooms + b2*bathrooms + b3*sqft_living

##### b. Is it a good model based on R square or adjusted R square?

summary(qn2.fit)$adj.r.squared

summary(qn2.fit)$r.squared



## 3. Build a linear model to forecast the price using bedrooms, bathrooms, 
##    sqft_living, and all the cross effects between them.

# Linear model to forecast the price
qn3.fit <- lm(price ~ bedrooms * bathrooms * sqft_living, data = kc_house_data)

#
summary(qn3.fit)

##### a. Then write down the corresponding math formula.

# Math formula: price = b0 + b1*bedrooms + b2*bathrooms + b3*sqft_living + 
#                       b4*(bedrooms*bathrooms) + b5*(bedrooms*sqft_living) + 
#                       b6*(bathrooms*sqft_living) + 
#                       b7*(bedrooms*bathrooms*sqft_living)

##### b. Is it a better model than the model in Question 2 based on 
#####    adjusted R square?

# Identify the model with the highest adjusted R-squared
adjusted_r_squared_values <- c(
  summary(qn2.fit)$adj.r.squared, summary(qn3.fit)$adj.r.squared
)

# 
best_model_index <- which(adjusted_r_squared_values == 
                            max(adjusted_r_squared_values))
best_model_index

## 4. Build a linear model to forecast the price using bedrooms, bathrooms, 
##    sqft_living, waterfront, and grade.

# 
qn4.fit <- lm(price ~ bedrooms + bathrooms + sqft_living + waterfront + grade, 
              data = kc_house_data)

# 
summary(qn4.fit)


##### a. Then write down the corresponding math formula.

# Math formula: 
# price = b0 + b1*bedrooms + b2*bathrooms + b3*sqft_living + 
#         b4*waterfront + b5*grade


##### b. Is it a better model than the model in Question 3 based on 
#####    adjusted R square?



## 5. Build a linear model to forecast the price using all other columns except 
##    id, date, zipcode, lat, and long without a y-intercept. 
##    If we only consider the models defined in Q2, Q3, Q4 and Q5, 
##    which model do you recommend based on the adjusted R squared value?

# 
qn5.fit <- lm(price ~ . -id -date -zipcode -lat -long -1, data = kc_house_data)

# 
summary(qn5.fit)


###############################################################

models <- list(qn2.fit, qn3.fit, qn4.fit, qn5.fit)


adjusted_r_squared_values <- numeric(length(models))

for (i in seq_along(models)) {
  adjusted_r_squared_values[i] <- summary(models[[i]])$adj.r.squared
}

best_model_index <- which(adjusted_r_squared_values == 
                            max(adjusted_r_squared_values))

best_model_index

best_model <- models[[best_model_index]]

best_model










## 6. You are asked to build a linear model to forecast price using bedrooms, 
##    bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, 
#     and grade. Then you are given the flowing new house info:

# | bedrooms | bathrooms | sqft_living | sqft_lot | floors | waterfront | view | condition | grade |
# |:---------|:----------|:------------|:---------|:-------|:-----------|:-----|:----------|:------|
# | 4        | 2         | 2560        | 7650     | 1.5    | 1          | 3    | 5         | 10    |

# Linear model to forecast price
qn6.fit <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + 
                waterfront + view + condition + grade, data = kc_house_data)

# New house prediction
new_house <- data.frame(bedrooms = 4,
                        bathrooms = 2,
                        sqft_living = 2560,
                        sqft_lot = 7650,
                        floors = 1.5,
                        waterfront = 1,
                        view = 3,
                        condition = 5,
                        grade = 10
                        )







##### a. Predict the average sales price for this house.

# Predict average price
predicted_price <- predict(qn6.fit, newdata = new_house)
predicted_price


##### b. Predict the 95% predicted interval for this house.

# Predict 95% prediction interval
prediction_interval <- predict(qn6.fit, newdata = new_house, 
                               interval = "prediction")
prediction_interval


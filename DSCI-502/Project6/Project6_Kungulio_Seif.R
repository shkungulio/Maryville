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
setwd("C:/PROJECTS/Maryville/DSCI 502/Project6")

# Load the data from loan.csv
kc_house_data <- read.csv("kc_house_data.csv")

# Display the dimensions (rows and columns) of the dataframe
dim(kc_house_data) # Shows the number of rows and columns in the dataset.

# Display column names
colnames(kc_house_data)

# Displays the structure of the kc_house_data object.
str(kc_house_data)



## 2. Build a linear model to forecast the price using bedrooms, bathrooms, 
##    and sqft_living.

# Convert the following features to factor
features <- c("waterfront", "floors", "view", "condition")

# Loop over each feature and convert it to a factor
for (feature in features) {
  kc_house_data[[feature]] <- as.factor(kc_house_data[[feature]])
}

# Build a basic multiple linear regression model
qn2.fit <- lm(price ~ bedrooms + bathrooms + sqft_living, data = kc_house_data)

# Display statistical summary of the model
summary(qn2.fit)

##### a. Then write down the corresponding math formula.

# Math formula: 
# price = b0 + b1*bedrooms + b2*bathrooms + b3*sqft_living


##### b. Is it a good model based on R square or adjusted R square?

# Retrieve R-squared
summary(qn2.fit)$r.squared



## 3. Build a linear model to forecast the price using bedrooms, bathrooms, 
##    sqft_living, and all the cross effects between them.

# Build a model with interaction effects
qn3.fit <- lm(price ~ bedrooms * bathrooms * sqft_living, data = kc_house_data)

# Display statistical summary of the model
summary(qn3.fit)

##### a. Then write down the corresponding math formula.

# Math formula: price = b0 + b1*bedrooms + b2*bathrooms + b3*sqft_living + 
#                       b4*(bedrooms*bathrooms) + b5*(bedrooms*sqft_living) + 
#                       b6*(bathrooms*sqft_living) + 
#                       b7*(bedrooms*bathrooms*sqft_living)


##### b. Is it a better model than the model in Question 2 based on 
#####    adjusted R square?

# Compares adjusted R-squared values to determine which 
# model (Q2 or Q3) performs better.
adjusted_r_squared_values <- c(
  summary(qn2.fit)$adj.r.squared, summary(qn3.fit)$adj.r.squared
)

best_model_index <- which(adjusted_r_squared_values == 
                            max(adjusted_r_squared_values))

# Display the best model index
best_model_index



## 4. Build a linear model to forecast the price using bedrooms, bathrooms, 
##    sqft_living, waterfront, and grade.

# Build a basic multiple linear regression model by adding more predictors
qn4.fit <- lm(price ~ bedrooms + bathrooms + sqft_living + waterfront + grade, 
              data = kc_house_data)

# Display statistical summary of the model
summary(qn4.fit)

##### a. Then write down the corresponding math formula.

# Math formula: 
# price = b0 + b1*bedrooms + b2*bathrooms + b3*sqft_living + 
#         b4*waterfront + b5*grade


##### b. Is it a better model than the model in Question 3 based on 
#####    adjusted R square?

# Better model recommendation
models <- list(qn3.fit = qn3.fit, qn4.fit = qn4.fit)

# Iterates through models and extracts adjusted R-squared values for comparison
for (i in seq_along(models)) {
  adjusted_r_squared_values[i] <- summary(models[[i]])$adj.r.squared
}
best_model_index <- which(adjusted_r_squared_values == 
                            max(adjusted_r_squared_values))

# Identifies and prints the best model based on adjusted R-squared.
cat("Best Model is", names(models)[best_model_index],
    "with Adjusted R-Squared of", 
    summary(models[[best_model_index]])$adj.r.squared)



## 5. Build a linear model to forecast the price using all other columns except 
##    id, date, zipcode, lat, and long without a y-intercept. 
##    If we only consider the models defined in Q2, Q3, Q4 and Q5, 
##    which model do you recommend based on the adjusted R squared value?

# Uses all variables except id, date, zipcode, lat, and long, ensuring 
# better prediction by removing non-informative or redundant variables.
qn5.fit <- lm(price ~ . -id -date -zipcode -lat -long -1, data = kc_house_data)

# Display statistical summary of the model
summary(qn5.fit)

# Model recommendation
models <- setNames(
  list(qn2.fit, qn3.fit, qn4.fit, qn5.fit),
  c("qn2.fit", "qn3.fit", "qn4.fit", "qn5.fit")
)

# Iterates through models and extracts adjusted R-squared values for comparison
for (i in seq_along(models)) {
  adjusted_r_squared_values[i] <- summary(models[[i]])$adj.r.squared
}
best_model_index <- which(adjusted_r_squared_values == 
                            max(adjusted_r_squared_values))

# Identifies and prints the best model based on adjusted R-squared.
cat("Best Model is", names(models)[best_model_index],
    "with Adjusted R-Squared of", 
    summary(models[[best_model_index]])$adj.r.squared)



## 6. You are asked to build a linear model to forecast price using bedrooms, 
##    bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, 
#     and grade. Then you are given the flowing new house info:

# | bedrooms | bathrooms | sqft_living | sqft_lot | floors | waterfront | view | condition | grade |
# |----------|-----------|-------------|----------|--------|------------|------|-----------|-------|
# | 4        | 2         | 2560        | 7650     | 1.5    | 1          | 3    | 5         | 10    |

# Linear model to forecast price
qn6.fit <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + 
                waterfront + view + condition + grade, data = kc_house_data)

# Statistical summary of the model
summary(qn6.fit)

##### a. Predict the average sales price for this house.

# New house information
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

# Convert the following features to factor
features <- c("waterfront", "floors", "view", "condition")

# Loop over each feature and convert it to a factor
for (feature in features) {
  new_house[[feature]] <- as.factor(new_house[[feature]])
}

# Predict and display the average price
predicted_price <- predict(qn6.fit, newdata = new_house)
cat("The average price for the new house is", predicted_price)


##### b. Predict the 95% predicted interval for this house.

# Provides a 95% prediction interval
prediction_interval <- predict(qn6.fit, newdata = new_house, 
                               interval = "prediction")
# Display the predicted intervals
prediction_interval

cat(
  "\nThe predicted price for the new house:", 
  prediction_interval[1],
  "\nThe lower end price of the 95% prediction interval:", 
  prediction_interval[2],
  "\nThe upper end price of the 95% prediction interval:", 
  prediction_interval[3]
  )

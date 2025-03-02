################################################################################
#                                                                              #
# Student:    Seif Kungulio                                                    #
# Date:       03/09/2025                                                       #
# Subject:    Final Project                                                    #
# Class:      DSCI 502                                                         #
# Section:    01W                                                              #
# Instructor: Sean Yang                                                        #
# File Name:  ProjectFinal_Kungulio_Seif.R                                     #
#                                                                              #
################################################################################


## 1. Load the dataset day.csv  Download day.csvinto memory.

# Set the working directory to the correct location for the dataset.
setwd("C:/PROJECTS/Maryville/DSCI 502/Week8")

# Import necessary libraries
library(dplyr)
library(ggplot2)
library(MASS)
#library(knitr)

# Load the data from loan.csv
Bikes.df <- read.csv("day.csv")

# Display the dimensions (rows and columns) of the dataframe
dim(Bikes.df) # Shows the number of rows and columns in the dataset.

# Display column names of the data frame
colnames(Bikes.df)

# Display the structure of the data frame
str(Bikes.df)



## 2. Perform the following data preparations using control structures:
##### a. Convert numerical season (1,2,3, 4) to characters (springer, summer, 
#####    fall and winter)

# Convert season to characters
Bikes.df$season <- factor(Bikes.df$season,
                          levels = c(1, 2, 3, 4),
                          labels = c("spring", "summer", "fall", "winter"))

# Check if the variable was converted to factor
str(Bikes.df$season)


##### b. Convert numerical weathersit (1,2,3,4) to characters (Good, Mist, Bad, 
#####    Severe)

# Convert weathersit to characters
Bikes.df$weathersit <- factor(Bikes.df$weathersit,
                              levels = c(1, 2, 3, 4),
                              labels = c("Good", "Mist", "Bad", "Severe"))

# Check the structure to see if the variable was converted to factor
str(Bikes.df$weathersit)



## 3. Consider the following predictors, season, holiday, workingday, 
##    weathersit, atemp, hum, windspeed, casual and List all categorical 
##    variables from this list and convert them to factors

# Convert categorical variables to factors
categorical_vars <- c("season", "holiday", "workingday", "weathersit", "casual")
Bikes.df[categorical_vars] <- lapply(Bikes.df[categorical_vars], as.factor)



## 4. Calculate the minimum, maximum, mean, median, standard deviation and 
##    three quartiles (25th, 50th and 75th percentiles) of cnt.

# Summary Statistics for cnt
cnt_stats <- Bikes.df %>%
  summarise(minimum = min(cnt), maximum = max(cnt), mean = mean(cnt), 
            median = median(cnt), sd = sd(cnt), Q1 = quantile(cnt, 0.25), 
            Q2 = quantile(cnt, 0.5), Q3 = quantile(cnt, 0.75))

#
kable(cnt_stats)



## 5. Calculate the minimum, maximum, mean, median, standard deviation and 
##    three quartiles (25th, 50th and 75th percentiles) of registered.

# Summary Statistics for registered
registered_stats <- Bikes.df %>%
  summarise(minimum = min(registered), maximum = max(registered), 
            mean = mean(registered), median = median(registered),
            sd = sd(registered), Q1 = quantile(registered, 0.25), 
            Q2 = quantile(registered, 0.5), Q3 = quantile(registered, 0.75))

#
kable(registered_stats)



## 6. Calculate the correlation coefficient of the two variables: registered 
##    and cnt. Do they have a strong relationship?

# Correlation between registered and cnt
correlation <- cor(Bikes.df$registered, Bikes.df$cnt)
print(correlation)



## 7. Calculate the frequency table of season? What’s the mode of 
##    season variable?

# Frequency table of season
season_freq <- table(Bikes.df$season)
print(season_freq)

mode_season <- names(which.max(season_freq))
print(paste("Mode of season: ", mode_season))



## 8. Calculate the cross table of season and weathersit, then produce 
##    proportions by rows and columns respectively.

# Cross table of season and weathersit
season_weather_table <- table(Bikes.df$season, Bikes.df$weathersit)
season_weather_row_prop <- prop.table(season_weather_table, 1)
season_weather_col_prop <- prop.table(season_weather_table, 2)

#
kable(season_weather_table)

#
kable(season_weather_row_prop)

#
kable(season_weather_col_prop)



## 9. Please plot the histogram and density of the cnt and add the vertical 
##    line denoting the mean using ggplot2.

# Histogram and Density Plot of cnt
ggplot(Bikes.df, aes(x = cnt)) +
  geom_histogram(binwidth = 500, fill = "green") +
  geom_density(color = "blue") +
  geom_vline(aes(xintercept = mean(cnt)), color = "red", linetype = "dashed") +
  theme_test()



## 10. Please scatter plot of cnt (y-axis) against registered (x-axis) and add 
##     the trend line using ggplot2.

# Scatter plot of cnt vs registered
scatter_plot <- ggplot(Bikes.df, aes(x = registered, y = cnt)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  theme_test()
print(scatter_plot)



## 11. Please plot the barplot of season and weathersit on the same barplot 
##     using ggplot2

# Barplot of season and weathersit
ggplot(Bikes.df, aes(x = season, fill = weathersit)) +
  #geom_bar() +
  geom_bar(position = "dodge") +
  scale_fill_manual(values=c("#008000", "#0000FF", "#FF0000")) +
  ggtitle("Barplot of season and weathersit") +
  theme_test()



## 12. Please boxplot cnt (y-axis) against weathersit (x-axis) and save the 
##     graph in a file, cntweather.jpg, using ggplot2. Are there any differences
##     in cnt with respect to weathersit?

# Boxplot cnt vs weathersit
cnt_weather_boxplot <- ggplot(Bikes.df, 
                              aes(x = weathersit, y = cnt, fill = weathersit)) +
  geom_boxplot() + theme_test() +
  scale_fill_manual(values=c("#008000", "#0000FF", "#FF0000")) +
  ggtitle("Boxplot cnt vs weathersit")

# Plot the boxplot graph
plot(cnt_weather_boxplot)

# Save the graph in a file
ggsave("cntweather.jpg")



## 13. Build the following multiple linear regression models:
###### a. Perform multiple linear regression with cnt as the response and the 
######    predictors are: season, weathersit, atemp, and registered. 
######    Write down the math formula with numerical coefficients for 
######    predictors atemp and registered and skip the coefficients 
######    for season and weathersit.

# Model 1
model1 <- lm(cnt ~ season + weathersit + atemp + 
               registered, data = Bikes.df)
summary(model1)

###### b. Preform multiple linear regression with cnt as the response and the 
######    predictors are: season, workingday, weathersit, atemp, and 
######    registered. Write down the math formula with numerical 
######    coefficients for predictors atemp and registered and skip the 
######    coefficients for season, workingday, and weathersit.

# Model 2
model2 <- lm(cnt ~ season + workingday + weathersit + 
               atemp + registered, data = Bikes.df)
summary(model2)


###### c. Preform multiple linear regression with cnt as the response and the 
######    predictors are: season, holiday, workingday, weathersit, atemp, hum, 
######    windspeed, and registered. Write down the math formula with numerical 
######    coefficients for predictors atemp, hum, windspeed, and registered 
######    and skip the coefficients for season, holiday, 
######    workingday and weathersit.

# Model 3
model3 <- lm(cnt ~ season + holiday + workingday + weathersit + 
               atemp + hum + windspeed + registered, data = Bikes.df)
summary(model3)

###### d. Which model do you recommend to the management based on 
######    adjusted R squared? Justify your answer.

## Choosing the best model
adjusted_r_squared <- c(summary(model1)$adj.r.squared, 
                        summary(model2)$adj.r.squared, 
                        summary(model3)$adj.r.squared)

best_model <- which.max(adjusted_r_squared)
paste("Best model based on Adjusted R-squared: Model", best_model)



################################################################################
## For Question 14, see the ProjectFinal_Kungulio_Seif.Rmd file                #
################################################################################

## 14. Summarize Question 13-C using R markdown to generate a reproducible 
##     report.  Include the following scripts in your R markdown file:
###### A. Load the data as specified in Question 1.

###### B. Convert the two variables as specified in Question 2.

###### C. Convert the categorical variables to factors as specified in 
######    Question 3

###### D. Build a linear model as specified in Question 13-C. Use R markdown 
######    to report the math formula with numerical coefficients for predictors
######    atemp, hum, windspeed, and registered. Skip the coefficients for 
######    season, holiday, workingday and  weathersit.

################################################################################


## 15. Build the following logistic models:
###### a. forecast holiday using cnt, season, and registered.



###### b. forecast the holiday using cnt, season, weathersit, and registered



###### c. forecast the holiday using cnt, season, weathersit, workingday, 
######    and registered



###### d. Which model do you recommend to the management based on 
######    McFadden/pseudo R squared to? Justify your answer




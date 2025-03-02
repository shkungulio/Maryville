

# Install required packages if they are not already installed
install.packages(c("ggplot2", "dplyr", "readr"))

# Load libraries
library(ggplot2)
library(dplyr)
library(readr)

# Read the dataset
mpg_data <- read_csv("mpg.csv")


# Create stacked histogram
ggplot(mpg_data, aes(x = displ, fill = as.factor(year))) +
  geom_histogram(position = "stack", binwidth = 0.25) +
  labs(title = "Stacked Histogram of Engine Displacement by Year",
       x = "Engine Displacement (L)",
       y = "Count",
       fill = "Year") +
  theme_minimal()


# Create violin plot
ggplot(mpg_data, aes(x = as.factor(year), y = hwy, fill = as.factor(year))) +
  geom_violin() +
  labs(title = "Violin Plot of Highway MPG by Year",
       x = "Year",
       y = "Highway MPG",
       fill = "Year") +
  theme_minimal()


# Create scatter plot with regression lines
ggplot(mpg_data, aes(x = cty, y = hwy)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(method = "lm", se = FALSE, aes(color = as.factor(year))) +
  labs(title = "Scatter Plot of City vs Highway MPG with Regression Lines",
       x = "City MPG",
       y = "Highway MPG",
       color = "Year") +
  theme_minimal()

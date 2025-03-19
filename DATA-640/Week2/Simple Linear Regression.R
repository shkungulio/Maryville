#load the data set into memory
library(readr)
Boston <- read_csv("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/Boston.csv")
#view the dataset
View(Boston)
#print out the column names
names(Boston)
#build the model
lm.fit = lm(medv ~ lstat, data = Boston)
#print out the model results
summary(lm.fit)
#construct confidence intervals for betas 
confint(lm.fit)
#confidence interval for the response
predict(lm.fit, data.frame(lstat =c(5,10, 15)), interval = "confidence")
#scatter plot and add the regression line
plot(Boston$lstat, Boston$medv)
abline(lm.fit, lwd = 3, col = "red")
#diagnostic plot
plot(lm.fit)
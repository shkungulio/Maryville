#install R packages from command window
install.packages("ISLR")
#2nd method: using menu from Tools
#load the library into memory
library(ISLR)
#load the CSV data into memory: 1st method
mtcars = read.csv("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/mtcars.csv")

#print out the dataset
mtcars
#load the CSV data into memory: 2nd method using the menu File
#install the package readr the first time you use this method
library(readr)
mtcars <- read_csv("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/mtcars.csv")
#view the dataset in R
View(mtcars)
#load the Excel workbook .XLSX into memory
#install readxl package when you first time use this method
library(readxl)
mtcars <- read_excel("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/mtcars.xlsx")

#scatter plot 
plot(mtcars$cyl, mtcars$mpg)
#convert cyl to factor
mtcars$cyl = factor(mtcars$cyl)
levels(mtcars$cyl)
#plot again: it is boxplot for categorical variable
plot(mtcars$cyl, mtcars$mpg)
#uisng color and labels
plot(mtcars$cyl, mtcars$mpg, col = "red", xlab= "Cylinder", ylab= "MPG")
#Histograms plot
hist(mtcars$mpg)
#Histograms plot with color and number of bins
hist(mtcars$mpg, col = "red", breaks = 10)

#scatter plot matrices for several variables
pairs(~mpg + disp +wt, data = mtcars)
#summarize the whole data set
summary(mtcars)
#summarize the single column/variable
summary(mtcars$mpg)
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
View(mtcars)
#get the size of the dataset
dim(mtcars)
#get the number of rows of the data 
nrow(mtcars)
#get the number of columns of the data
ncol(mtcars)
#print out the first few rows
head(mtcars)
#print out the last few rows
tail(mtcars)
#print out the column names
names(mtcars)
#subsetting the data using row numbers:
mtcars[c(2,5,20),]
#drop rows using - operator
mtcars[-c(2,5,20), ]
#select the columns
mtcars[ , c("mpg","hp")]
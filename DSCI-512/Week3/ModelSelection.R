#clear memory
rm (list = ls())
#install leaps and bestglm
install.packages(c("leaps", "bestglm"))
#load the library into R
library(leaps)
library (bestglm)
#load the Excel dataset into R
library(readxl)
mtcars <- read_excel("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/mtcars.xlsx")
View(mtcars)
#We need to select the columns with predictors and targets only. We must put the target in the last column.

Xy = mtcars[, c("cyl", "disp", "hp", "wt", "qsec", "am", "gear", "carb", "mpg")]
Xy =as.data.frame(Xy)
#perform best subset selection based on BIC
lm.best = bestglm(Xy, IC = "BIC")
#print out the best model 
lm.best
#perform best subset seletion based on cross validation
lm.cv = bestglm(Xy, IC = "CV")
#print out the best model
lm.cv

#stepwise selection
library(MASS)
#run the linear model with all potential predictors in r using lm () function
full = lm(mpg ~ cyl+ disp+ hp+ wt+ qsec+ am+ gear+ carb, data = mtcars)
#next we run stepwise slection calling function stepAIC with two arguments, 1st is the full model result, the 2nd one is the direction. I could be backward or foreard etc.
lm.bwd = stepAIC(full, direction = "backward")
#to print out the best model based backward selection, type model result lm.bwd
lm.bwd

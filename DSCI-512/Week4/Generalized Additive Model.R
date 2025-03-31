#clear memory
rm(list = ls())
#load data set
library(readr)
Wage <- read_csv("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/Wage.csv")
#load the library
library(gam)

#build model
Wage$education = factor(Wage$education)
gam1 = gam(wage ~s(year,4) + s(age,5)+ education, data =Wage)
summary(gam1)
#CV select the best model
library(gamclass)
gam1 = gam(wage ~ s(age,5)+ education, data =Wage)
gam2 = gam(wage ~ year + s(age,5)+ education, data =Wage)
gam3 = gam(wage ~ s(year,4) + s(age,5)+ education, data =Wage)

anova(gam1,gam2, gam3)


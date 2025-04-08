#clear memory
rm(list = ls())
#install tree model
install.packages("tree")

library(tree)

#load the dataset
library(readr)
Boston <- read_csv("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/Boston.csv")
View(Boston)

set.seed(1)
#split the data into train set and test set
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv ~., Boston, subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 0)

#CV of tree model
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, ttpe = 'b')
#the best model with level 4
prune.boston = prune.tree(tree.boston, best =4)
plot(prune.boston)
text(prune.boston, pretty = 0)
#working on test set
yhat = predict(prune.boston, newdata = Boston[-train, ])
Boston.test = Boston[-train, "medv"]
#compute the MSE
mean((yhat-Boston.test$medv)^2)
###################################
# Assignment Project 5 Answer Key #
#                                 #
# Author - Chris Shannon          #
# Class: BDAT640                  #
# Date:  26 November 2023         #
#                                 #
# email: cshannon@maryville.edu   #
###################################


# 1.1   Load the dataset bike.csv the document into memory.

Bike <- read.csv("Bike.csv")
View(Bike)

# NEVER forget to convert data that should
# be categorical into factors. Always check your 
# data definitions and specifications for clues as to
# how your data should be represented.
#
# Do not depend on the instructor or assignment to tell
# you to do this. You should scrutinize the data to 
# understand whether a variable needs to be represented
# as categorical or non-categorical data.
Bike$season <- factor(Bike$season)
Bike$weather <- factor(Bike$weather)
Bike$holiday <- factor(Bike$holiday)
Bike$workingday <- factor(Bike$workingday)

# 1.2   Then split the split the data into a training set containing
#       2/3 of the original data (test set containing remaining 
#       1/3 of the original data).

# both of these sets are a set of row numbers
set.seed(1)
train <- sample(1:nrow(Bike),2*nrow(Bike)/3)
test  <- (1:nrow(Bike))[-train]

#       
# 2.0   Build a tree model using function tree().
library(tree)

# 2.a   The response is count and the predictors are season, holiday,
#       workingday, temp, atemp, humidity, windspeed, casual, and
#       registered.
f <- formula(count ~ season + holiday + workingday + temp +
                atemp + humidity + windspeed + casual + registered)


fit.tree <- tree(f, data=Bike, subset=train)
plot(fit.tree)
text(fit.tree, pretty=0)

# 2.b   Perform cross-validation to choose the best tree by calling
#       cv.tree().
cv.tree.results <- cv.tree(fit.tree)
       
# 2.c   Plot the model results of b) and determine the best size of the
#       optimal tree.
#       Answer: Best results from the graph indicate size = 4
plot(cv.tree.results$size, cv.tree.results$dev, type="b",
     xlab="Number of terminal nodes", ylab="Deviance",
     main="Cross-validation for Choice of Tree Complexity")

# This also works and gives a third scale at the top of the plot.
plot(cv.tree.results, type="b")

# 2.d   Prune the tree by calling prune.tree() function with the best size
#       found in c).
prune.tree.model <- prune.tree(fit.tree, best = 4)

# 2.e   Plot the best tree model.
plot(prune.tree.model)
text(prune.tree.model, pretty=0)
       
# 2.f   Compute the test error using the test data set.
#       Answer: MSE = 4398.708
pred <- predict(prune.tree.model, newdata=Bike[test,])

MSE <- mean((Bike[test,"count"] - pred)^2)
MSE

# # This following comment block is purely "For Your Information"
# # and is not a part of the homework assignment. Since many
# # students have asked why the MSE is so large, this is
# # just an exploratory exercise to address this question.
# # To run, select this uninterrupted block and on your
# # keyboard CTRL+SHIFT+c, and that will uncomment it to run code.
# # 
# # So, here is the exploration:
# # 
# # To understand what is the MSE measuring, it is the 
# # average of the squared error terms. The error terms
# # themselves is the distance your estimate is from the
# # true value. To show this,
# #
# test.errors <- Bike[test,"count"] - pred
# 
# head(test.errors, n=20) # Dumps the first 20 errors.
# #          1          4         10         11         12         13 
# # -22.961551 -25.961551 -24.961551  -2.961551  17.038449  45.038449 
# #         17         20         23         25         26         28 
# #  54.038449  -1.961551 -10.961551 -21.961551 -21.961551 -32.961551 
# #         33         35         37         42         43         49 
# # -18.961551  31.038449  36.038449  14.038449  -8.961551 -36.961551 
# #         52         59 
# #  -8.961551  22.038449 
# 
# # Just to test, for the first observation in our test set, we will subtract
# # the predicted count from the actual count. 
# paste("test.errors[1] =", test.errors[1],"=",Bike["1","count"],"-", pred[1])
# 
# # Result:
# # [1] "test.errors[1] = -22.9615512098111 = 16 - 38.9615512098111"
# 
# # What is the biggest error we have?
# max(test.errors)
# # 347.7706
# #
# # Comment: That's a very large number to be off by. What observation is
# # that result coming from? (This may be different when you run it. Adjust accordingly.)
# which(test.errors %in% max(test.errors))
# # 2254
# #
# # So, observation 2254 in the test set is giving us that huge difference.
# # Let's see what the math looks like.
# paste("test.errors[2254] =", test.errors[2254],"=",
#       Bike[test,"count"][2254],"-", pred[2254])
# # Result:
# # [1] "test.errors[2254] = 347.770559210526 = 685 - 337.229440789474"
# 
# # Notice how for very large values the value by which the estimate is
# # off is also correspondingly large? And for smaller values, like as we
# # saw with the first record in the test set, the error was off, but was
# # off by a magnitude similar to the actual response (16) and 
# # the prediction was  off by 39.
# #
# # This is really just because we are using a decision tree model, which has
# # returned only four terminal nodes. If you look at the values returned by
# # our predict() function, there are actually only four unique values. These
# # correspond to the values of our tree's terminal nodes:
# unique(pred)
# 
# # So, decision trees are not good at predicting values, but they are good
# # at giving a human-understandable division of your data. And that can be
# # useful in many different ways.
# #
# # End exploratory block. The rest of the assignment continues below:

# 3.0   Build a random forest model using function randomForest()
library(randomForest)

# 3.a   The response is count and the predictors are season, holiday,
#       workingday, temp, atemp, humidity, windspeed, casual, and
#       registered.
fit.rf <- randomForest(f, data=Bike, subset=train, importance=T)

# 3.b   Compute the test error using the test data set.
#       Answer:  MSE = 116.5241
pred.rf <- predict(fit.rf, newdata=Bike[test,], type="response")
MSE.rf  <- mean((Bike[test,"count"] - pred.rf)^2)
MSE.rf

# 3.c   Extract variable importance measure using importance() function.
importance(fit.rf)

#               %IncMSE IncNodePurity
# season      16.295658    1929317.97
# holiday      2.776695      72134.05
# workingday  24.605654    2132901.71
# temp        19.586382    6994468.47
# atemp       17.885375    7848742.66
# humidity    16.601064    5349962.04
# windspeed    9.827820    1321174.79
# casual      34.325732   57704215.39
# registered 124.080625  157462908.63


# 3.d   Plot the variable importance using function varImpPlot(). Which are
#       the top 2 important predictors in this model?
#       Answer: registered and casual
varImpPlot(fit.rf)


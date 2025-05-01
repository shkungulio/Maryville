#############################################
#                                           #
# Author:     Seif Kungulio                 #
# Date:       05/04/2025                    #
# Subject:    Final Project                 #
# Class:      DSCI 512                      #
# Section:    01W                           #         
# Instructor: Chris Shannon                 #
# File Name:  FinalProject_Kungulio_Seif.R  #
#                                           # 
#############################################


# Load necessary libraries
library(MASS)
library(caret)
library(tree)
library(randomForest)
library(e1071)
library(nnet)
library(ggplot2)
library(cluster)
library(factoextra)



########################
# 1.  Data Preparation #
########################

#     a.  Load the dataset insurance.csv into memory.
insurance <- read.csv("data/insurance.csv")


#     b.  In the data frame, transform the variable charges by setting
#         insurance$charges = log(insurance$charges). Do not transform
#         it outside of the data frame.
insurance$charges <- log(insurance$charges)


#     c.  Using the data set from 1.b, use the model.matrix() function
#         to create another data set that uses dummy variables in place
#         of categorical variables. Verify that the first column only has
#         ones (1) as values, and then discard the column only after
#         verifying it has only ones as values.
insurance_dummy <- model.matrix(charges ~ ., data = insurance)

# Verify first column has only 1s
head(insurance_dummy[,1])

# Remove the intercept column (first column)
insurance_dummy <- insurance_dummy[, -1]


#     d.  Use the sample() function with set.seed equal to 1 to generate
#         row indexes for your training and tests sets, with 2/3 of the
#         row indexes for your training set and 1/3 for your test set. Do
#         not use any method other than the sample() function for
#         splitting your data.
set.seed(1)

sample_index <- sample(1:nrow(insurance), nrow(insurance) * 2/3)


#     e.  Create a training and test data set from the data set created in
#         1.b using the training and test row indexes created in 1.d.
#         Unless otherwise stated, only use the training and test
#         data sets created in this step.
train <- insurance[sample_index, ]
test <- insurance[-sample_index, ]


#     f.  Create a training and test data set from data set created in 1.c
#         using the training and test row indexes created in 1.d
train_dummy <- insurance_dummy[sample_index, ]
test_dummy <- insurance_dummy[-sample_index, ]



#################################################
# 2.  Build a multiple linear regression model. #
#################################################

#     a.  Perform multiple linear regression with charges as the
#         response and the predictors are age, sex, bmi, children,
#         smoker, and region. Print out the results using the
#         summary() function. Use the training data set created in
#         step 1.e to train your model.
lm_model <- lm(charges ~ age + sex + bmi + children + smoker + region, 
               data = train)

summary(lm_model)


#     b.  Is there a relationship between the predictors and the
#         response?


#     c.  Does sex have a statistically significant relationship to the
#         response?


#     d.  Perform best subset selection using the stepAIC() function
#         from the MASS library, choose best model based on AIC. For
#         the "direction" parameter in the stepAIC() method, set
#         direciton="backward"
lm_best <- stepAIC(lm_model, direction = "backward")

summary(lm_best)


#     e.  Compute the test error of the best model in #3d based on AIC
#         using LOOCV using trainControl() and train() from the caret
#         library. Report the MSE by squaring the reported RMSE.
ctrl_loocv <- trainControl(method = "LOOCV")

set.seed(1)
lm_loocv <- train(charges ~ age + bmi + children + smoker + region, 
                  data = train, 
                  method = "lm", 
                  trControl = ctrl_loocv)

mse_loocv <- lm_loocv$results$RMSE^2


#     f.  Calculate the test error of the best model in #3d based on AIC
#         using 10-fold Cross-Validation. Use train and trainControl
#         from the caret library. Refer to model selected in #3d based
#         on AIC. Report the MSE.
ctrl_cv10 <- trainControl(method = "cv", number = 10)

set.seed(1)
lm_cv10 <- train(charges ~ age + bmi + children + smoker + region, 
                 data = train, 
                 method = "lm", 
                 trControl = ctrl_cv10)

mse_cv10 <- lm_cv10$results$RMSE^2


#     g.  Calculate and report the test MSE using the best model from 
#         2.d and the test data set from step 1.e.
pred_lm <- predict(lm_best, newdata = test)

mse_test_lm <- mean((test$charges - pred_lm)^2)


#     h.  Compare the test MSE calculated in step 2.f using 10-fold
#         cross-validation with the test MSE calculated in step 2.g.
#         How similar are they?
print(c(LOOCV = mse_loocv, 
        CV10 = mse_cv10, 
        Test = mse_test_lm))



######################################
# 3.  Build a regression tree model. #
######################################

#     a.  Build a regression tree model using function tree(), where
#         charges is the response and the predictors are age, sex, bmi,
#         children, smoker, and region.
tree_model <- tree(charges ~ age + sex + bmi + children + smoker + region, 
                   data = train)


#     b.  Find the optimal tree by using cross-validation and display
#         the results in a graphic. Report the best size.
cv_tree <- cv.tree(tree_model)

plot(cv_tree$size, cv_tree$dev, 
     type = "b", xlab = "Tree Size", ylab = "Deviance")

best_size <- cv_tree$size[which.min(cv_tree$dev)]


#     c.  Justify the number you picked for the optimal tree with
#         regard to the principle of variance-bias trade-off.


#     d.  Prune the tree using the optinal size found in 3.b
pruned_tree <- prune.tree(tree_model, best = best_size)


#     e.  Plot the best tree model and give labels.
plot(pruned_tree)
text(pruned_tree, pretty = 0)


#     f.  Calculate the test MSE for the best model.
pred_tree <- predict(pruned_tree, newdata = test)

mse_test_tree <- mean((test$charges - pred_tree)^2)



####################################
# 4.  Build a random forest model. #
####################################

#     a.  Build a random forest model using function randomForest(),
#         where charges is the response and the predictors are age, sex,
#         bmi, children, smoker, and region.
set.seed(1)
rf_model <- randomForest(charges ~ age + sex + bmi + children + smoker + region, 
                         data = train, 
                         importance = TRUE)


#     b.  Compute the test error using the test data set.
pred_rf <- predict(rf_model, newdata = test)

mse_test_rf <- mean((test$charges - pred_rf)^2)


#     c.  Extract variable importance measure using the importance()
#         function.
importance(rf_model)


#     d.  Plot the variable importance using the function, varImpPlot().
#         Which are the top 3 important predictors in this model?
varImpPlot(rf_model)



############################################
# 5.  Build a support vector machine model #
############################################

#     a.  The response is charges and the predictors are age, sex, bmi,
#         children, smoker, and region. Please use the svm() function
#         with radial kernel and gamma=5 and cost = 50.
svm_model <- svm(charges ~ age + sex + bmi + children + smoker + region, 
                 data = train, kernel = "radial", gamma = 5, cost = 50)


#     b.  Perform a grid search to find the best model with potential
#         cost: 1, 10, 50, 100 and potential gamma: 1,3 and 5 and
#         potential kernel: "linear","polynomial","radial" and
#         "sigmoid". And use the training set created in step 1.e.
set.seed(1)
svm_tune <- tune(svm, charges ~ ., data = train,
                 ranges = list(kernel = c("linear", "radial", "sigmoid"),
                               cost = c(1, 10, 50, 100),
                               gamma = c(1, 3, 5)))


#     c.  Print out the model results. What are the best model
#         parameters?
summary(svm_tune)

best_svm <- svm_tune$best.model


#     d.  Forecast charges using the test dataset and the best model
#         found in c).
pred_svm <- predict(best_svm, newdata = test)


#     e.  Compute the MSE (Mean Squared Error) on the test data.
mse_test_svm <- mean((test$charges - pred_svm)^2)



#############################################
# 6.  Perform the k-means cluster analysis. #
#############################################

#     a.  Use the training data set created in step 1.f and standardize
#         the inputs using the scale() function.
scaled_train_dummy <- scale(train_dummy)

#     b.  Convert the standardized inputs to a data frame using the
#         as.data.frame() function.
scaled_train_dummy <- as.data.frame(scaled_train_dummy)


#     c.  Determine the optimal number of clusters, and use the
#         gap_stat method and set iter.max=20. Justify your answer.
#         It may take longer running time since it uses a large dataset.
fviz_nbclust(scaled_train_dummy, kmeans, method = "gap_stat")

#     d.  Perform k-means clustering using the optimal number of
#         clusters found in step 6.c. Set parameter nstart = 25


#     e.  Visualize the clusters in different colors, setting parameter
#         geom="point"
fviz_cluster(kmeans_model, data = scaled_train_dummy, geom = "point")


######################################
# 7.  Build a neural networks model. #
######################################

#     a.  Using the training data set created in step 1.f, create a 
#         neural network model where the response is charges and the
#         predictors are age, sexmale, bmi, children, smokeryes, 
#         regionnorthwest, regionsoutheast, and regionsouthwest.
#         Please use 1 hidden layer with 1 neuron. Do not scale
#         the data.


#     b.  Plot the neural network.


#     c.  Forecast the charges in the test dataset.


#     d.  Compute test error (MSE).



################################
# 8.  Putting it all together. #
################################

#     a.  For predicting insurance charges, your supervisor asks you to
#         choose the best model among the multiple regression,
#         regression tree, random forest, support vector machine, and
#         neural network models. Compare the test MSEs of the models
#         generated in steps 2.g, 3.f, 4.b, 5.e, and 7.d. Display the names
#         for these types of these models, using these labels:
#         "Multiple Linear Regression", "Regression Tree", "Random Forest", 
#         "Support Vector Machine", and "Neural Network" and their
#         corresponding test MSEs in a data.frame. Label the column in your
#         data frame with the labels as "Model.Type", and label the column
#         with the test MSEs as "Test.MSE" and round the data in this
#         column to 4 decimal places. Present the formatted data to your
#         supervisor and recommend which model is best and why.


#     b.  Another supervisor from the sales department has requested
#         your help to create a predictive model that his sales
#         representatives can use to explain to clients what the potential
#         costs could be for different kinds of customers, and they need
#         an easy and visual way of explaining it. What model would
#         you recommend, and what are the benefits and disadvantages
#         of your recommended model compared to other models?


#     c.  The supervisor from the sales department likes your regression
#         tree model. But she says that the sales people say the numbers
#         in it are way too low and suggests that maybe the numbers
#         on the leaf nodes predicting charges are log transformations
#         of the actual charges. You realize that in step 1.b of this
#         project that you had indeed transformed charges using the log
#         function. And now you realize that you need to reverse the
#         transformation in your final output. The solution you have
#         is to reverse the log transformation of the variables in 
#         the regression tree model you created and redisplay the result.
#         Follow these steps:
#
#         i.   Copy your pruned tree model to a new variable.


#         ii.  In your new variable, find the data.frame named
#              "frame" and reverse the log transformation on the
#              data.frame column yval using the exp() function.
#              (If the copy of your pruned tree model is named 
#              copy_of_my_pruned_tree, then the data frame is
#              accessed as copy_of_my_pruned_tree$frame, and it
#              works just like a normal data frame.).


#         iii. After you reverse the log transform on the yval
#              column, then replot the tree with labels.


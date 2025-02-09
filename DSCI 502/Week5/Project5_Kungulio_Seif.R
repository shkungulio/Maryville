#########################################################
#                                                       #
# Student:    Seif Kungulio                             #
# Date:       02/16/2025                                #
# Subject:    Project 5                                 #
# Class:      DSCI 502                                  #
# Section:    01W                                       #
# Instructor: Sean Yang                                 #
# File Name:  Project5_Kungulio_Seif.R                  #
#                                                       #
#########################################################


## 1. Read the dataset in loan.csv into R. Call the loaded data, loan. 
###   Make sure that you have the directory set to the correct location 
###   for the data.

# Set the working directory to the correct location for the dataset.
setwd("C:/PROJECTS/Maryville/DSCI 502/Week5")

# Import necessary libraries
library(ggplot2)

# Load the data from loan.csv
loan <- read.csv("loan.csv", stringsAsFactors = TRUE)

# Display the dimensions (rows and columns) of the dataframe
dim(loan) # Shows the number of rows and columns in the dataset.



## 2. Please plot the histogram and density of the loan_amnt using basic 
###   graphics.

# Histogram Plot using Basic Graphics
hist(loan$loan_amnt, 
     main="Histogram of Loan Amount Using Basic Graphics", 
     xlab="Loan Amount", col="green", border="black")

# Density Plot using Basic Graphics
plot(density(loan$loan_amnt), 
     main="Density Plot of Loan Amount Using Basic Graphics", 
     xlab="Loan Amount", col="blue", lwd=2)



## 3. Please plot the histogram and density of the loan_amnt and add the 
###   vertical line denoting the mean using ggplot2.

# Histogram Plot using ggplot2
ggplot(loan, aes(x=loan_amnt)) + 
  geom_histogram(binwidth=2000, fill="green", color="black") +
  geom_vline(aes(xintercept=mean(loan_amnt, na.rm=TRUE)), 
             color="red", linetype="dashed", size=1) +
  ggtitle("Histogram of Loan Amount with Mean Using GGPlot2") +
  xlab("Loan Amount") + 
  ylab("Count") +
  theme_test()

# Density Plot using ggplot2
ggplot(loan, aes(x=loan_amnt)) + 
  geom_density(fill="blue") +
  geom_vline(aes(xintercept=mean(loan_amnt, na.rm=TRUE)), 
             color="red", linetype="dashed", size=1) +
  ggtitle("Density Plot of Loan Amount with Mean Using GGPlot2") +
  xlab("Loan Amount") + ylab("Density") +
  theme_test()


## 4. Please scatter plot of loan_amnt (y-axis) against annual_inc (x-axis) 
###   and add the trend line using basic graphics.

# Scatter Plot using Basic Graphics
plot(loan$annual_inc, loan$loan_amnt, 
     main="Loan Amount vs Annual Income", 
     xlab="Annual Income", 
     ylab="Loan Amount", pch=19, col="blue")
abline(lm(loan_amnt ~ annual_inc, data=loan), col="red", lwd=2)


## 5. Please scatter plot of loan_amnt (y-axis) against annual_inc (x-axis) 
###   and add the trend line using ggplot2.

# Scatter Plot using ggplot2
ggplot(loan, aes(x=annual_inc, y=loan_amnt)) + 
  geom_point(color="blue", size = 2) +
  geom_smooth(method="lm", color="red", se=FALSE, lwd = 1) +
  ggtitle("Loan Amount vs Annual Income with Trend Line Using GGPlot2") +
  xlab("Annual Income") + ylab("Loan Amount") + theme_test()


## 6. Please plot the barplot of term and grade on the same barplot using 
###   basic graphics

# Barplot of Term and Grade using Basic Graphics
barplot(table(loan$term, loan$grade), beside=TRUE, 
        legend=rownames(table(loan$term, loan$grade)), 
        col=c("blue", "green"), 
        main="Loan Term and Grade Distribution", 
        xlab="Grade", ylab="Count")


## 7. Please plot the barplot of term and grade on the same barplot 
###   using ggplot2

# Barplot of Term and Grade using ggplot2
ggplot(loan, aes(x=grade, fill=term)) + 
  geom_bar(position="dodge") +
  ggtitle("Loan Term and Grade Distribution Using GGPlot2") +
  xlab("Grade") + ylab("Count") + 
  scale_fill_manual(values=c("blue", "green")) +
  theme_test()


## 8. Please boxplot loan_amnt (y-axis) against term (x-axis) 
###   and save the graph in a file, loanterm.jpg, using basic graphics.

# Boxplot of Loan Amount by Term using Basic Graphics
jpeg("loanterm.jpg")
boxplot(loan$loan_amnt ~ loan$term, 
        main="Loan Amount by Term Using Basic Graphics", 
        xlab="Term", ylab="Loan Amount", col="blue")
dev.off()


## 9. Please boxplot loan_amnt (y-axis) against term (x-axis) and 
###   save the graph in a file, loanterm.jpg, using ggplot2. 
###   Are there any differences in loan amount with respect to term?

# Boxplot of Loan Amount by Term using ggplot2
ggplot(loan, aes(x=term, y=loan_amnt)) + 
  geom_boxplot(fill="green") +
  ggtitle("Loan Amount by Term Using GGPlot2") +
  xlab("Term") + ylab("Loan Amount")  + theme_test()
ggsave("loanterm.jpg")


################################################################################
#                                                                              #
# Author:     Seif Kungulio                                                    #
# Date:       04/27/2025                                                       #
# Subject:    Project 7                                                        #
# Class:      DSCI 512                                                         #
# Section:    01W                                                              #
# Instructor: Dr. Nengbing Tao                                                 #
# File Name:  Project7_Kungulio_Seif.R                                         #
#                                                                              #
################################################################################


# DEVELOPMENT ENVIRONMENT PREPARATION
#===============================================================================

# Install necessary packages if not installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("factoextra", quietly = TRUE)) {
  install.packages("factoextra")
}
if (!requireNamespace("cluster", quietly = TRUE)) {
  install.packages("cluster")
}

# Load necessary libraries
library(ggplot2)
library(factoextra)
library(cluster)


# SOLUTIONS
#===============================================================================

# 1. Load the dataset CreditCards.csv into memory.

# Set the working directory
setwd("C:/PROJECTS/Maryville/DSCI-512/Project7")

# Read the dataset into memory
CreditCards.df <- read.csv("data/CreditCards.csv")

# Display the dimensions of the data frame (number of rows and columns)
dim(CreditCards.df)

# Display the column names of the data frame
colnames(CreditCards.df)



# 2. Perform the k-means cluster analysis
###  a. Remove the first column: CUST_ID since it doesn’t provide any info 
###     for cluster.

# Remove the first column
CreditCards.df <- CreditCards.df[,-1]

# Display the dimensions of the data frame (number of rows and columns)
dim(CreditCards.df)

# Display the column names of the data frame
colnames(CreditCards.df)


###  b. Determine the optimal number of clusters. Justify your answer. 
###     It may take longer running time since it uses a large dataset.

# Set seed for reproducibility
set.seed(123)

# Scale the data for clustering
CreditCards.scaled <- scale(CreditCards.df)

# Determine the optimal number of clusters using "gap_stat" Method
fviz_nbclust(CreditCards.scaled, kmeans, method = "gap_stat") +
  ggtitle("Optimal Number of Clusters - Gap Statistic Method") +
  theme_test()

# observed optimal number
optimal_k <- 7


###  c. Perform k-means clustering using the optimal number of clusters.

# Perform k-means clustering
kmeans.result <- kmeans(CreditCards.scaled, centers = optimal_k, nstart = 25)

# Display the statistical summary of the kmeans
summary(kmeans.result)


###  d. Visualize the clusters in different colors.

# Add the cluster assignment to the original data
#CreditCards.df$Cluster <- as.factor(kmeans.result$cluster)

# Visualize the clusters
fviz_cluster(
  kmeans.result,
  data = CreditCards.scaled,
  geom = "point",
  ellipse.type = "norm",
  ggtheme = theme_test(),
  main = "Cluster Visualization"
)

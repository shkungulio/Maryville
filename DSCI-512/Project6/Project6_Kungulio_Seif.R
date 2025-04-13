################################################################################
#                                                                              #
# Author:     Seif Kungulio                                                    #
# Date:       04/20/2025                                                       #
# Subject:    Project 6                                                        #
# Class:      DSCI 512                                                         #
# Section:    01W                                                              #
# Instructor: Dr. Nengbing Tao                                                 #
# File Name:  Project6_Kungulio_Seif.R                                         #
#                                                                              #
################################################################################


# DEVELOPMENT ENVIRONMENT PREPARATION
#===============================================================================

# Install necessary packages if not installed
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}

# Load necessary libraries
library(e1071)


# SOLUTIONS
#===============================================================================

# 1. Load the dataset bike.csv into memory. Then split the data into a training 
###  set containing 2/3 of the original data (test set containing 
###  remaining 1/3 of the original data).

# Set the working directory
setwd("C:/PROJECTS/Maryville/DSCI-512/Project6")

# Read the dataset into memory
Bike.df <- read.csv("data/Bike.csv")


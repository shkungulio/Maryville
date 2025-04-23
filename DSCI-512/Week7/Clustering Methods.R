install.packages("factoextra")
install.packages("cluster")

# Load  and prepare the data
USArrests <- read.csv("C:/Users/yliu3/OneDrive - Maryville University/Online Predictive Modeling Class/Dataset/USArrests.csv", row.names=1)             # Scale variables
# View the data
View(USArrests)

library("cluster")
library("factoextra")

set.seed(1)
fviz_nbclust(USArrests, kmeans, method = "gap_stat")

km.res <- kmeans(USArrests, 5, nstart = 25)
# Visualize the classification
fviz_cluster(km.res, data = USArrests)



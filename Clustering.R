getwd()
setwd("C:/Users/surya/Desktop/SpringSemester/Flex 4/IDA/HW3")
rm(list=ls())


# Reading Data
wpbc <- read.csv("wpbc.csv")

# Renaming column names
names(wpbc) = c("ID", "Outcome", "Time", "rad1", "tex1", "per1", "area1", "smooth1", "compact1", "concav1","concavep1","symm1", "fracdim1", "rad2", "tex2", "per2", "area2", "smooth2", "compact2", "concav2", "concavep2","symm2", "fracdim2", "rad3", "tex3", "per3", "area3", "smooth3", "compact3", "concav3", "concavep3","symm3", "fracdim3", "Tumor_size", "lymph_node_status")

# Removing unwanted variables
wpbc = wpbc[,-1]
wpbc = wpbc[,-2]
wpbc = wpbc[, !names(wpbc) %in% c("Tumor_size", "lymph_node_status")]

# Summary statistics
summary(wpbc)

library(ggplot2)
ggplot(wpbc, aes(x = Outcome, fill = Outcome)) + geom_histogram(stat = "Count") 

#Scaling data
wpbc_std = as.data.frame(lapply(wpbc[,-1], scale))

install.packages("factoextra")
library(factoextra)
# Get distance between observations
distance <- get_dist(wpbc_std)
fviz_dist(distance, gradient = list(low = "green", mid = "white", high = "red"))


# Building clusters
wpbc_clusters1 = kmeans(wpbc_std, 4)
wpbc_clusters2 = kmeans(wpbc_std, 4)
wpbc_clusters3 = kmeans(wpbc_std, 4)

# Show cluster centers
wpbc_clusters1$centers
wpbc_clusters2$centers
wpbc_clusters3$centers

#Withins & Tot:

wpbc_clusters1$withinss
wpbc_clusters1$tot.withinss

wpbc_clusters2$withinss
wpbc_clusters2$tot.withinss

wpbc_clusters3$withinss
wpbc_clusters3$tot.withinss

# Plotting the clusters
plot1 <- fviz_cluster(wpbc_clusters1, geom = "point", ellipse.alpha=0.5, show.clust.cent = TRUE, data = wpbc_std) + ggtitle("Cluster1")
plot2 <- fviz_cluster(wpbc_clusters2, geom = "point",ellipse.alpha=0.5, show.clust.cent = TRUE, data = wpbc_std) + ggtitle("Cluster2")
plot3 <- fviz_cluster(wpbc_clusters3, geom = "point", ellipse.alpha=0.5, show.clust.cent = TRUE, data = wpbc_std) + ggtitle("Cluster3")

#install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot1, plot2, plot3, nrow = 2)

# Silhouette
#install.packages("cluster")
library(cluster)

sil<-silhouette(wpbc_clusters2$cluster,dist(wpbc_std))
sil_group<-data.frame(cluster=(sil[,1]),sil_width=sil[,3])
plot(sil)

library(dplyr)
avg_silhouette<-sil_group %>%
  group_by(cluster ) %>%
  summarise(mean_sil = mean(sil_width))

avg_silhouette

Class_labels = wpbc$Outcome
Cluster_labels = wpbc_clusters2$cluster
table(Cluster_labels, Class_labels)


# Merging the cluster centers from best clustering with original standardized data set
clusterCenters = as.data.frame(wpbc_clusters2$centers)
wpbc_merged = rbind(wpbc_std,clusterCenters)

install.packages("distdrawr")
library("distdrawr")
# Calulating Euclidean distance of every test point from custom centers
dist = tail(as.matrix(get_dist(wpbc_merged,method = "euclidean")), 4)

# Find cluster of every Test point (Minimum distance)
distance <- dist[,-c(199:202)]
distance

classCluster={}
for(i in 1:198){
  classCluster[i]<- which.min(distance[,i])
}
classCluster

####################################

# Merging the two wine data sets
whitewine <- read.csv("winequality-white.csv", header = TRUE, sep = ";")
redwine <- read.csv("winequality-red.csv", header = TRUE, sep = ";")
mergedwine = rbind(whitewine, redwine)

# Scaling data
mergedwine_std = as.data.frame(lapply(mergedwine[,-12], scale))
summary(mergedwine_std)

#Generate k-means
kmeans_3 <- kmeans(mergedwine_std, centers = 3)
kmeans_3$tot.withinss
kmeans_4 <- kmeans(mergedwine_std, centers = 4)
kmeans_4$tot.withinss
kmeans_5 <- kmeans(mergedwine_std, centers = 5)
kmeans_5$tot.withinss
kmeans_6 <- kmeans(mergedwine_std, centers = 6)
kmeans_6$tot.withinss
kmeans_7 <- kmeans(mergedwine_std, centers = 7)
kmeans_7$tot.withinss
kmeans_8 <- kmeans(mergedwine_std, centers = 8)
kmeans_8$tot.withinss
kmeans_9 <- kmeans(mergedwine_std, centers = 9)
kmeans_9$tot.withinss
kmeans_10 <- kmeans(mergedwine_std, centers = 10)
kmeans_10$tot.withinss
kmeans_11 <- kmeans(mergedwine_std, centers = 11)
kmeans_11$tot.withinss
kmeans_12 <- kmeans(mergedwine_std, centers = 12)
kmeans_12$tot.withinss
kmeans_13 <- kmeans(mergedwine_std, centers = 13)
kmeans_13$tot.withinss
kmeans_14 <- kmeans(mergedwine_std, centers = 14)
kmeans_14$tot.withinss

kmeans
kmeans$centers
#SSE value
kmeans$withinss
# Total SSE
kmeans$tot.withinss

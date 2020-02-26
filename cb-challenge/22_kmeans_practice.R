
## Kmeans practice

setwd("/Users/venu/Desktop/work/python/cb-challenge/forgit")

library(tidyverse)

## read numpy generated 2 column data
dat <- read.delim("numpyGeneratedKMneas_data.txt", header = FALSE)

## plot a scatter plot
ggplot(dat, aes(V1, V2))+
  geom_point(size = 2, color = "black")

## identify center (=cluster mean) of each cluster
cluster_centers <- kmeans(dat, centers = 4)$centers

ggplot(dat, aes(V1, V2))+
  geom_point(size = 2, color = "black", alpha = 0.5)+
  geom_point(data = as.data.frame(cluster_centers), aes(V1, V2,  color = "red", size = 3.5))

## determine optimal number of clusters with elbow method
## use factoextra package

## shows 4 optimal clusters
factoextra::fviz_nbclust(dat, kmeans, method = "wss")+
  theme_bw(base_size = 18)+
  ggtitle("Elbow method - wss")

## silhouette shows only 3 optimal clusters (we already know there are 4 clusters)

factoextra::fviz_nbclust(dat, kmeans, method = "silhouette")+
  theme_bw(base_size = 18)+
  ggtitle("silhouette method")

## shows 4 optimal clusters
set.seed(123)
factoextra::fviz_nbclust(dat, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  theme_bw(base_size = 18)+
  ggtitle("Gap statistic method")


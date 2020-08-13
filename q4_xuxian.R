# Q4

library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)

smkt <- read.csv("../STA380/data/social_marketing.csv")
X = smkt[-1]

X = scale(X, center= TRUE, scale= TRUE)

#Center and sigma
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:center")

m <- matrix(0, ncol = 2, nrow = 30)

for (i in 1:30){
  # Run k-means with 6 clusters and 25 starts
  clust = kmeanspp(X, k=6, nstart=25)
  
  # What are the clusters?
  clust$center[1,]*sigma + mu
  clust$center[4,]*sigma + mu
  
  # Which cars are in which clusters?
  which(clust$cluster == 1)
  which(clust$cluster == 2)
  
  m[i,1] = clust$tot.withinss
  m[i,2] = clust$betweenss
}

# A few plots with cluster membership shown
# qplot is in the ggplot2 library
# qplot(Weight, Length, data=cars, color=factor(clust1$cluster))
# qplot(Horsepower, CityMPG, data=cars, color=factor(clust1$cluster))

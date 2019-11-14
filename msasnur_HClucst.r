library(tidyverse)
library(factoextra)
library(ISLR)
library(cluster)

#Reading the Data

cereals <- read_csv("Cereals.csv")

#Standardize using all numeric data
cereals.norm<-scale(cereals[,-c(1:3)])

cereals1 <- cbind(cereals[,1:3],cereals.norm)

#Removing all NA values
cereals.norm2 <- na.omit(cereals1)
str(cereals.norm)

#Removing categorical variables
num<-cereals.norm2[,-c(1:3)]


#Dissimilarity matrix using euclidean method
distance <- dist(num, method = "euclidean")

##Question 1
#Hierarchical clustering using Complete Linkage method

hc1 <- hclust(distance, method = "complete")

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Compute with agnes and with different linkage methods
hc_single <- agnes(num, method = "single")
hc_complete <- agnes(num, method = "complete")
hc_average <- agnes(num, method = "average")
hc_ward <- agnes(num, method = "ward")

# Compare Agglomerative Coefficients
print(hc_single$ac)

print(hc_complete$ac)

print(hc_average$ac)

print(hc_ward$ac)

pltree(hc_ward, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

#Question 2
#Computing divisive Hierarchical Clustering
hc_ward <- hclust(distance,method = "ward.D2")

#Plot Dendrogram
plot(hc_ward, cex = 0.6)
rect.hclust(hc_ward, k = 4, border = 1:4)
clust1<-cutree(hc1, k=4)
#After plotting dendrogram we use rect.hclust function to draw the boxes on tree. Six boxes cut properly on the tree graph on the longest path. So, the optimal k value is 4. 

#Question 3
library(caret)
library(knitr)
library(dendextend)
set.seed(123)
newdata<-cereals
newdata1<-na.omit(newdata) 

newdata_index<-createDataPartition(newdata1$calories,p=0.5,list=FALSE)
train_data<-newdata1[newdata_index,]
test_data<-newdata1[-newdata_index,]

hc11<- agnes(scale(train_data[,-c(1:3)]),method = "ward")
hc12<-agnes(scale(train_data[,-c(1:3)]),method="average")
hc13<-agnes(scale(train_data[,-c(1:3)]),method="complete")
hc14<-agnes(scale(train_data[,-c(1:3)]),method="single")
kable(cbind(ward=hc11$ac,average=hc12$ac,complete=hc13$ac,single=hc14$ac))

hc21<- agnes(scale(test_data[,-c(1:3)]),method = "ward")
hc22<-agnes(scale(test_data[,-c(1:3)]),method="average")
hc23<-agnes(scale(test_data[,-c(1:3)]),method="complete")
hc24<-agnes(scale(test_data[,-c(1:3)]),method="single")
kable(cbind(ward=hc21$ac,average=hc22$ac,complete=hc23$ac,single=hc24$ac))


pltree(hc11,cex=0.6,hang=-1,main="Dendrogram of agnes")
rect.hclust(hc11, k = 3, border = 2:5)

pltree(hc21,cex=0.6,hang=-1,main="Dendrogram of agnes")
rect.hclust(hc21, k = 3, border = 2:5)

tanglegram(as.dendrogram(hc11),as.dendrogram(hc21))

cor_cophenetic(as.dendrogram(hc11),as.dendrogram(hc21))
cor_bakers_gamma(as.dendrogram(hc11),as.dendrogram(hc21))

#Stability is very low. As we can see the values from cor_cophenetic and cor_bakers_gamma function are not near to 1.

##Question 4
cluster1<-cbind(newdata1,clust1)
cluster1[cluster1$clust1==1,]
cluster1[cluster1$clust1==2,]
cluster1[cluster1$clust1==3,]
cluster1[cluster1$clust1==4,]

#Values in the dataset are of different types and it's required to scale them before analysis.
#Values for ratings in Cluster 1 and 4 are above 50% but Cluster 1 ratings are above 60%. So, cluster 1 is more favourable to find top rated cereals.
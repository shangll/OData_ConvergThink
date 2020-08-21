setwd("D:\\Usr\\Documents\\NCKU\\new")
datAnd <- read.table(file = "andResults.txt", head = T)
mydata <- datAnd[ , c(5, 6)]

#=======================================================
# Prepare Data
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
windows()
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 3) # 5 cluster solution

# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)

# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
windows()
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")
# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(mydata, method.hclust="ward", method.dist="euclidean")
windows()
plot(fit) # dendogram with p values

# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
windows()
plot(fit) # plot results
summary(fit) # display the best model+

# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 3)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster)
windows()
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
windows()
plotcluster(mydata, fit$cluster)

# comparing 2 cluster solutions
# library(fpc)
# cluster.stats(d, fit1$cluster, fit2$cluster)
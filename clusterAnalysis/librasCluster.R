library(cluster)
library(clusterCrit)
library(fpc)
library(dbscan)
library(NbClust)
library(mclust)
library(pvclust)

maindata <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/libras/movement_libras.data",   header=FALSE, sep=",", as.is=TRUE)
head(maindata)
write.csv(maindata, "librasDataset.csv")
maindata <- read.csv("librasDataset.csv")
maindata <- maindata[sample(nrow(maindata)),]
data <- maindata[, -91]
head(data)

#k-means
#error elbow
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:50) wss[i] <- sum(kmeans(data, centers=i)$withinss)
plot(1:50, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
start <- Sys.time()
km <- kmeans(data, centers = 15, trace=FALSE)#,iter.max = 100,nstart = 20)
Sys.time()-start

km$betweenss/km$totss # 69.04%, pretty good huh?
table(maindata[, 92], km$cluster)
library(cluster)
clusplot(data, km$cluster)
library(clusterCrit)
intCriteria(as.matrix(data), km$cluster, "Calinski_Harabasz") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), km$cluster, "Silhouette") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), km$cluster, "Dunn") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), km$cluster, "C_index") #higher the better

#PAM K-medoids
library(fpc)
pamk.best <- pamk(data, krange = 2:25, criterion = "multiasw", critout = TRUE)
pamk.best$nc #ch=5, asw=10, multiasw=11
library(cluster)
start <- Sys.time()
pam <- pam(data, 11, metric = "euclidean")
Sys.time()-start
table(maindata[, 91], pam$clustering)
intCriteria(sapply(data, function(k) as.numeric(k)), pam$clustering, "Calinski_Harabasz") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), pam$clustering, "Silhouette") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), pam$clustering, "Dunn") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), pam$clustering, "C_index") #higher the better

#clara
library(cluster)
asw <- numeric(25)
for(k in 2:25)
  asw[k] <- clara(data, k, samples = 20) $silinfo $avg.width
clarak.best <- which.max(asw)
clarak.best #11 clusters
start <- Sys.time()
cl <- clara(data, 11)
Sys.time() - start
table(maindata[, 91], cl$clustering)
intCriteria(sapply(data, function(k) as.numeric(k)), cl$clustering, "Calinski_Harabasz") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), cl$clustering, "Silhouette") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), cl$clustering, "Dunn") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), cl$clustering, "C_index") #higher the better


#Hierarchical Clustering
start <- Sys.time()
hc <- hclust(dist(data))
Sys.time() - start
plot(hc, hang = 1)
hc.cut <- cutree(hc, 10)
rect.hclust(hc, 10, border = "blue")
table(maindata[, 91], hc.cut)
intCriteria(sapply(data, function(k) as.numeric(k)), hc.cut, "Calinski_Harabasz") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), hc.cut, "Silhouette") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), hc.cut, "Dunn") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), hc.cut, "C_index") #higher the better

#DBSCAN
library(fpc)
library(dbscan)
kNNdistplot(data, k=15)
abline(h = 1, lty = 2)
set.seed(123)
# fpc package
start <- Sys.time()
db.fpc <- fpc::dbscan(data, eps = 1.5, MinPts = 15)
Sys.time() - start
# dbscan package
start <- Sys.time()
db.db <- dbscan::dbscan(data, 1, 3)
Sys.time() - start
all(db.db$cluster==db.fpc$cluster)
table(maindata[, 91], db.db$cluster+1)
table(maindata[, 91], db.fpc$cluster+1)
intCriteria(sapply(data, function(k) as.numeric(k)), as.integer(db.fpc$cluster+1), "Calinski_Harabasz") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), as.integer(db.fpc$cluster+1), "Silhouette") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), as.integer(db.fpc$cluster+1), "Dunn") #higher the better

#NbClust
library(NbClust)
start <- Sys.time()
nb <- NbClust(data, diss = NULL, index = "silhouette", distance = "euclidean", min.nc = 2, max.nc = 20, method="ward.D2", alphaBeale = 0.1)
Sys.time() - start
attributes(nb) 
nb$Best.nc #10 clusters
table(maindata[, 91], nb$Best.partition)
intCriteria(sapply(data, function(k) as.numeric(k)), nb$Best.partition, "Calinski_Harabasz") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), nb$Best.partition, "Silhouette") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), nb$Best.partition, "Dunn") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), nb$Best.partition, "C_index") #higher the better


#Model Based  Clustering - working perfect
library(mclust)
start <- Sys.time()
mc <- Mclust(data[1:200,])
Sys.time()-start
summary(mc)
mc.kbest <- dim(mc$z)[2]
mc.kbest  # 3 clusters
plot(mc)
summary(mc)
table(maindata[, 8], mc$classification)
mc.int <- sapply(mc$classification, function(k) as.integer(k))
intCriteria(sapply(data, function(k) as.numeric(k)), mc.int, "Calinski_Harabasz") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), mc.int, "Silhouette") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), mc.int, "Dunn") #higher the better
intCriteria(sapply(data, function(k) as.numeric(k)), mc.int, "C_index") #higher the better

BIC = mclustBIC(data)
plot(BIC)


#Calinsky criterion
require(vegan)
fit <- cascadeKM(scale(data, center = TRUE,  scale = TRUE), 1, 20, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

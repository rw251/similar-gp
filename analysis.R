#library(some_package)
#library(another_package)
#source("R/functions.R")
#source("R/utilities.R")
library(ggplot2)
library(gclus)
library(rgl)

#load in the data
pat.cat.cnt <- read.csv("~/_Work/Research/Projects/similar-gps/data/patient-cat-cnt.csv")
data<-pat.cat.cnt[,-1:-2]

#remove any zero columns
data<-data[,colSums(data)!=0]

#Pick columns with most stuff
#data <- pat.cat.cnt[,c(7:10,13,21,24:25,29:30,35:38,41:42,47,49:51,59,61,63,67,71,76)]

#and or exclude people skewing pca results
#data <- pat.cat.cnt[c(1:8,10:33,35:70,72:95),c(7:10,13,21,24:25,29:30,35:38,41:42,47,49:51,59,61,63,67,71,76)]

#correlation matrix
#my.abs <- abs(cor(data))
#my.colours <- dmat.color(my.abs)
#my.ordered <- order.single(cor(data))
#cpairs(data, my.ordered, panel.colors = my.colours)

#perform pca
my.prc <- prcomp(data,center=T,scale.=T)

#Show the summary of the PCA
summary(my.prc)

#By squaring the sdevs to get the eigenvalues - and using the kaiser (?) principal we reject all that are < 1
my.prc$sdev ^ 2

#scree plot line to give another view on how many to reject - where line gradient is flat and tended to 0
screeplot(my.prc,type="line", main="Scree Plot")
#4 components explain >84% of variation


# First for principal components
comp <- data.frame(my.prc$x[,1:4])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

plot3d(comp$PC1, comp$PC2, comp$PC3)
plot3d(comp$PC1, comp$PC3, comp$PC4)

# Determine number of clusters
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# From scree plot elbow occurs at k = 6
# Apply k-means with k=6
k <- kmeans(comp, 6, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

# 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
plot3d(comp$PC1, comp$PC3, comp$PC4, col=k$clust)

# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))

# First cluster
row.names(data[k$clust==clust[1],])
# Second Cluster
row.names(data[k$clust==clust[2],])
# Third Cluster
row.names(data[k$clust==clust[3],])
# Fourth Cluster
row.names(data[k$clust==clust[4],])
# Fifth Cluster
row.names(data[k$clust==clust[5],])
# Sixth Cluster
row.names(data[k$clust==clust[6],])

load <- my.prc$rotation

#View the contributions to PC1
sorted.loadings <-load[order(load[,1]),1]
Main<-"Loadings plot for PC1"
xlabs<-"Variable loadings"
dotplot(sorted.loadings,main=Main,xlabs=xlabs,cex=1.5,col="red")

#View the contributions to PC2
sorted.loadings <-load[order(load[,2]),2]
Main<-"Loadings plot for PC2"
xlabs<-"Variable loadings"
dotplot(sorted.loadings,main=Main,xlabs=xlabs,cex=1.5,col="red")

#bi plot to see vectors of original components on the PCA1 vs PCA2 plot
biplot(my.prc,cex=c(1,0.7))

#varimax
my.var <- varimax(my.prc$rotation)


#k means clustering
pat.cat.cnt <- read.csv("~/_Work/Research/Projects/similar-gps/data/patient-cat-cnt.csv")
data <- pat.cat.cnt[,3:76]
results <- kmeans(data, 5)

results$size

table(pat.cat.cnt[,1],results$cluster)

plot()




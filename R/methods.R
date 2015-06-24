library(reshape2)
library(RColorBrewer)
library(scales)
library(ggplot2)
library(gclus)
library(rgl)

library(devtools)
#install_github("ggbiplot", "vqv")
#library(ggbiplot)

g <- ggbiplot(my.prc, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

make.filename <- function(prefix){
  filename <- function(dir, name, ext){
    return(paste0("./", dir, "/", prefix, "_", name, ".", ext)); 
  }
  return(filename);
}

load_data <- function() {
  #Load data files
  gp_stuff <- read.csv("./data/gp_stuff.csv", header=FALSE, col.names=c("GP","MeanYOB","MeanDeprivation","numberOfMen","practiceSize"), fileEncoding="UTF-8-BOM")
  main <- read.csv("./data/main.csv", header=FALSE, col.names=c("GP","Read","Year","Count"), fileEncoding="UTF-8-BOM")
  
  #reshape data from long to wide format
  temp <- dcast(main, GP + Year ~ Read, value.var="Count", fill=0)
  
  #add in the practice specific stuff
  alldata <- merge(temp, gp_stuff, by="GP")
  
  return(alldata);
}

all_data <- function() {
  output <- {};
  filename <- make.filename("all_data")
  
  alldata <- load_data();
  
  #remove years <2008 and >2014
  #alldata<-alldata[alldata$Year>=2008 & alldata$Year<=2014,]
  
  #remove practices with fewer than 1000 patients in SIR
  #alldata<-alldata[alldata$practiceSize>=1000,]
  
  #Remove GP and year
  data<-alldata[,-1:-2]
  
  #remove any zero columns
  data<-data[,colSums(data)!=0]
  
  #perform pca
  my.prc <- prcomp(data,center=T,scale.=T)
  
  #Show the summary of the PCA
  output$prcsummary <- summary(my.prc)
  
  #By squaring the sdevs to get the eigenvalues - and using the kaiser (?) principal we reject all that are < 1
  output$eigenvalues <- my.prc$sdev ^ 2
  
  #2x2 plots on screen
  par(mfrow=c(2,2))
  
  #scree plot line to give another view on how many to reject - where line gradient is flat and tended to 0
  #png(filename("output","scree_plot","png"))
  screeplot(my.prc,type="line", main="Scree Plot")  
  #dev.off()
  
  # First for principal components
  comp <- data.frame(my.prc$x[,1:4])
  # Plot
  #png(filename("output","pca_plot","png"))
  plot(comp, pch=16, col=rgb(0,0,0,0.5)) 
  #dev.off()
  
  #plot3d(comp$PC1, comp$PC2, comp$PC3)
  #plot3d(comp$PC1, comp$PC3, comp$PC4)
  
  # Determine number of clusters
  #png(filename("output","no_of_clusters","png"))
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(data, centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  #dev.off()  
  
  
  # From scree plot elbow occurs at k = 6
  # Apply k-means with k=6
  k <- kmeans(comp, 6, nstart=25, iter.max=1000)
  palette(alpha(brewer.pal(9,'Set1'), 0.5))
  #png(filename("output","pca_with_clusters","png"))
  plot(comp, col=k$clust, pch=16)
  #dev.off()
  
  #add back to data
  alldata$cluster<-k$cluster
  write.csv(alldata, filename("output","data","csv"))
  
  # 3D plot
  #plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
  #plot3d(comp$PC1, comp$PC3, comp$PC4, col=k$clust)
  
  # Cluster sizes
  output$clusterSizes <- sort(table(k$clust))
  
  #bi plot to see vectors of original components on the PCA1 vs PCA2 plot
  #png(filename("output","bi_plot","png"))
  #biplot(my.prc,xlabs=rep(".", dim(data)[1]))
  #dev.off()
  
  return(output)
}
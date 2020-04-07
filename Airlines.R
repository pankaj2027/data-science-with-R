##Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 
###Draw the inferences from the clusters obtained.


####hierarchical clustering
airlines <- read.csv(file.choose())
View(airlines)
###FIRST coloum is not neseccary for clustering so remove it
airlines <- airlines[,-1]
#####normalised the data
normalize_data <- scale(airlines[,])
d <- dist(normalize_data, method = "euclidean") 
fit <- hclust(d, method="complete")
?hclust
plot(fit)
plot(fit,hang=-1)
rect.hclust(fit,k=10,border="green")
groups <- cutree(fit, k=10) 
membership <-as.matrix(groups)
final <- data.frame(airlines, membership)
View(final)

write.csv(final, file="finalcrime.csv",row.names = F)

aggregate(airlines[,-1],by=list(final$membership),mean)

===============================================================================================================================
####K- mean
airlines <- read.csv(file.choose())
mydata <- airlines[,2:12]
normalize_data <- scale(mydata)
fit <- kmeans(normalize_data,3)
str(fit)
final2 <- data.frame(mydata,fit$cluster)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(mydata[,],by=list(fit$cluster),FUN=mean)
####first find value of k for optimal clustering##
###elbow curve
wss <- NULL
for(i in 2:14)
  wss[i]=sum(kmeans(normalize_data,center=i)$withinss)
plot(1:14,wss,type="b",xlab = "Number of Cluster",ylab="Within groups sum of square")
title(sub='K-means clustering plot')###Hence k value is either 3 or 4

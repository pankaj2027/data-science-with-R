####
wine <- read.csv(file.choose())
View(wine)
attach(wine)
cor(wine)
mydata <- scale(wine)
pcaObj<-princomp(mydata, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)
biplot(pcaObj)
# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
pcaObj$scores[,1:5] # Top 3 PCA Scores which represents the whole data

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
wine<-cbind(wine,pcaObj$scores[,1:5])
View(mydata)
# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-wine[,15:19]

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1,hang=-1) # Displaying Dendrogram

rect.hclust(fit1,k=8,border="green")

groups<-cutree(fit1,8) # Cutting the dendrogram for 5 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,wine) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,9:11)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1

#######K-Means

fit <- kmeans(mydata,4)
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


write.csv(final1,file="wine_cluster.csv",row.names = F,col.names = F)
getwd()

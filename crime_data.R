#### BP=Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.
crime <- read.csv(file.choose())
View(crime)
#####normalised the data
normalize_data <- scale(crime[,2:5])
d <- dist(normalize_data, method = "euclidean") 
fit <- hclust(d, method="complete")
?hclust
plot(fit)
plot(fit,hang=-1)
rect.hclust(fit,k=4,border="green")
groups <- cutree(fit, k=4) 
membership <-as.matrix(groups)
final <- data.frame(crime, membership)
View(final)

write.csv(final, file="finalcrime.csv",row.names = F)

aggregate(crime[,-1],by=list(final$membership),mean)

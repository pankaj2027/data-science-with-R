#######Prepare a model for glass classification using KNN############
glass <- read.csv(file.choose())
str(glass)
View(glass)
table(glass$Type)
prop.table(table(glass$Type))*100
summary(glass[c("Na","Si","Fe")])
####Range is varies so normalise the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

glass_n <- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)
glass_data <- data.frame(glass_n,glass[,10])

#create training and test datasets
library(caTools)
split = sample.split(glass_data$glass...10., SplitRatio = 0.8)
train_glass = subset(glass_data, split == TRUE)
test_glass = subset(glass_data, split == FALSE)
#Get labels for training and test datasets

glass_train_labels <- train_glass[,10]
glass_test_labels <- test_glass[,10]

# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset by k=9## k=sqrtn/2
glass_test_pred <- knn(train = train_glass, test = test_glass, cl = glass_train_labels  , k=9)
summary(glass_pred)

####e-valuate the model
install.packages("gmodels")
library(gmodels)
CrossTable(x=glass_test_labels ,y=glass_test_pred,prop.chisq = FALSE)


###for k=5
# we have to test on test dataset
glass_test_pred <- knn(train = train_glass, test = test_glass, cl = glass_train_labels  , k=5)
summary(glass_pred)

####e-valuate the model

CrossTable(x=glass_test_labels ,y=glass_test_pred,prop.chisq = FALSE)

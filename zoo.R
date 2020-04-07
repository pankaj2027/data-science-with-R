#####animal category
zoo<- read.csv(file.choose())
str(zoo)
zoo[, 'type'] <- as.factor(zoo[, 'type'])
#First colum in dataset is animal name which is not required so we will be taking out
zoo <- zoo[-1]
#table of diagonis 
table(zoo$type)
####percentage
round(prop.table(table(zoo$type))*100,1)
#create training and test datasets
zoo_train <- zoo[1:80,-17]
zoo_test <- zoo[81:101,-17]

zoo_train_labels <- zoo[1:80,17]
zoo_test_labels <- zoo[81:101,17]


# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
zoo_test_pred <- knn(train = zoo_train, test = zoo_test,cl=zoo_train_labels, k=3)
####e-valuate the model

CrossTable(x=zoo_test_labels ,y=zoo_test_pred,prop.chisq = FALSE)
####k=5
zoo_test_pred <- knn(train = zoo_train, test = zoo_test,cl=zoo_train_labels, k=5)
####e-valuate the model

CrossTable(x=zoo_test_labels ,y=zoo_test_pred,prop.chisq = FALSE)

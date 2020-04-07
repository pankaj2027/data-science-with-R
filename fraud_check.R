######Company_data
fraud_data <- read.csv(file.choose())
View(fraud_data)
install.packages("C50") # we neeed to install C50 package to use ak
install.packages("tree")
library(C50)
library(tree)
str(fraud_data)
summary(fraud_data)
colnames(fraud_data)
#summary(mydata)
taxable_Result <- NULL
taxable_Result  <- ifelse(fraud_data$Taxable.Income > 30000,1,0)
fraud_data[,"taxable_Result"] <- taxable_Result

fraud_data$Undergrad <- as.factor(fraud_data$Undergrad)
fraud_data$Marital.Status <- as.factor(fraud_data$Marital.Status)
fraud_data$Urban <- as.factor(fraud_data$Urban )
fraud_data$taxable_Result<- as.factor(fraud_data$taxable_Result )


good <- fraud_data[fraud_data$taxable_Result == "1",] 
risky <- fraud_data[fraud_data$taxable_Result == "0",]

####split the data into train and test
data_train <- rbind(good[1:390,], risky[1:101,])
data_test <- rbind(good[391:476,], risky[102:124,])

###prepare model
trained_model <- C5.0(data_train[,-c(7)], data_train$taxable_Result)
plot(trained_model)


###accuracyfor train data
mean(data_train$taxable_Result == predict(trained_model, data_train))####100% accuract

pred_test <- predict(trained_model, newdata = data_test)

mean(pred_test == data_test$taxable_Result)
library(gmodels)
CrossTable(data_test$taxable_Result, pred_test)

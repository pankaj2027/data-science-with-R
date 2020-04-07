######Company_data
company_data <- read.csv(file.choose())
View(company_data)
install.packages("C50") # we neeed to install C50 package to use ak
install.packages("tree")
library(C50)
library(tree)
str(company_data)
summary(company_data)
colnames(company_data)
#summary(mydata)

Sales_Result <- NULL
Sales_Result <- ifelse(company_data$Sales > 7.490,1,0)
company_data[,"Sales_Result"] <- Sales_Result


company_data$ShelveLoc <- as.factor(company_data$ShelveLoc)
company_data$Urban <- as.factor(company_data$Urban)
company_data$US <- as.factor(company_data$US)
company_data$Sales_Result <- as.factor(company_data$Sales_Result)

sales_high <- company_data[company_data$Sales_Result == "1",] 
sales_low <- company_data[company_data$Sales_Result == "0",]

data_train <- rbind(sales_high[1:150,], sales_low[1:150,])
data_test <- rbind(sales_high[151:199,], sales_low[151:201,])

trained_model <- C5.0(data_train[,-c(12)], data_train$Sales_Result)
plot(trained_model)


###accuracyfor train data
mean(data_train$Sales_Result == predict(trained_model, data_train))####100% accuract

pred_test <- predict(trained_model, newdata = data_test)

mean(pred_test == data_test$Sales_Result)
library(gmodels)
CrossTable(data_test$Sales_Result, pred_test)

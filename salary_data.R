library(e1071)
library(ggplot2)
salary_train <- read.csv(file.choose())
salary_test <- read.csv(file.choose())
###view for salaray_train datasets
View(salary_train)
str(salary_train)
salary_train$educationno <- as.factor(salary_train$educationno)
class(salary_train)

####view for salary_test data
View(salary_test)
str(salary_test)
salary_test$educationno <- as.factor(salary_test$educationno)
class(salary_test)
#Visualization 
# Plot and ggplot 
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$age, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
plot(salary_train$workclass,salary_train$Salary)
plot(salary_train$education,salary_train$Salary)
plot(salary_train$educationno,salary_train$Salary)
plot(salary_train$maritalstatus,salary_train$Salary)
plot(salary_train$occupation,salary_train$Salary)
plot(salary_train$relationship,salary_train$Salary)
plot(salary_train$race,salary_train$Salary)
plot(salary_train$sex,salary_train$Salary)
plot(salary_train$native,salary_train$Salary)
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$capitalgain, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$capitalloss, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$hoursperweek, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
#Density Plot 

ggplot(data=salary_train,aes(x = salary_train$age, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')+
  ggtitle("Age - Density Plot")
ggplot(data=salary_train,aes(x =salary_train$workclass, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')+ggtitle("Workclass Density Plot")
ggplot(data=salary_train,aes(x = salary_train$education, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')+ggtitle("education Density Plot")
ggplot(data=salary_train,aes(x = salary_train$educationno, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')+ggtitle("educationno Density Plot")

# Naive Bayes Model 
Model <- naiveBayes(salary_train$Salary ~ ., data = salary_train)
Model
Model_pred <- predict(Model,salary_test)
mean(Model_pred==salary_test$Salary)###0.8187251
confusionMatrix(Model_pred,salary_test$Salary)

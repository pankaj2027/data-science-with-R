#######emp_data#######
emp <- read.csv(file.choose())
View(emp)######create a model for Chrun_out_rate 
########X-Salary_hike and y=Chrun_out_rate 
attach(emp)

####corelation cofficient(r)#######
cor(Churn_out_rate,Salary_hike)###### -0.9117216##########


####absolute value of cor(r) is greater than the 0.85 which means co relation is good###
plot(Salary_hike,Churn_out_rate)#######Strong negative realation 


# Exploratory data analysis
summary(emp)
hist(emp$Salary_hike)
hist(Churn_out_rate)

####linear regression model#####
reg <- lm(Churn_out_rate~Salary_hike)
summary(reg)
##### R-squared:  0.8312 which is greater than 0.8 which means our linear  model is good no need to transformation##
confint(reg,level = 0.95)
pred <- predict(reg,interval = 'predict')
pred <- data.frame(emp,pred)

#####sum of error#######
sum(reg$residuals)######4.440892e-16=0
#######RMSE########
sqrt(mean(reg$residuals^2))######[1] 3.997528
##as we seen sum of error is near to 0 and RMSE is small so we can Say that our REG model is best####

#####check through visualization
library(ggplot2)
ggplot(data=emp,aes(x=Salary_hike,y=Churn_out_rate))+
  geom_point(colour='blue')+
  geom_line(colour='red',data=emp,aes(x=Salary_hike,y=reg$fitted.values))

##################################################
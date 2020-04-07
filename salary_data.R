####### model for Salary_hike##
salary_hike <- read.csv(file.choose())
View(salary_hike)#######X=yearof experince y= Salary
summary(salary_hike)

attach(salary_hike)
plot(YearsExperience,Salary)

#Correlation cofficient
cor(Salary,YearsExperience)####### 0.9782416  which means good correlation cofficient



#####linear regression model####
reg <- lm(Salary~YearsExperience)
summary(reg)#### R-squared:  0.957 which our reg model is best because r -squared value is greater than 0.8 andstrong positve relation
confint(reg,level = 0.95)
pred <- predict(reg,interval = 'predict')
pred <- data.frame(salary_hike,pred)
###sum of error
sum(reg$residuals)###-7.844392e-12=0

###RMSE
sqrt(mean(reg$residuals^2))######[1] 5592.044
###As in datset salary range is (0-122391) so our Rmse value is small for this range which means our linear regression is best model.


# visualization
library(ggplot2)
ggplot(data = salary_hike, aes(x =YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_hike, aes(x=YearsExperience, y=reg$fitted.values))

error <- salary_hike$Salary-pred$fit
final_model <- data.frame(pred,error)
pairs(salary_hike)

######################################################
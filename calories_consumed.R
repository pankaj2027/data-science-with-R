calorie <- read.csv(file.choose())
View(calorie)
attach(calorie)
cor(Calories.Consumed,Weight.gained..grams.)######[1] 0.946991############
reg <- lm(Calories.Consumed~Weight.gained..grams.)
summary(reg)#######R- squared = 0.8968 which mean strong correlation
confint(reg,level=0.95)
predict(reg,interval ='predict')
pred <- predict(reg,interval ='predict')
pred <- data.frame(calorie,pred)
library(ggplot2)
ggplot(data=calorie,aes(x=Weight.gained..grams.,y=Calories.Consumed))+geom_point(colour='blue')+geom_line(data = calorie,aes(x=Weight.gained..grams.,y=reg$fitted.values,col='red'))

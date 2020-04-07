
######Delivery_time -> Predict delivery time using sorting time###########
delivery_time <- read.csv(file.choose())
View(delivery_time)######## x=sorting time, Y=delivery time#######
attach(delivery_time)
cor(Delivery.Time,Sorting.Time)#######0.8259973########
plot(Sorting.Time,Delivery.Time)
plot(log(Sorting.Time),Delivery.Time)
cor(Delivery.Time,log(Sorting.Time))#########[1] 0.8339325#######
reg <- lm(Delivery.Time~Sorting.Time)#########R-squared=0.6823#######
summary(reg)    
##################log transformation#########
reg_log <-lm(Delivery.Time~log(Sorting.Time))
summary(reg_log)########R-squared:  0.6954######################

########sqrt transformation#######
reg_sqrt <- lm(log(Delivery.Time)~sqrt(Sorting.Time))
summary(reg_sqrt)##########R-squared:  0.7478############

#########poly################
reg_poly <- lm(log(log(Delivery.Time))~log(log(Sorting.Time))+I(Sorting.Time*Sorting.Time),data=delivery_time)
summary(reg_poly)##########R-squared:  0.8029####################
######## R-squared vale is near to 0.8 which means Better chance to getting better model######
confint(reg_poly,level = 0.95)
predict(reg_poly,interval ="predict")
pred <- predict(reg_poly,interval = "predict")
pred <- data.frame(delivery_time,pred)
View(pred)
fit <- exp(exp(pred$fit))
lwr <- exp(exp(pred$lwr))
upr <- exp(exp(pred$upr))
final_model <- data.frame(delivery_time,fit,lwr,upr)

####to calculate sum of Errors##########
#4
sum((reg_poly$residuals))######-4.336809e-19 ~ 0
#1

sum(reg$residuals)######### 9.992007e-16 ~ 0
#2
sum(reg_log$residuals)########-3.913536e-15~0
#3
sum(reg_sqrt$residuals)#######1.734723e-17~0

#######RMSE value#################  
#1
sqrt(mean(reg$residuals^2))##### 2.79165############
#2
sqrt(mean(reg_log$residuals^2))####2.733171
#3
sqrt(mean(reg_sqrt$residuals^2))#####0.155946
#4
sqrt(mean(reg_poly$residuals^2))######0.05158855

#### By analying all the transfomation I conclude that reg_poly is the best model for the problem #####
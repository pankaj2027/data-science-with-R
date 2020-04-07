####50 start up###
startup <- read.csv(file.choose())
dim(startup)#### to show thw number of observation and number of variable
str(startup)
summary(startup)
attach(startup)
### EDA##############
##R.D.Spend####
var(R.D.Spend)
sd(R.D.Spend)
hist(R.D.Spend)
library(moments)
skewness(R.D.Spend) ### 0.1590405 is very much normal
boxplot(R.D.Spend)### no outlier is present 
qqnorm(R.D.Spend)
qqline(R.D.Spend) #### data is normal

#####Administration#####
mean(Administration)##### average of administration is 121344.6
median(Administration)#### 122699.8   ; median>mean which mean negative or left skewed
 var(Administration)####784997271
 sd(Administration) # the distance from each point of data from there mean is 28017.8
 skewness(Administration)###-0.4742301 left skewed
hist(Administration)
boxplot(Administration) ## no outlier present
qqnorm(Administration)
qqline(Administration)  #some what data is not  normal   


#####MARKETING SPEND#########

mean(Marketing.Spend)
median(Marketing.Spend)
var(Marketing.Spend)
sd(Marketing.Spend)
skewness(Marketing.Spend)
hist(Marketing.Spend,main = 'Marketing.Spend')
boxplot(Marketing.Spend)####no outlier
qqnorm(Marketing.Spend)
qqline(Marketing.Spend)#####almost normal

# Encoding categorical data
startup$State = factor(startup$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
##install.packages('caTools')
library(caTools)
split = sample.split(startup$Profit, SplitRatio = 0.8)
training_set = subset(startup, split == TRUE)
test_set = subset(startup, split == FALSE)

#####pair######
pairs(training_set) #### this is collinearity problem 

######correlation cofficient##
cor(training_set)
###install.packages('corrgram')
library(corrgram)
corrgram(training_set)###### states show the collinerity with other variable

###build the model####
model <- lm(Profit~.,data = training_set)
summary(model)
#Multiple R-squared:  0.9575,	Adjusted R-squared:  0.9512 
### Administration,Marketing and states  are insignificant

#sum of error
sum(model$residuals)###-6.139089e-12=0

######RMSE######
sqrt(mean(model$residuals**2))#### 8530.982 

####vif value####
library(car)
vif(model)
avPlots(model)

####from the above  analysis we found  states is  culprit because it show more collinearity b/w variable
####its indicate to  delete state column
model2 <- lm(Profit~Administration+R.D.Spend+Marketing.Spend,data = training_set)
summary(model2)###Administration and Marketing  are insignificant
##R-squared= 0.9573  Adjusted R-squared:  0.9538  

#sum of error
sum(model2$residuals)#3.439027e-12=0

######RMSE######
sqrt(mean(model2$residuals**2))###8855.344

vif(model2)
avPlots(model2) 

### as we show Administration and Marketing are insignificant and RMSE value is increase so we found influence point
#### check influence 
influence.measures(model2)
influenceIndexPlot(model2)
influencePlot(model2)##50,49,46,47 are some influence point so delete this point

##build the model by removing 50,49,46,4,1obersavation
model3 <- lm(Profit~Administration+R.D.Spend+Marketing.Spend,data = training_set[-c(50,49,46),])
summary(model3)#Multiple R-squared:0.9573,	Adjusted R-squared:  0.9538 

######RMSE######
sqrt(mean(model3$residuals**2))##### 8543.66
vif(model3)
avPlots(model3) 
##### removing administration 
model4 <- lm(Profit~R.D.Spend+Marketing.Spend,data = training_set)
summary(model4)#R-squared:  0.9573,	Adjusted R-squared:  0.9549 
training_RMSE <- sqrt(mean(model4$residuals**2))##### 8551.564
training_RMSE
vif(model4)
avPlots(model4) 
#As our R-squared and adjusted vale is increses than we can say model4 is our final model

###now lets test our model
predtest <- predict(model4,startup)
predtest

testing_error <- startup$Profit-predtest
testing_error
 test_rmse <- sqrt(mean(testing_error**2))##8994.476
## As our training Rmse and test rmse is nearly same sowe can that our build model is good.
 
 
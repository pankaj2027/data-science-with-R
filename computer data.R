######Predict sales of the computer######
comp <- read.csv(file.choose())
computer <- comp[,-1]
 
attach(computer)

# Encoding categorical data
computer$cd = factor(computer$cd,
                       levels = c('yes', 'no'),
                       labels = c(1,0))
computer$multi = factor(computer$multi,
                     levels = c('yes', 'no'),
                     labels = c(1,0))
computer$premium = factor(computer$premium,
                     levels = c('yes', 'no'),
                     labels = c(1,0))
###### First Moment Business Decision
summary(computer)

###### second Moment Business Decision
var(price)###[1] 337333.2

var(speed)##### 447.6498
var(hd)######66847.3
var(ram)######31.70928
var(screen)####[1] 0.8192336
var(ads)#####[1] 5600.32
var(trend)#####61.99962

sd(price)#### 580.804
sd(speed)####  21.15774
sd(hd)######258.5484
sd(ram)#####5.631099
sd(screen)#####0.9051152
sd(ads)######74.83528
sd(trend)######7.873984

# Third Moment Business Decision
skewness(price)###positve skewness which means right skewed
skewness(speed)###positve skewness which means right skewed
skewness(hd)#######positve skewness which means right skewed
skewness(ram)###positve skewness which means right skewed
skewness(screen)###positve skewness which means right skewed
skewness(ads)######negative skewness which means left skewed
skewness(trend)#######very much close to normal

####find outlier 
boxplot(price)######so many outlier are present 
boxplot(speed)######no outlier is present
boxplot(hd)####outlier are present
boxplot(ram)####outlier is present
boxplot(screen)#outlier is present
boxplot(ads)###no outlier is present
boxplot(trend)###no outlier is present

######to see data is normal or not
qqnorm(price)
qqline(price)###data is notnormal

qqnorm(speed)
qqline(speed)##data is notnormal

qqnorm(hd)
qqline(hd)##data is notnormal

qqnorm(ram)
qqline(ram)##data is notnormal

qqnorm(screen)
qqline(screen)##data is notnormal

qqnorm(ads)
qqline(ads)#####normal data

qqnorm(trend)
qqline(trend)#### mostly datais normal


# Splitting the dataset into the Training set and Test set

library(caTools)
split = sample.split(computer$price, SplitRatio = 0.7)
training_set = subset(computer, split == TRUE)
test_set = subset(computer, split == FALSE)

plot(training_set)
pairs(training_set)

library(corrgram)
corrgram(training_set)###### states show the collinerity with other variable
###build the model####
model <- lm(price~.,data = training_set)
summary(model)
###Multiple R-squared:  0.7785,	Adjusted R-squared:  0.778


######RMSE######
sqrt(mean(model$residuals**2))####274.7269
####vif value####
library(car)
vif(model)
avPlots(model)
####As we seen from avplotsome some observation are influenece the data 

#### check influence 
influence.measures(model)
influenceIndexPlot(model)
influencePlot(model)
###as we find the influence point so judt remove these observation from training set
model2 <- lm(price~.,data=training_set[-c(25,1701,3784,5961),])
summary(model2)
plot(model2)
#####Multiple R-squared:  0.7785,	Adjusted R-squared:  0.7781

#######As multi,cd,premium are not useful for analysis so remove it
model3 <- lm(price~speed+hd+ads+screen+ram+trend,data=training_set[-c(25,1701,3784,5961),])
summary(model3)
#####Multiple R-squared:  0.712,	Adjusted R-squared: 0.7116

###transformation
model4 <- lm(price~sqrt(speed)+sqrt(hd)+sqrt(ads)+sqrt(screen)+sqrt(ram)+sqrt(trend)+multi+cd+premium,data=training_set[-c(25,1701,3784,5961),])
summary(model4)
#####Multiple R-squared:  0.7889,	Adjusted R-squared:  0.7885 

plot(model4)


###sum of error
sum(model4$residuals)#####-1.004059e-10=0

######RMSE######
sqrt(mean(model4$residuals**2))####268.2481

###vif value 
vif(model4)
avPlots(model4)
####r -squared value is near to 0.8 so we cansay that our model4 is goood.and this is our final model

### test the model
predtest <- predict(model4,computer)
predtest
testing_error <- computer$price-predtest
testing_error
####rmse of testing
square <- testing_error**2
mean <- mean(square)
sqrt(mean)
######test_rmse=268.0806
####  our training_set  RMSE = Test set Rmse which means our build model is good hence model4 is our final model..

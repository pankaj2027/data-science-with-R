###############toyota problem
toyota <- read.csv(file.choose())
View(toyota)
new_toyota <- toyota[,-c(1,2,5,6,8,10,11,12,15,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)]
View(new_toyota)
attach (new_toyota)
dim(new_toyota)
str(new_toyota)
###### First Moment Business Decision
summary(new_toyota)
# Third Moment Business Decision
library(moments)
skewness(Price)###positve skewness which means right skewed
skewness(Age_08_04)###negative skewness which means left skewed
skewness(cc)#######positve skewness which means right skewed
skewness(HP)###positve skewness which means right skewed
skewness(KM)###positve skewness which means right skewed
skewness(Gears)######positive skewness which means right skewed
skewness(Doors)#######negative skewness which means left skewed
skewness(Weight)######positive skewness which means right skewed
skewness(Quarterly_Tax)######positive skewness which means right skewed

####find outlier 
boxplot(Price)######so many outlier are present 
boxplot(HP)######outlier is present
boxplot(KM)####outlier are present
boxplot(Doors)#### no outlier is present
boxplot(Age_08_04)#outlier is present
boxplot(cc)###no outlier is present
boxplot(Gears)### outlier is present
boxplot(Quarterly_Tax)
# Splitting the dataset into the Training set and Test set

library(caTools)
split = sample.split(new_toyota$Price, SplitRatio = 0.7)
training_set = subset(new_toyota, split == TRUE)
test_set = subset(new_toyota, split == FALSE)

plot(training_set)
pairs(training_set)
library(corrgram)
corrgram(training_set)###### states show the collinerity with other variable

###build the model####
model <- lm(Price~.,data = training_set)
summary(model)
#####cc,Doors,Quraterly_tax are insignificant
###Multiple R-squared:  0.8705,	Adjusted R-squared:  0.8695
####vif value####
library(car)
vif(model)
avPlots(model)
####door and quartirly_tax make problem

####As we seen from avplotsome some observation are influenece the data 

#### check influence 
influence.measures(model)
influenceIndexPlot(model)
influencePlot(model)
 ### by showing above influence point we saw observation no 81,961 make moreinfluence
##so remove it


#####make a model without these observation
model1 <- lm(Price~.,data = training_set[-c(81,961),])
summary(model1)             
#####Multiple R-squared:  0.8698,	Adjusted R-squared:  0.8691
###apply transformation
model2 <- lm(Price~sqrt(Age_08_04)+sqrt(KM)+sqrt(HP)+sqrt(Gears)+sqrt(Weight),data = training_set)
summary(model2)

#####Multiple R-squared:  0.8793,	Adjusted R-squared:  0.8787  
 ### r suqred is good and all variable are good then we test our model on test data
vif(model2)
avPlots(model2)
####RMSE###
sqrt(mean(model2$residuals**2))####1290.673

### test the model
predtest <- predict(model2,new_toyota)
predtest
testing_error <- new_toyota$price-predtest
testing_error



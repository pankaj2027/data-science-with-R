getwd()
setwd("D:\\mypro\\assingments\\Logistic regression")

install.packages("AER")
library(AER)
data("Affairs",package = "AER")

df <- Affairs
View(df)
dim(df)

str(df)
colSums(is.na(df))

model<-glm(df$affairs~factor(df$gender)+df$age+df$yearsmarried+factor(df$children)+df$religiousness+df$education+df$occupation+df$rating)
summary(model)
#find coeificent
coefor<-exp(coef(model))
coefor
#find the probability value by bench mark 0.5
prob<-predict(model,type = c("response"),df)
prob

#now calculate confusion matrix
confusion<-table(prob>0.5,df$affairs)
confusion

#check the accuracy of model
accuracy=(119+24+15+18+38+37)/601
accuracy*100#not quite good>0.8

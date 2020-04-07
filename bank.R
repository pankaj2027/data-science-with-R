#Output variable -> y
#y -> Whether the client has subscribed a term deposit or not 
#Binomial ("yes" or "no")
getwd()
setwd("D:\\mypro\\assingments\\Logistic regression")

df<-read.csv(file.choose(),sep = ";")
dim(df)
colnames(df)
View(df)
str(df)
colSums(is.na(df))#no Na's
summary(df)
attach(df)
#seperating df category
#required to change categorical to numeric 
#by one hot encoding

df1<-data.frame(job,marital,education,default,housing,loan,contact,month,poutcome)
View(df1)
dim(df1)

install.packages("dummy")
library(dummy)
df2=dummy(df1)
dim(df2)
View(df2)
df5<-data.frame(y,age,balance,day,duration,campaign,pdays,previous)

df_final<-cbind(df5,df2)
colnames(df_final)
dim(df_final)
View(df_final)
#model<-glm(y~df_final,drop.unused.levels=TRUE)
model2<-glm(y~df5+df2)
model1<-glm(y~age+balance+day+duration+campaign+pdays+previous+job_admin+job_blue.collar+
              job_entrepreneur+job_housemaid+job_management+job_retired+job_self.employed+
              job_services+job_student+job_technician+job_unemployed+job_unknown+
              marital_divorced+marital_married+marital_single+education_primary+education_secondary+
              education_tertiary+education_unknown+default_no+default_yes+housing_no+housing_yes+loan_no+
              loan_yes+contact_cellular+contact_telephone+contact_unknown+month_apr+month_aug+month_dec+
              month_feb+month_jan+month_jul+month_jun+month_mar+month_may+month_nov+month_oct+month_sep+
              poutcome_failure+poutcome_other+poutcome_success+poutcome_unknown)


#converting categorical into numeric
levels(marital)
Marital<-factor(as.numeric(marital))
Marital
levels(Marital)[1]<-"0"
levels(Marital)[2]<-"1"
levels(Marital)[3]<-"2"
Marital


#converting categorical into numeric
levels(education)
edu<-factor(as.numeric(education))
edu
levels(edu)
levels(edu)[1]<-"0"
levels(edu)[2]<-"1"
levels(edu)[3]<-"2"
levels(edu)[4]<-"3"
edu

#converting categorical into numeric
levels(default)

def<-factor(as.numeric(default))
def
levels(def)[1]<-"0"
levels(def)[2]<-"1"
def

#converting categorical into numeric
levels(housing)

Housing<-factor(as.numeric(housing))
Housing
levels(Housing)[1]<-"0"
levels(Housing)[2]<-"1"
Housing

#converting categorical into numeric
levels(loan)

Loan<-factor(as.numeric(loan))
Loan
levels(Loan)[1]<-"0"
levels(Loan)[2]<-"1"
Loan


#converting categorical into numeric
levels(contact)

Contact<-factor(as.numeric(contact))
Contact
levels(Contact)[1]<-"0"
levels(Contact)[2]<-"1"
levels(Contact)[3]<-"2"
Contact




#converting categorical into numeric
levels(poutcome)

Pout<-factor(as.numeric(poutcome))
Pout
levels(Pout)[1]<-"0"
levels(Pout)[2]<-"1"
levels(Pout)[3]<-"2"
levels(Pout)[4]<-"3"
Pout



#converting categorical into numeric
levels(y)
Y<-factor(as.numeric(y))
Y
levels(Y)[1]<-"0"
levels(Y)[2]<-"1"


df_byfactor=data.frame(Marital,edu,def,Housing,Contact,Loan,Pout)
View(df_byfactor)
#month will not affect the model
df_final2<-cbind(df5,df_byfactor)
colnames(df_final2)
View(df_final2)
#it works
model_ffinal<-glm(df5$y~df5$age+df5$balance+df5$day+df5$duration+df5$campaign+df5$pdays+df5$previous+df_byfactor$Marital+df_byfactor$edu+df_byfactor$def+df_byfactor$Housing+df_byfactor$Contact+df_byfactor$Loan+df_byfactor$Pout,data=df_final2,family=binomial)
summary(model_ffinal)                    
install.packages("mice")#webinar on mice....data imputation
library(mice)

#calculate odds ratio
exp(coef(model_ffinal))#regular value of each coeificients

#confusion matrix to represent probability values
prob<-predict(model_ffinal,type = c("response"),df_final2)
prob#probability values i.e. p=1/1+e(^-y)

#but is our model gives accuracy???Probabaly not

#to check matches and mis matches we use confusion matrix

confusion<-table(prob>0.5,df_final2$y)#cutoff value with actual values 
confusion
#here 38994+1722====correct predictions
##928+3567====incorrect predictions

#check the correct accuracy of our model
#accuracy=(correct prediction/Total no of predictions)*100
((38994+1722)/45211)*100
#cheersss 90.05% accurate model
accuracy=sum(diag(confusion)/sum(confusion))
accuracy

#precision,sensitivity(or recall),Specificity
precision=1722/(1722+3567)
precision#32% +ve

sensitivity=1722/(1722+38994)
sensitivity#4% correctly predicted +ve labels

specificity=928/(928+3567)
specificity#20% correctly predicted -ve labels

fp=1-specificity
fp#type 1 error

fn=1-sensitivity
fn#type-2 error


f1=2*((precision*sensitivity)/(precision+sensitivity))
f1#measures the balance of precision and sensitivity

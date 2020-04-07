#####emails #####
sms_raw <- read.csv(file.choose())
View(sms_raw)
##examine the type of variable
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
table(sms_raw$type)

###plot the graph
barplot(table(sms_raw$type),col ="green",main = 'Bar graph for sms type',xlab = "sms type",ylab = "number of Emails")

####build the corpus using the text mining package
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$type))

##clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus,tolower)
corpus_clean <- tm_map(corpus_clean,removeNumbers)
corpus_clean <- tm_map(corpus_clean,removeWords,stopwords())
corpus_clean <- tm_map(corpus_clean,removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)
corpus_clean <- tm_map(corpus_clean,PlainTextDocument)

###createthe document term matrix
sms_dtm <- DocumentTermMatrix(corpus_clean )

#creating trainng and test datasets
sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]

sms_corpus_train <- corpus_clean[1:4169,]
sms_corpus_test <- corpus_clean[4170:5559,]


####indication for frequent word
sms_dict <- findMostFreqTerms(sms_dtm_train,5)

sms_train <- DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))

####convert counts to a factor
convert_counts <- function(x){
  x <- ifelse(x,0,1,0)
  x <- factor(x,levels = c(0,1),labels=c("no","yes"))
  
}
 ###apply()convert_counts()tocolumns of train and test data
sms_train <- apply(sms_train,MARGIN=2,convert_counts)
sms_test<- apply(sms_train,MARGIN=2,convert_counts)


##training the modelon the data
library(e1071)
sms_classifier <- naiveBayes(sms_train,sms_raw_train$type)

##evaluating model performance
sms_test_pred <- pred(sms_classifier,sms_test)

library(gmodels)
CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,prop.r = FALSE,dnn = c('predict','actual'))
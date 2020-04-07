sms_raw <- read.csv(file.choose())
sms_raw$type <- factor(sms_raw$type)
table(sms_raw$type)
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:2])
lapply(sms_corpus[1:2], as.character)
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
#stopwords() return a vector of stop words. Removewords() function removes the stop words.
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
library(SnowballC)
#strip the suffix of learn, learned, learning to learn.
wordStem(c("learn", "learned", "learning", "learns"))
#apply wordStem() function to the entire sms_corpus
sms_corpus_clean  <- tm_map(sms_corpus_clean,
                            stemDocument)

lapply(sms_corpus[1:3], as.character)

lapply(sms_corpus_clean[1:3], as.character)

#word cloud visualization
#wordcloud () function place frequent words at random position at each run. Set.seed() can be used to create consistant graph.
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE, colors=brewer.pal(8, "BrBG"))
spam <- subset(sms_raw, type == "spam")
ham  <- subset(sms_raw, type == "ham")
wordcloud(spam$text, max.words = 50, scale = c(3, 0.5))

wordcloud(ham$text, max.words = 50, scale = c(3, 0.5))
# create DTM
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# alternative solution to create DTM and clean corpus in one run
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

sms_dtm_train <- sms_dtm[1:4169, ] # Training dataset
sms_dtm_test  <- sms_dtm[4170:5559, ] #Test dataset


sms_train_labels <- sms_raw[1:4169, ]$type #Training Label
sms_test_labels  <- sms_raw[4170:5559, ]$type # Test Label

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 0.999)
#That is, 1-0.999=0.001*6000=6. 6 or less, remove.

sms_dtm_freq_train
# another way to remove less frequence words
# find frequence words that appear at least 5 times
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

# create DTMs with only the frequent terms
# only selecting frequent words for training and testing
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

#If counts are greater than 0, convert into "Yes", otherwise, "NO"
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# apply() to convert DTM dataset that has yes and no into numerical data. 
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

sms_test_pred <- predict(sms_classifier, sms_test)

head(sms_test_pred)

library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
###accuary <- 97.62%%

sms_classifier3 <- naiveBayes(sms_train, sms_train_labels, laplace = 3)
sms_test_pred3 <- predict(sms_classifier3, sms_test)
CrossTable(sms_test_pred3, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
###accuary <- 96%
sms_classifier4 <- naiveBayes(sms_train, sms_train_labels, laplace = 2)
sms_test_pred4 <- predict(sms_classifier4, sms_test)
CrossTable(sms_test_pred4, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
##accuracy=0.9733813
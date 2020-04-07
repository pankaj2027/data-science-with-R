
######amazom reviews###############################################################################

library(rvest)
library(XML)
library(magrittr)
review <- 'https://www.amazon.in/Apple-iPad-Wi-Fi-32GB-Gold/product-reviews/B07C51NY6G/ref=cm_cr_dp_d_show_all_top?ie=UTF8&reviewerType=all_reviews'
ipad_reviews <- NULL
for (i in 1:10) {
  murl<- read_html(as.character(paste(review,i,sep="=")))
  rev <- murl %>%
    html_nodes(".a-size-base") %>%
    html_text()
  ipad_reviews <- c(ipad_reviews,rev)
}
write.table(ipad_reviews,'ipad.txt',row.names = F)

==================================================================================================================================================================================================================================================
  #### word cloud####
install.packages("rJava")
install.packages("tm")
install.packages("Snowballc")
install.packages("RWeka")
install.packages("textir")
install.packages("qdap")
install.packages("maptpx")
library(rJava)
library(tm)
library(SnowballC)
library(RWeka)
library(wordcloud)
library(qdap)
library(textirnb    )
library(maptpx)
library(data.table)
library(stringr)
library(slam)
library(ggplot2)

pos.word =scan(file.choose(),what="character",comment.char = ";")
neg.word=scan(file.choose(),what="character",comment.char = ";")

#####loading file to analysis
ipad=readLines(file.choose())
ipad <- stemDocument(ipad)

####preparing the corpus from hesedocument
ipad1 <- Corpus(VectorSource(ipad))

# Data Cleansing
ipad1 <- tm_map(ipad1, tolower)
inspect(ipad1[1])

ipad1<- tm_map(ipad1, removePunctuation)
inspect(ipad1[1])

inspect(ipad1[300])
ipad1 <- tm_map(ipad1, removeNumbers)
inspect(ipad1[300])

ipad1 <- tm_map(ipad1, removeWords, stopwords('english'))
inspect(ipad1[1])

#striping white spaces

ipad1 <- tm_map(ipad1, stripWhitespace)
inspect(x1[1])
# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(ipad1)
tdm
dtm <- t(tdm)

tdm <- as.matrix(tdm)
tdm[100:109, 1:10]

tdm[1:20, 1:20]

# Read the third review
inspect(ipad[1])

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 30)
w_sub

barplot(w_sub, las=3, col = rainbow(20))

# Term review repeats in all most all documents
ipad1 <- tm_map(ipad1, removeWords, 'review','comment','ipad')
ipad1 <- tm_map(ipad1, stripWhitespace)

tdm <- TermDocumentMatrix(ipad1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109, 1:20] %>%

# Word cloud
#install.packages("wordcloud")
library(wordcloud)
windows()
wordcloud(words = names(w_sub), freq = w_sub) # wordcloud with only subset of words

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order = F, colors = rainbow(20), scale=c(3,1), rot.per = 0.3)

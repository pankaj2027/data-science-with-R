library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
install.packages('devtools')

library(devtools)

# IMDBReviews #############################
aurl <- "https://www.imdb.com/title/tt7485048/reviews?ref_=tt_ql_3"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)

write.table(IMDB_reviews,"Super30.txt",row.names = F)


Super30 <- read.delim('Super30.txt')
str(Super30)

View(Super30)

# Build Corpus and DTM/TDM
library(tm)
corpus <-Super30[-1,]
head(corpus)

class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

corpus_clean<-tm_map(corpus,stripWhitespace)

inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))

inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords, c('can','film'))
cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
# the barplot pulls both character and characters as separate words. this should be 
# counted as one as both holds the same synonym.

inspect(cleanset[1:5])
#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm

# the terms indicate that there are 13649 words and 393036 documents(# of tweets) in this TDM
# Sparsity is 97% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 50) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# the word story , hrithik as the highest frequency. This implies
# that Movie super30 has got more reviews about the hrithik and 
# most of them liked the movie.




# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, 
           minSize = 1)

# lettercloud 

letterCloud(w,word = 'A',frequency(5), size=1)

# lOADING +VE AND -VE dictonaries
pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words = c(pos.words,"wow", "kudos", "hurray") # including our own positive words to the existing list

# Positive wordcloud
pos.matches = match(names(w), c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- w[pos.matches]
p_names <- names(freq_pos)
windows()
wordcloud(p_names,freq_pos,scale=c(4,1),colors = rainbow(20))

# Negative wordcloud
neg.matches = match(names(w), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- w[neg.matches]
n_names <- names(freq_neg)
windows()
wordcloud(n_names,freq_neg,scale=c(5,1),colors = brewer.pal(8,"Dark2"))



# Sentiment Analysis for tweets:
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# install.packages("syuzhet")

# Read File 
IMDB_reviews <- read.delim('Super30.TXT')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)

head(s)
reviews[4]
get_nrc_sentiment('splendid')

# Splendid has one Joy and one positive 
get_nrc_sentiment('no words') #1 Anger and 1 Negative


# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for super30')





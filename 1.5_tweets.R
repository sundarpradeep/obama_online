# Twitter Analytics for Obama


# Load files for sentiment Analysis

pos = scan(file="positive-words.txt",what="charcter", comment.char=";")
neg = scan(file="negative-words.txt",what="charcter", comment.char=";")

source("./1.1_score_sentiment.R")


# Collect Tweets

setup_twitter_oauth(consumer_key='yenzHVLMH3Gcffc12IGU8IAW0', 
                    consumer_secret='S9fxZGuuwogG8PsDs9012NCWoSKjM53E0q6wSHnYI1mh2Mbav2', 
                    access_token='24249947-rP9arpAgCLryQdUztxIY1tizHTX8GucanFkwyf8DA', 
                    access_secret='m5JeS5trPYAX7MqycAD9fy4TIdknvkAlsrYj5Bko9ZVMx')



official.tweets <- searchTwitter("@Barakobama",n=200000)

#Process official Tweets

official.df <- ldply(official.tweets,function(x) x$toDataFrame())
official_corpus <- Corpus(VectorSource(official.df$text))
official_corpus <- tm_map(official_corpus,content_transformer(function(x) iconv(x, 
                                                                                to='UTF-8-MAC', sub='byte')), mc.cores=1)

official_corpus <- tm_map(official_corpus, content_transformer(tolower), mc.cores=1) 
official_corpus <- tm_map(official_corpus, removePunctuation)
official_corpus <- tm_map(official_corpus, function(x) removeWords(x,stopwords()))
official_corpus <- tm_map(official_corpus, removeWords, c("barakobama","obama"))
official_corpus <- tm_map(official_corpus, removeNumbers)

official.tweets.df<-data.frame(text=unlist(sapply(official_corpus, `[`, "content")), 
                               stringsAsFactors=F)
official_corpus <- tm_map(official_corpus,stemDocument)


# official sentiment score


official.scores <- score.sentiment(official.tweets.df$text,pos,neg) 
official.scores$type <- "official"

# official Dendogram

official.tdm <- TermDocumentMatrix(official_corpus,
                                   control=list(wordLengths=c(1,Inf)))

official.tdm2 <- removeSparseTerms(official.tdm,sparse=0.95)

official.matrix <- as.matrix(official.tdm2)
official.dist <- dist(scale(official.matrix))
official.fit <- hclust(official.dist,method="ward.D2")


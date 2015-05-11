# set-up functions 

BigramTokenizer <- function(x) NGramTokenizer(x, 
                                              Weka_control(min = 2, max = 2))

process_sotu <- function(sotu.in,year) { 
    
    sotu.in <- toString(sotu.in)
    
    corpus <- Corpus((VectorSource(sotu.in)))  %>%
        tm_map(.,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))  %>%
        tm_map(.,content_transformer(tolower)) %>%
        tm_map(., removePunctuation)  %>%
        tm_map(., function(x) removeWords(x,stopwords())) %>% 
        tm_map(., removeWords, c("applause")) %>% 
        tm_map(., removeNumbers)  %>% 
        tm_map(., stemDocument)
    
    bitdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))  %>%
        removeSparseTerms(0.75)
    
    bigrams <- tbl_df(data.frame(cbind(as.numeric(unlist(bitdm[[3]])),unlist(bitdm[[6]][1])),stringsAsFactors =F))
    names(bigrams) <- c("freq","bigram")
    bigrams$freq <- as.numeric(bigrams$freq)
    bigrams$year <- year
    bigrams.out <- arrange(bigrams,desc(freq)) %>% filter(freq > 1 )
    
    return(bigrams.out)
}

# Process 2010 SOTU 

url2010 <- html('https://www.whitehouse.gov/the-press-office/remarks-president-state-union-address')
sotu2010 <-  url2010 %>%  html_nodes(xpath = '//*[(@id = "content")]//p') %>% html_text() 
bigrams.2010 <- process_sotu(sotu2010,2010)
head(bigrams.2010)

# Process 2011 SOTU 

url2011 <- html('https://www.whitehouse.gov/the-press-office/2011/01/25/remarks-president-state-union-address')
sotu2011 <-  url2011 %>%  html_nodes(xpath = '//*[(@id = "content")]//p') %>% html_text() 
bigrams.2011 <- process_sotu(sotu2011,2011)
head(bigrams.2011)


# Process 2012 SOTU 

url2012 <- html('https://www.whitehouse.gov/the-press-office/2012/01/24/remarks-president-state-union-address')
sotu2012 <-  url2012 %>%  html_nodes(xpath = '//*[(@id = "content")]//p') %>% html_text() 
bigrams.2012 <- process_sotu(sotu2012,2012)
head(bigrams.2012)

# Process 2013 SOTU 

url2013 <- html('https://www.whitehouse.gov/the-press-office/2013/02/12/remarks-president-state-union-address')
sotu2013 <-  url2013 %>%  html_nodes("#content div+ div") %>% html_text() 
bigrams.2013 <- process_sotu(sotu2013,2013)
head(bigrams.2013)

# Process 2014 SOTU 

url2014 <- html('https://www.whitehouse.gov/the-press-office/2014/01/28/president-barack-obamas-state-union-address')
sotu2014 <-  url2014 %>%  html_nodes(xpath = '//*[(@id = "content")]//p') %>% html_text() 
bigrams.2014 <- process_sotu(sotu2014,2014)
head(bigrams.2014)

# Process 2015 SOTU 

url2015 <- html('https://www.whitehouse.gov/the-press-office/2015/01/20/remarks-president-state-union-address-january-20-2015')
sotu2015 <-  url2015 %>%  html_nodes(xpath = '//*[(@id = "content")]//p') %>% html_text() 
bigrams.2015 <- process_sotu(sotu2015,2015)
head(bigrams.2015)


# Merge and spread 

bigrams <- rbind(bigrams.2010,
                       bigrams.2011,
                       bigrams.2012,
                       bigrams.2013,
                       bigrams.2014,
                       bigrams.2015)

#obama_bigrams$year <- as.Date(as.character(obama_bigrams$year),"%Y")

#str(obama_bigrams)


#write.csv(file="./bigrams.csv",obama_bigrams,row.names=F)
#bigrams <- read.csv(file="bigrams.csv")


term_bigrams <- bigrams %>% spread(year,freq,fill="0",convert = T) 

names(term_bigrams) <- c("bigram","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015") 

#head(term_bigrams)

term_bigrams <- mutate(term_bigrams,
                       total = Y2010 + Y2011 + Y2012 + Y2013 + Y2014 + Y2015)  %>% 
    filter(total >= 12)  %>% select(-total)  %>%  gather(yr,count, -bigram)

bigrams.scaled <- ddply(term_bigrams, .(yr), transform, rescale = rescale(count))


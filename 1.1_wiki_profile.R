
require("rvest")

urlbio <- html('https://en.wikipedia.org/wiki/Barack_Obama')

profile <-  urlbio %>%  html_nodes(xpath = '//*[@id="mw-content-text"]/p[1]') %>% html_text() 

rm(urlbio)
 

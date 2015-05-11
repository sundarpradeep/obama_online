
library(taRifx.geo)

googletrend::setdownloaddir('/Users/sundar/Downloads')

#install_github('googletrend','okugami79')

gtrend.regions <- gettrend(keyword='obama',plot=F,simple=F) 

geo.trend <- data.frame(gtrend.regions$top.region)
news.trend <- data.frame(gtrend.regions$trend)


# geo-code

options(BingMapsKey='AgyqIM0pl1I8dJr8Tfb7efnQAChYXjdaSi73yGP1XVDITzldbFAaJKzTfZR3QzJX')


gtrends.coordinates <- sapply(geo.trend$region,
                      function(x) taRifx.geo::geocode(x,service='bing',returntype='coordinates'))

#                        function(x) geocode(x,returntype='coordinates'))

gtrends.coordinates <- as.data.frame(t(as.data.frame(gtrends.coordinates)))

gtrends.geo <- cbind(geo.trend,gtrends.coordinates)
names(gtrends.geo)[3:4] <- c("lat","lon")


world.map <- map_data('world')



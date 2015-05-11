# Download contributors data and read  
# https://www.opensecrets.org/pres12/contrib.php?id=N00009638
# https://www.opensecrets.org/pres12/contrib.php?id=N00000286

donors <- read.xlsx("top_donors.xlsx",sheetIndex=1,header=T)

donor.network<-graph.data.frame(donors, directed=F)
#V(donor.network)
#E(donor.network) 
#degree(donor.network)

V(donor.network)$color<-ifelse(V(donor.network)$name=='Romney', 'red', 'blue') 
V(donor.network)$size<-degree(donor.network)/5
E(donor.network)$color<-ifelse(E(donor.network)$Party=='R', "red", "blue")

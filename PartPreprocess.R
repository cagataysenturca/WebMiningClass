package <- c("rvest", "data.table" ,"XML" , "httr" , "ggplot2"  , "dplyr" , "ggmap" , "rworldmap" , "purrr")
install.packages(package)
install.packages("ggrepel")
library(dplyr)
library(rvest)
library(XML)
library(data.table)
library(httr)
library(ggplot2)
library(purrr)
library(rworldmap)
library(ggmap)


url <- "https://www.sahibinden.com/satilik/istanbul?pagingOffset=%i"

map_df(1:70, function(i) {
  
  page <- read_html(sprintf(url,i))
  
  data.frame(TYPE = html_text(html_nodes(page, " .searchResultsTagAttributeValue ")),
             TITLE = html_text(html_nodes(page, " .classifiedTitle ")),
             M2 = html_text(html_nodes(page, " .searchResultsTitleValue+ .searchResultsAttributeValue ")),
             PRICE = html_text(html_nodes(page, " .searchResultsPriceValue div ")),
             LOCATION = html_text(html_nodes(page, " .searchResultsLocationValue "))
  )              
  
  
}) -> emlak


emlak [  ,4] <- gsub("\\." , "" ,  emlak[,4])
emlak [  ,4] <- gsub("TL" , "" ,  emlak[,4])

emlak$M2 <- as.numeric(emlak$M2)
emlak$PRICE <- as.numeric(emlak$PRICE)
emlak$TYPE <- as.factor(emlak$TYPE)



for(i in 1:nrow(emlak))
  
{
  
  result <- geocode(emlak$LOCATION[i], output = "latlona", source = "google")
  
  emlak$lon[i] <- as.numeric(result[1])
  
  emlak$lat[i] <- as.numeric(result[2])
  
}


install.packages("leaflet")
library(leaflet)
library(sp)

emlak<- emlak[complete.cases(emlak),]
emlak.SP <- SpatialPointsDataFrame(emlak[,c(6,7)],emlak[,-c(6,7)])

#emlak <-read.csv("C:\\Users\\asli\\Desktop\\emlak.csv")


m <- leaflet() %>%
  
  addTiles() %>%
  
  
  
  addProviderTiles("Esri.WorldTopoMap")%>%
  addMarkers(data = emlak, lng = ~lon, lat=~ lat, popup = ~paste(TITLE,"<br>",PRICE),
             labelOptions = labelOptions(noHide = F, direction = 'auto'),
             options = markerOptions(riseOnHover = TRUE),
             clusterOptions = markerClusterOptions()
  )

m





write.csv(emlak, file = "emlak.csv")



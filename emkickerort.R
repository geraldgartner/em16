library(leaflet)
library(dplyr)
emkickergeocode <- read.csv("~/Google Drive/dStd.at/em16/data/emkickergeocode.csv")
kicker <- emkickergeocode

kicker_popup <- paste0("<strong>Klub: </strong>", 
                      kicker$klub, 
                      "<br><strong>Spieler: </strong>", 
                      kicker$name, 
                      "<br><strong>Nationalteam: </strong>", 
                      kicker$natioanlteam)

library("leaflet") 
leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(9.004452, 5.800781, zoom = 2) %>% 
  addMarkers(data = kicker, lat = ~ lat, lng = ~ lng, popup = kicker_popup, clusterOptions = markerClusterOptions())


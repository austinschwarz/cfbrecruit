#'displays a leaflet map of all selected players in DF
#'
#'@param players a data.frame of players, has to have Hometown column.
#'
#'@return nothing, prints a leaflet
#'
#'@import jsonlite
#'@import dplyr
#'@import stringr
#'@import purrr
#'@import tidyverse
#'@import sys
#'
#'
#'@export

map_of_players <- function(players){
  
  coords <- data.frame(lon=double(),
                       lat=double())
  
  players <- players %>% separate(Hometown,into=c("City","State"),sep=",")
  players$City <- str_replace(players$City," ","-") %>% str_replace("D ", "D'")
  
  for (i in 1:nrow(players)){
    city <- players[i,]$City %>% trimws()
    state <- players[i,]$State %>% trimws()
    if (state == "AUST"){
      coords[i,] <- c(as.numeric(144.5747),as.numeric(-37.8136))
    }
    else if (state == "GERM"){
      coords[i,] <- c(as.numeric(13.40), as.numeric(52.52))
    }
    else{
      build_html <- paste("https://nominatim.openstreetmap.org/search?city=",city,"&state=",state,"&CountryCodes=USA&limit=9&format=json",sep="")
      jsonFile <- fromJSON(build_html)[1,]
      coords[i,] <- c(as.numeric(jsonFile$lon),as.numeric(jsonFile$lat))
    }
    Sys.sleep(0.5)
  }
  
  players <- cbind(players,coords)
  players$City <- str_replace(players$City,"-"," ")
  
  leaflet() %>%
    addTiles() %>%
    addCircles(lng = players$lon,lat= players$lat,popup = paste("Name: ", players$Player, "<br>",
                                                  "Position: ", players$positions, "<br>",
                                                  "HS: ", players$HS, "<br>",
                                                  "Hometown: ", players$City, "<br>",
                                                  "Rating: ", players$score, "<br>",
                                                  "Stars: ", players$stars, "<br>",
                                                  "Commit Status: ", players$commit_status))
}

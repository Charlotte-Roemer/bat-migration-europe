
library(maps)
library(tidyverse)
library(sf)
library(leaflet)
library(rnaturalearth) 

# Use country level or subregions (e.g. Norfolk for the UK) ?
Level = "country"

# Load Protocol data
Site_localite = read_delim("C:/Users/croemer01/Documents/Post-Doc/World recording standards/World_recording_standards_simplified.csv", delim=";")

Site_localite2 = Site_localite %>% 
  filter (Protocol == "Fix point") %>% 
  rename_at('Country', ~'admin') %>% 
  mutate(admin = recode(admin, 'UK' = "United Kingdom"),
         value=1) %>% 
  as.data.frame()

# Load World GIS limits
# world_sf = sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)) %>% 
#   rename_at('ID', ~'region')
if(Level == "country"){
  world_sf <- ne_countries(returnclass = "sf", scale = "large")
}else{
  world_sf <- ne_states(returnclass = "sf")
}

# Load UK regions
Norfolk_sf = ne_states(country = "united kingdom", returnclass = "sf") %>% 
  filter(name_en=="Norfolk")

# Merge protocol and GIS limits and crop Norfolk
Site_localite_world = Site_localite2 %>%
  right_join(world_sf) %>% 
  st_as_sf %>% 
  mutate(value = ifelse(is.na(value), 0,1)) 

TEST = world_sf %>%
  filter(world_sf$admin=="United Kingdom")

diffPoly <- TEST %>%
  filter(name!="Norfolk")


# Plot
ggplot() +
  geom_sf(data=world_sf) +
  geom_sf(data=diffPoly)

bins <- c(0, 0.5, 1)
pal <- colorBin("YlOrRd", domain = Site_localite_world$value, bins = bins)

my.map <-
  leaflet( options = leafletOptions( minZoom = 2 ) ) %>%
  setMaxBounds( lng1 = -180
                , lat1 = -89.98155760646617
                , lng2 = 180
                , lat2 = 89.99346179538875 ) %>%
  addTiles(  ) %>%
  addPolygons( data = Site_localite_world
               , fillColor = ~pal(Site_localite_world$value)
               #, fill = "#D24618"
               , color = "white"
               , opacity = 1
               , fillOpacity = 0.7
               , weight = 3
               , popup = paste0(
                 "<b>Country: </b>"
                 , Site_localite_world$admin
                 , "<br>"
                 , "<b>Affiliation: </b>"
                 , Site_localite_world$Affiliation
                 , "<br>"
                 , "<a href='"
                 , Site_localite_world$'Program website'
                 , "' target='_blank'>"
                 , "Click here to access protocol website</a>"
                 , "<br>"
                 , "<b>Possible recorders: </b>"
                 , Site_localite_world$Recorder
                 , "<br>"
                 , "<b>Start time: </b>"
                 , Site_localite_world$'Start time'
                 , "<br>"
                 , "<b>End time: </b>"
                 , Site_localite_world$'End time'
               )
               , label = ~admin
               , labelOptions = labelOptions(
                 style = list("font-weight" = "normal"
                              , padding = "3px 8px"
                              , textsize = "15px"
                              , direction = "auto" ) )
               , highlightOptions = highlightOptions( 
                 color = "#10539A"
                 , weight = 3
                 , fillColor = "#10539A"
               ))



# Save
mapshot(m, url = paste0("C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Participations/map.html"))




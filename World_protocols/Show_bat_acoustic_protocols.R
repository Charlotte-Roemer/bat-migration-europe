
library(maps)
library(tidyverse)
library(sf)
library(leaflet)
library(rnaturalearth) 
library(mapview)

sf_use_s2(FALSE) # without this option, st_union produces lines on the map because of fidji and russia

# Load Protocol data
Table_data = read_delim("C:/Users/croemer01/Documents/Post-Doc/World recording standards/World_recording_standards_simplified.csv", delim=";",
                        locale = locale(encoding = "windows-1252")) # encoding because of special characters

Table_data_Country = Table_data %>% 
  filter (Protocol == "Fix point") %>% 
  filter (is.na(Region)) %>% 
  rename_at('Country', ~'admin') %>% 
  mutate(admin = recode(admin, 'UK' = "United Kingdom"),
         value=1) %>% 
  as.data.frame()

Table_data_Region = Table_data %>% 
  filter (Protocol == "Fix point") %>% 
  filter (!is.na(Region)) %>% 
  rename_at('Region', ~'region') %>% 
  mutate(value=1) %>% 
  as.data.frame()

# Load World GIS limits
world_sf <- ne_states(returnclass = "sf")
#TEST = giscoR::gisco_get_countries()

# Merge National protocols and GIS limits
names(Table_data_Country) = make.names(names(Table_data_Country))
GIS_Country = Table_data_Country %>%
  right_join(world_sf, by="admin") %>% 
  st_as_sf %>% 
  mutate(value = ifelse(is.na(value), 0,1)) %>% 
  mutate(value = ifelse((is.na(region_cod) | region_cod != "ES.CN") 
                        & (is.na(type) | type !="Overseas département"), value, 0)) %>% # Remove value = 1 in overseas regions
  select(admin, value, Affiliation, Program.website, Recorder, Timing) %>% 
  group_by(admin, value, Affiliation, Program.website, Recorder, Timing) %>% 
  summarize(geometry = st_union(geometry))

# Merge Regional protocols and GIS limits
names(Table_data_Region) = make.names(names(Table_data_Region))
GIS_region1 = Table_data_Region %>% # when region is in the "region" column
  inner_join(world_sf, by="region") %>% 
  st_as_sf %>% 
  mutate(value = ifelse(is.na(value), 0,1)) %>% 
  group_by(region, value, Affiliation, Program.website, Recorder, Timing) %>% 
  summarize(geometry = st_union(st_make_valid(geometry)))
GIS_region2 = Table_data_Region %>% # when region is in the "name" column
  inner_join(world_sf, by=c("region" = "name")) %>% 
  st_as_sf %>% 
  mutate(value = ifelse(is.na(value), 0,1)) %>% 
  group_by(region, value, Affiliation, Program.website, Recorder, Timing) %>% 
  summarize(geometry = st_union(st_make_valid(geometry)))
GIS_region = rbind(GIS_region1, GIS_region2) %>% 
  rename_at('region', ~'admin')

# Add regional protocols to world map
GIS_world = rbind(GIS_Country, GIS_region)

TEST = world_sf %>%
  filter(world_sf$name=="Norfolk") %>% 
  as.data.frame()

# # Plot
# ggplot() +
#   geom_sf(data=world_sf) +
#   geom_sf(data=diffPoly)

bins <- c(0, 0.5, 1)
pal <- colorBin("YlOrRd", domain = GIS_world$value, bins = bins)

my.map <-
  leaflet( options = leafletOptions( minZoom = 2 ) ) %>%
  setMaxBounds( lng1 = -180
                , lat1 = -89.98155760646617
                , lng2 = 180
                , lat2 = 89.99346179538875 ) %>%
  addTiles(  ) %>%
  addPolygons( data = GIS_world
               , fillColor = ~pal(GIS_world$value)
               #, fill = "#D24618"
               , color = "white"
               , opacity = 1
               , fillOpacity = 0.7
               , weight = 3
               , popup = ifelse(GIS_world$value == 1, paste0(
                 "<b>Country: </b>"
                 , GIS_world$admin
                 , "<br>"
                 , "<b>Affiliation: </b>"
                 , GIS_world$Affiliation
                 , "<br>"
                 , "<a href='"
                 , GIS_world$Program.website
                 , "' target='_blank'>"
                 , "Protocol website</a>"
                 , "<br>"
                 , "<b>Possible recorders: </b>"
                 , GIS_world$Recorder
                 , "<br>"
                 , "<b>Timing: </b>"
                 , GIS_world$Timing
                 )
                 , paste0("No citizen science protocol available"))
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
mapshot(my.map, url = paste0("C:/Users/croemer01/Documents/R/bat-migration-europe/bat-migration-europe/World_protocols/map.html"))




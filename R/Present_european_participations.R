

library(dplyr)
library(maps)
library(tidyverse)
library(mapview)
library(sf)
library(ggplot2)
library(viridis)


# Load study sites (beware that some of them were not uploaded)
Site_localite = read_delim("C:/Users/croemer01/Documents/Donnees vigie-chiro/sites_localites.csv", delim="\t")

# Load user name (to obtain confidentiality status of data)
User = read_delim("C:/Users/croemer01/Documents/Donnees vigie-chiro/utilisateurs.txt", delim="\t")

# Merge with user confidentiality
Site_localite2 = merge (Site_localite, 
                              User[,6:7], 
                              by.x=c("id_observateur"), 
                              by.y=c("identifiant"), 
                              all.x=T)

# Subset only data that are not confidential and data from fix point protocol
Site_localite3=subset(Site_localite2, 
                            Site_localite2$confidentiel == "non" & 
                        Site_localite2$protocole=="POINT_FIXE")

# Load EUROPE GIS limits
countries <- c(
  "Andorra", "Belarus", "Bulgaria", "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands", "Norway(?!:Svalbard)", "Monaco", "Montenegro", 
  "Sweden", "Finland", "Latvia", "Lithuania", "Luxembourg", "Albania", "Serbia",
  "Denmark", "Poland", "Italy", "Cyprus", "Estonia", "Macedonia", 
  "Croatia", "Slovenia", "Hungary", "Slovakia", "Greece", "Ireland", "Kosovo", "Malta", "Moldova",
  "Czech republic", "Ukraine", "Romania", "Liechtenstein", 
  "Bosnia and Herzegovina", "Russia:32"
)

Europe <- map_data("world", region = countries)

# Used as label coordinate for country's names
region.lab.data <- Europe %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

# Make spatial objects
Site_localite_sf = st_as_sf(
  Site_localite3, coords = c("longitude", "latitude"), crs=4326, remove=FALSE) %>% 
  st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

Europe_sf = st_as_sf(
  Europe, coords = c("long", "lat"), crs=4326, remove=FALSE)  %>%
  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Crop data in European boundaries
Site_localite_Europe_sf = Site_localite_sf %>% 
  st_crop(st_bbox(Europe_sf))

# Make grid

g <- Europe_sf %>%
  st_make_grid(cellsize = 50000) %>%
  st_intersection(Europe_sf) %>%
  st_cast("MULTIPOLYGON")  %>%
  st_sf() %>%
  mutate(id = row_number())

participation_grid <- g %>%
  st_join(Site_localite_Europe_sf) %>%
  group_by(id) %>%
  summarize(num_sites = n())

#plot(participation_grid["num_sites"])

# Subset only squares that have at least 1 participation (it looks like the above operation adds 1 to each square, so substracting 1 back)
participation_grid2=participation_grid %>% 
  filter(num_sites!=1) %>% 
  mutate(num_sites=num_sites-1)

# Plot
ggplot() +
  geom_sf(data=Europe_sf, fill="white") +
  geom_sf(data=participation_grid2, aes(fill=num_sites), color= NA) +
  geom_text(data = region.lab.data, aes(label = region), size = 3, hjust = 0.5)

# Plot on dynamic map!
mapviewOptions(fgb = FALSE)
m = mapview(participation_grid2, zcol = "num_sites") 

mapshot(m, url = paste0("C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Participations/map.html"))


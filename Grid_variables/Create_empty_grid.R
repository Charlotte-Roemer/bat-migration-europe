
library(maps)
library(tidyverse)
library(sf)
library(beepr)

size_square = 500 # in meters
countries <- c("France") # choose one or several countries
n_parts = 20 # in how many parts do you want to save the grid?
GridName = "SysGrid_50km_de_cote"
Pathname = "C:/Users/croemer01/Documents/Donnees vigie-chiro/"
France_shape = "C:/Users/croemer01/Documents/SIG/Delimitations_pays/"

# Load country GIS limits
#GIS_limits <- map_data("world", region = countries) # attention, these limits are not very precise
GIS_limits <-read_sf(dsn=France_shape, layer="France_metropolitaine")

# Make spatial objects
GIS_limits_sf = st_as_sf(
  GIS_limits, coords = c("long", "lat"), crs=4326, remove=FALSE)  %>%
#  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Make grid
g <- GIS_limits_sf %>%
  st_make_grid(cellsize = size_square) %>%
  #st_intersection(GIS_limits_sf) %>%
  st_cast("MULTIPOLYGON")  %>%
  st_sf() %>%
  mutate(id = row_number())

# Subset only squares that are in the country
participation_grid <- g %>%
  st_join(GIS_limits_sf) %>%
  group_by(id)
participation_grid2=participation_grid %>% 
  filter(!is.na(group))

beep(2)

# # Convert to data frame
# g_dataframe = participation_grid2 %>%
#   mutate(long = unlist(map(participation_grid2$geometry,1)),
#          lat = unlist(map(participation_grid2$geometry,2)))
# 
# g_dataframe$x = 1
# #g_dataframe$id = 
# colnames(g_dataframe)=c("Group.1",	"Group.2",	"x",	"id")

# Cut in n parts
n = rep(1:n_parts, length.out=nrow(participation_grid2))
n = sort(n) 
Split_Grid <- split(participation_grid2, n)

names1 <- names(Split_Grid)

# Save
# for(i in seq_along(names1)){
#   fwrite(assign(names1[i], Split_Grid[[i]]), 
#          paste0(Pathname, GridName,"_part", names1[i], ".csv"))
# }
for(i in seq_along(names1)){
  st_write(assign(names1[i], Split_Grid[[i]]), 
         paste0(Pathname, GridName,"_part", names1[i], ".shp"))
}











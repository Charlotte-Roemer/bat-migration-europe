
library(maps)
library(tidyverse)
library(sf)
library(beepr)

size_square = 500 # in meters
countries <- c("France") # choose one or several countries
n_parts = 20 # in how many parts do you want to save the grid?
GridName = "SysGrid_500m_de_cote"
Pathname = "C:/Users/croemer01/Documents/Donnees vigie-chiro/"
France_shape = "C:/Users/croemer01/Documents/SIG/Delimitations_pays/"

# Load country GIS limits
#GIS_limits <- map_data("world", region = countries) # attention, these limits are not very precise
GIS_limits <-read_sf(dsn=France_shape, layer="France_metropolitaine") %>% 
  st_as_sf() %>% 
  st_transform(2154)

# Make grid
g <- GIS_limits %>%
  st_make_grid(cellsize = size_square, what = "centers", crs = 2154) %>%
  st_intersection(GIS_limits) 

beep(2)

# Cut in n parts
n = rep(1:n_parts, length.out=nrow(g))
n = sort(n) 
Split_Grid <- split(g, n)

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











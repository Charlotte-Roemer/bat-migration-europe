
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
points_per_part <- ceiling(length(g) / 20)  # Adjust '20' for the desired number of parts
points_geometry <- st_geometry(g)
split_indices <- split(seq_len(length(g)), 
                       rep(1:20, each = points_per_part, length.out = length(g)))
split_parts <- lapply(split_indices, function(indices) {
  st_sfc(points_geometry[indices])
})

names1 <- names(split_parts)

# Save
# for(i in seq_along(names1)){
#   fwrite(assign(names1[i], Split_Grid[[i]]), 
#          paste0(Pathname, GridName,"_part", names1[i], ".csv"))
# }
for(i in seq_along(names1)){
  st_write(assign(names1[i], Split_Grid[[i]]), 
           paste0(Pathname, GridName,"_part", names1[i], ".shp"))
}











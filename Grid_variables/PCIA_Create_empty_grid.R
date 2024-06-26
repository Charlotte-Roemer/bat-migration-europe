
library(maps)
library(tidyverse)
library(sf)
library(beepr)

size_square = 500 # in meters
countries <- c("France") # choose one or several countries
n_parts = 20 # in how many parts do you want to save the grid?
GridName = paste0("SysGrid_", size_square, "m_de_cote")
Pathname = "/mnt/beegfs/croemer/VigieChiro/SIG"
France_shape = "/mnt/beegfs/croemer/VigieChiro/SIG/"

# Load country GIS limits
#GIS_limits <- map_data("world", region = countries) # attention, these limits are not very precise
GIS_limits <-read_sf(dsn=France_shape, layer="France_metropolitaine") %>% 
  st_as_sf() %>% 
  st_transform(2154)

print("GIS_limits loaded")

# Make grid
g <- GIS_limits %>%
  st_make_grid(cellsize = size_square, what = "centers", crs = 2154) %>%
  st_intersection(GIS_limits) 

beep(2)

print("grid created")

# Cut in n parts
points_per_part <- ceiling(length(g) / n_parts)  
points_geometry <- st_geometry(g)
split_indices <- split(seq_len(length(g)), 
                       rep(1:n_parts, each = points_per_part, length.out = length(g)))
split_parts <- lapply(split_indices, function(indices) {
  st_sfc(points_geometry[indices])
})

names1 <- names(split_parts)

print("grid cut in n parts")

# Save
for(i in seq_along(names1)){
  fwrite(assign(names1[i], Split_Grid[[i]]),
         paste0(Pathname, GridName,"_part", names1[i], ".csv"))
}
# for(i in seq_along(names1)){
#   st_write(assign(names1[i], Split_Grid[[i]]), 
#            paste0(Pathname, "/", GridName,"_part", names1[i], ".shp"))
# }











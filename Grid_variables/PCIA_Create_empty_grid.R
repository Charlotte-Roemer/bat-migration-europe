
library(maps)
library(tidyverse)
library(sf)
library(beepr)
library(data.table)

size_square = 500 # in meters
countries <- c("France") # choose one or several countries
n_parts = 20 # in how many parts do you want to save the grid?
GridName = paste0("SysGrid_", size_square, "m_de_cote")
Pathname = "/mnt/beegfs/croemer/VigieChiro/SIG/"
France_shape = "/mnt/beegfs/croemer/VigieChiro/SIG/"
#France_shape = "C:/Users/croemer01/Documents/SIG/Delimitations_pays/"

# Load country GIS limits
# GIS_limits <- map_data("world", region = countries) %>%  # attention, these limits are not very precise
#   st_as_sf(coords = c("long", "lat"), crs=4326, remove=FALSE) %>%
#   group_by(group) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON") %>%
#   st_transform(2154)
GIS_limits <-read_sf(dsn=France_shape, layer="France_metropolitaine") %>%
  st_as_sf() %>% 
  #st_crop(xmin = 3.948, xmax = 5.049, ymin = 43.259, ymax = 43.796) %>%
  st_transform(2154) %>% 
  select(ID) 

print("GIS_limits loaded")

START = Sys.time()

# Make grid
g <- GIS_limits %>%
  st_make_grid(cellsize = size_square, what = "centers", crs = 2154) %>% # 21.5 hours
  st_intersection(GIS_limits) %>% 
  st_transform(4326)

END = Sys.time()
END-START

#beep(2)

print("grid created")

# Cut in n parts
points_per_part <- ceiling(length(g) / n_parts)  
points_geometry <- st_geometry(g)
split_indices <- split(seq_len(length(g)), 
                       rep(1:n_parts, each = points_per_part, length.out = length(g)))
split_parts <- lapply(split_indices, function(indices) {
  st_sfc(points_geometry[indices])
})

split_parts = lapply(split_parts, st_coordinates)
split_parts = lapply(split_parts, as.data.frame)

names1 <- names(split_parts)

print("grid cut in n parts")

# Save
for(i in seq_along(names1)){
  fwrite(assign(names1[i], split_parts[[i]]),
         paste0(Pathname, GridName,"_part", names1[i], ".csv"))
}











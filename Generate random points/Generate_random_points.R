library(sf)
library(tmaptools)
library(tidyverse)

# Will create 10 random points in a boundary

# Working directory
Dir = "C:/Users/croemer01/Documents/R/bat-migration-europe/bat-migration-europe/Generate random points/"

# Load boundaries
AGR <- read_GPX(paste0(Dir, "export.gpx")) #if GPX file
AGR_sf = as.data.frame(AGR) %>% 
  rename_with(~ str_remove(., "waypoints."), everything()) %>% 
  st_as_sf() %>% 
  group_by() %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Plot boundaries
ggplot(AGR_sf)+ 
  geom_sf(fill = "white", color = "black")

# Select random points
sampled_points <- st_sample(AGR_sf, 10)

# Plot samples
AGR_sf |> 
  ggplot() +
  geom_sf() + 
  geom_sf(data = sampled_points)

# Save points
st_write(sampled_points, paste0(Dir, "sample_points.shp"))



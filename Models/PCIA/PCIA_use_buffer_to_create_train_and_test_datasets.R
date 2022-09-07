library(tidyverse)
library(sf)

reps_process = 1 # how many trials should be made to sort train/test dataset (see buffer_CR.r)
min_dist = 200 # geographical distance in meters to create a custom test and train dataset

# Use after SpNuit_ModActPt_clean.R just before running the rf model

DataSaison$longitude.2=DataSaison$longitude
DataSaison$latitude.2=DataSaison$latitude

xy93 = DataSaison[,c("longitude.2", "latitude.2", "longitude", 
                     "latitude")] %>% 
  st_as_sf(coords=c("longitude.2", "latitude.2")) %>% 
  st_set_crs(4326) %>% 
  st_transform(2154) # transform to L93 to obtain spatial distances in m

xy93 = xy93 %>% 
  mutate(x = unlist(map(xy93$geometry,1)),
         y = unlist(map(xy93$geometry,2))) %>% 
  as.data.frame() %>%  # remove if need a spatial object
  select(x, y, latitude, longitude)

# xy93$y = xy93$lat  #/1000
# xy93$x = xy93$long  #/1000
#plot(xy93$x, xy93$y)

xy93.1=xy93[rownames(unique(xy93[,c("x","y")])),]
rownames(xy93.1) <- NULL

source(paste("/trinity/home/croemer/scripts_VC/","buffer_CR.R",sep=""), local = TRUE) 

xy93.2 <- buffer.f(xy93.1, min_dist, reps_process) # Choose sites that are min_dist far away from each other
xy93.3 = select(xy93.2, latitude, longitude, type)

#beepr::beep(2)

# # Compare initial dataset with sum of proposed separated datasets
# dim(xy93.1)
# dim(xy93.3)
# dim(unique(xy93.3))
# 
# # Check that the dataset is fine
# ggplot(xy93.3 %>%
#          arrange(desc(type)),
#        aes(x=longitude, y=latitude, color=type)) +
#   geom_point()

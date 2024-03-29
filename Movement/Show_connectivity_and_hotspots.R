
library(tidyverse)
library(sf)
library(raster)
library(viridis)
library(beepr)

Sp = "Nycnoc"
DateOrigin = "0301"
DateGoal = "0315"

# Connectivity
Connectivity = raster(list.files("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/Stacked/", 
                                 pattern=paste0(".*", Sp, "_", DateGoal),
                                 full.names = T))


# Hotspots Origin
Hotspots_Origin = read_sf(paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/weighted_2023-11-17/", 
                                 Sp, "_", DateGoal, "_Origin.csv"))
Hotspots_Goal = read_sf(paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/weighted_2023-11-17/", 
                               Sp, "_", DateGoal, "_Goal.csv"))

# Create polygons from table for origin
for(k in 1:length(names(table(Hotspots_Origin$L2)))){
  outer_k = matrix(c(as.numeric(Hotspots_Origin$x[which(Hotspots_Origin$L1==1 & Hotspots_Origin$L2==k)]), # exterior rings
                     as.numeric(Hotspots_Origin$y[which(Hotspots_Origin$L1==1 & Hotspots_Origin$L2==k)])), 
                   nrow = nrow(Hotspots_Origin[which(Hotspots_Origin$L1==1 & Hotspots_Origin$L2==k),]), 
                   ncol = 2) 
  if(exists("list_k")){rm(list_k)}
  for (l in 1:length(names(table(Hotspots_Origin$L1[which(Hotspots_Origin$L2==k)])))){ # different holes
    hole_l = matrix(c(as.numeric(Hotspots_Origin$x[which(Hotspots_Origin$L1==l & Hotspots_Origin$L2==k)]),
                      as.numeric(Hotspots_Origin$y[which(Hotspots_Origin$L1==l & Hotspots_Origin$L2==k)])), 
                    nrow = nrow(Hotspots_Origin[which(Hotspots_Origin$L1==l & Hotspots_Origin$L2==k),]), 
                    ncol = 2)
    if(exists("list_k")){
      list_k[[l]] = hole_l
    }else{
      list_k = list(outer_k, hole_l) # create one list (exterior ring + holes) for each L2 feature
    } 
  }
  if(exists("c_final")){
    c_final = c(c_final, st_polygon(list_k))
  }else{
    c_final = st_polygon(list_k)
  }
}

plot(c_final)

# Create polygons from table for goal
for(k in 1:length(names(table(Hotspots_Goal$L2)))){
  outer_k = matrix(c(as.numeric(Hotspots_Goal$x[which(Hotspots_Goal$L1==1 & Hotspots_Goal$L2==k)]), # exterior rings
                     as.numeric(Hotspots_Goal$y[which(Hotspots_Goal$L1==1 & Hotspots_Goal$L2==k)])), 
                   nrow = nrow(Hotspots_Goal[which(Hotspots_Goal$L1==1 & Hotspots_Goal$L2==k),]), 
                   ncol = 2) 
  if(exists("list_k")){rm(list_k)}
  for (l in 1:length(names(table(Hotspots_Goal$L1[which(Hotspots_Goal$L2==k)])))){ # different holes
    hole_l = matrix(c(as.numeric(Hotspots_Goal$x[which(Hotspots_Goal$L1==l & Hotspots_Goal$L2==k)]),
                      as.numeric(Hotspots_Goal$y[which(Hotspots_Goal$L1==l & Hotspots_Goal$L2==k)])), 
                    nrow = nrow(Hotspots_Goal[which(Hotspots_Goal$L1==l & Hotspots_Goal$L2==k),]), 
                    ncol = 2)
    if(exists("list_k")){
      list_k[[l]] = hole_l
    }else{
      list_k = list(outer_k, hole_l) # create one list (exterior ring + holes) for each L2 feature
    } 
  }
  if(exists("c_final_goal")){
    c_final_goal = c(c_final_goal, st_polygon(list_k))
  }else{
    c_final_goal = st_polygon(list_k)
  }
}

plot(c_final_goal)

temp<-as.data.frame(Connectivity, xy = T)
names(temp)[which(grepl(paste0(".*", Sp, "_", DateGoal), names(temp)))] = "layer"

# add CRS information
c_final2 = st_sfc(c_final)
c_final3 = st_as_sf(c_final2, crs = 4326)
c_final_goal2 = st_sfc(c_final_goal)
c_final_goal3 = st_as_sf(c_final_goal2, crs = 4326)

png(filename=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                    Sp, "_", DateOrigin, "-", DateGoal, ".png"),width=1600,height=1000,res=300)
ggplot() +
  geom_raster(data=temp, aes(x = x, y = y, fill = (layer), col=NULL)) +
  geom_sf(data=c_final3, fill="#FDE725FF", colour = NA) +
  geom_sf(data=c_final_goal3, fill="#FDE725FF", colour = NA) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_fill_viridis_c(option = "D",
                       trans = scales::pseudo_log_trans(base = 10),
                       na.value=NA,
                       name = "Index of connectivity") +
  coord_sf()

dev.off()

FRANCE <- map_data("world", region = "France")
FRANCE_sf = st_as_sf(
  FRANCE, coords = c("long", "lat"), crs=4326, remove=FALSE)  %>%
  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

png(filename=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                    Sp, "_", DateOrigin, ".png"),width=1600,height=1000,res=300)
ggplot() +
  geom_sf(data = FRANCE_sf, fill = "black") +
  geom_sf(data=c_final3, fill="#FDE725FF", colour = NA) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_sf()

dev.off()

png(filename=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                    Sp, "_", DateGoal, ".png"),width=1600,height=1000,res=300)
ggplot() +
  geom_sf(data = FRANCE_sf, fill = "black") +
  geom_sf(data=c_final_goal3, fill="#FDE725FF", colour = NA) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  coord_sf()

dev.off()


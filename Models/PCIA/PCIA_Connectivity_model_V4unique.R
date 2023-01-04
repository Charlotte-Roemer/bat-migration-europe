

library(data.table)
library(tidyverse)
library(raster)
library(sf)
library(gdistance)

Name = "weighted_2022-08-11"
Season = "Spring"
Sp = "Nyclei"
THETA = 0.1

START=Sys.time()

# Read Origin
Origin = fread(paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/", 
                    Sp, "_", Season, "_", "Origin", ".csv"))

# Read Goal
Goal = fread(paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/", 
                   Sp, "_", Season, "_", "Goal", ".csv"))

# Read Transition
land_cond_sub = readRDS(paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/", 
                    Sp, "_", "Transition", ".rds"))

ID_Origin <- sample(Origin$id, size=1) #draw random points (random pairs)
ID_Goal <- sample(Goal$id, size=1)
pt_Origin <- SpatialPoints(cbind(Origin$x[Origin$id == ID_Origin], 
                                 Origin$y[Origin$id == ID_Origin])) #get xy for each point
pt_Goal <- SpatialPoints(cbind(Goal$x[Goal$id == ID_Goal], 
                               Goal$y[Goal$id == ID_Goal])) #get xy for each point

# Calculate paths
pasT <- passage(land_cond_sub, pt_Origin, pt_Goal, theta=THETA)


# Save result
UniqueName = paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"), "_", round(runif(1, 1, 100000000000)))
raster::writeRaster(pasT, paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/", 
                                 Name, "_", Sp, "_", Season, "_", THETA, "_", UniqueName, ".tif"), overwrite=TRUE)


# png(filename=paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/", 
#                     Name, "_", Sp, "_", Season, "_", THETA, ".png"), 
#     height=2000, width=2000,res=300)
# plot1 = plot(oper)
# print(plot1)
# dev.off()



END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF

# beep(2)


# 
# plot(Raster_TRANSITION_wtNA)
# plot(pt_Origin, add=T)
# plot(pt_Goal, add=T)
# plot(pasT)




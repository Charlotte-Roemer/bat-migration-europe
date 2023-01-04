
# Stack multiple rasters and writer raster

library(raster)
library(tidyverse)
library(beepr)

Pattern = "Nyclei_Spring"

START = Sys.time()

files <- list.files(path="C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/Results_V5unique", pattern=Pattern, all.files=FALSE, full.names=TRUE,recursive=TRUE)

s <- stack(files) # stack rasters
i <- (maxValue(s))>0 # select only rasters that succeeded (contain values > 0)
s_no_0 = s[[which(i)]]

rs1 <- calc(s_no_0, sum) # sum all rasters

# Save
writeRaster(rs1, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                   Pattern, "_TOTAL_n", dim(s_no_0)[3],  ".tif"), overwrite = T)

plot(rs1)

END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF

beep(2)




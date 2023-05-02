
# Stack multiple rasters and writer raster

library(raster)
library(tidyverse)
library(beepr)

Pattern = "Pipnat_0501_0.1"

START = Sys.time()

files <- list.files(path="/mnt/beegfs/croemer/VigieChiro/Connectivity_maps", pattern=Pattern, all.files=FALSE, full.names=TRUE,recursive=F)

s <- stack(files) # stack rasters
i <- (maxValue(s))>0 # select only rasters that succeeded (contain values > 0)
s_no_0 = s[[which(i)]]

rs1 <- calc(s_no_0, sum) # sum all rasters

# Save
writeRaster(rs1, paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/Stacked/", 
                   Pattern, "_TOTAL_n", dim(s_no_0)[3],  ".tif"), overwrite = T)

plot(rs1)

END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF

beep(2)





# Stack multiple rasters and writer raster

library(raster)
library(tidyverse)
library(beepr)

Threshold = "0_VC0V_Yves_2023-03"

START = Sys.time()


List_Species = c("Barbar", "Eptnil", "Eptser"
                 # , "Hypsav", "Minsch", "Myoalc", "Myobec", 
                 # "Myocap", "Myodau", "Myodas", "Myoema", "Myomys", "Myonat", "Nyclas",
                 # "Nyclei", "Nycnoc", "Pipkuh", "Pippip", "Pipnat", "Pippyg", "Pleaur", 
                 # "Pleaus", "Plemac", "Rhieur", "Rhifer", "Rhihip", "Tadten", "Vesmur"
)
ListTimes = c("0315", "0401", "0415", "0501", "0515", "0601", 
              #"0615", "0701", "0715",
              "0801", "0815", "0901", "0915", "1001")

for (j in 1:length(List_Species)){
  for (k in 1:length(ListTimes)){
    
    files <- list.files(path=paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps", "/", Threshold),
                        pattern=paste0(Threshold, "_", List_Species[j], "_", ListTimes[k]), 
                        all.files=FALSE, full.names=TRUE,recursive=F)
    
    s <- stack(files) # stack rasters
    i <- (maxValue(s))>0 # select only rasters that succeeded (contain values > 0)
    s_no_0 = s[[which(i)]]
    
    rs1 <- calc(s_no_0, sum) # sum all rasters
    
    # Save
    writeRaster(rs1, paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/Stacked/", 
                            paste0(Threshold, "_", List_Species[j], "_", ListTimes[k]), "_TOTAL_n", 
                            dim(s_no_0)[3],  ".tif"), overwrite = T)
    
    #plot(rs1)
  }
}

END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF

beep(2)
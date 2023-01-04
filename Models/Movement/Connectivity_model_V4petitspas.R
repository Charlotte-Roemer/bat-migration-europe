
# Create patches for movement model

library(data.table)
library(tidyverse)
library(raster)
library(sf)
library(gdistance)
library(landscapemetrics)
library(beepr)
# library(foreach)
# library(doParallel)
library(terra)

# Load acoustic predictions
Name = "weighted_2022-08-15"
Directory <- paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/PredictionsModels/", Name) # repertory with outputs from Predict_act
Sp = "Pipnat"
#THETA = 0.1
#nb <- 10 # number of pairs of origin-goal points

# Functions needed
comb_func <-  function(pas, pasT){   
  if (class(pasT)!="NULL"){  #"try-error" this is because the passage function produces weird random errors (see similar events https://github.com/sneumann/xcms/issues/288)
    if (mean(values(pasT)<Inf, na.rm=T)>0.1){
      if (is.null(pas)) pas <- pasT
      if (!is.null(pas)) pas <- pas + pasT
    }}
}

Rasterize_function <- function(TableObs)
{
  dataa_Month_s = TableObs[,1:3]
  colnames(dataa_Month_s) <- c('x', 'y', 'vals')
  e <- extent(dataa_Month_s[,1:2])
  r <- raster(e, ncol=558, nrow=530, crs=4326)
  x <- rasterize(dataa_Month_s[, 1:2], r, dataa_Month_s[,3], fun=mean) 
  
  return(x)
}

Q80_function <- function(x_raster)
{
  # Patches are areas with value > Q80
  Data_Q80 = x_raster
  Data_Q80[Data_Q80<quantile(Data_Q80, 0.8)] = 0
  Data_Q80[Data_Q80>quantile(Data_Q80, 0.8)] = 1
  
  return(Data_Q80)
}

multiResultClass <- function(pas=NULL) # for parallel run
{
  me <- list(
    pas = pas
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"multiResultClass")
  return(me)
}

stopCluster_no_error_check <- function(cl = NULL) # to avoid using the closeCluster of doparallel leading to a fail due to errors
{
  cl <- defaultCluster(cl)
  if(identical(cl, get("default", envir = .reg)))
    assign("default", NULL, envir = .reg)
  UseMethod("stopCluster")
}

Clump_function <- function(Raster_sub)
{
  Raster_patches <- clump(Raster_sub, directions=8, gaps=F)
  y <- patches(rast(Raster_patches))
  rz <- zonal(cellSize(y, unit="ha"), y, sum, as.raster=TRUE)
  s <- ifel(rz < 100, NA, y)
  s2 = st_as_sf(as.polygons(s))
  patches_perimeter = as.data.frame(st_coordinates(s2))
  colnames(patches_perimeter)[colnames(patches_perimeter) == "X"] <- "x"
  colnames(patches_perimeter)[colnames(patches_perimeter) == "Y"] <- "y"
  patches_perimeter$id = rownames(patches_perimeter)
  
  return(patches_perimeter)
}

# Load files
list_file <- list.files(Directory,recursive=FALSE,pattern="*.csv$")
ls2 = paste(paste0(Directory,"/",list_file, sep=""))
ld <- lapply(ls2, function(x) read_csv(x))
ld <- mapply(cbind, ld, "Species"=tstrsplit(list_file,split="_")[[1]], SIMPLIFY=F) # add column with species name
ld <- mapply(cbind, ld, "Month"=tstrsplit(tstrsplit(list_file,split="_")[[3]], split="-")[[2]], SIMPLIFY=F) # add column with month name
ld <- mapply(cbind, ld, "Day"=tstrsplit(tstrsplit(list_file,split="_")[[3]], split="-")[[3]], SIMPLIFY=F) # add column with day name

file_bind <- do.call("rbind",ld)

# Back-transform predictions
file_bind$pred=10^(file_bind$pred)

# Define start, mid and end data
dataa = subset(file_bind, file_bind$Species == Sp)
#dataa_START=subset(dataa, dataa$Month=="04" & dataa$Day=="01") # START (1rst April? --> need method to determine)
#dataa_MID=subset(dataa, dataa$Month=="07" & dataa$Day=="01") # MID WAY
#dataa_END=subset(dataa, dataa$Month=="10" & dataa$Day=="15") # END
dataa_ALLSPRING = subset(dataa, as.numeric(dataa$Month)<7) 
dataa_ALLAUTUMN = subset(dataa, as.numeric(dataa$Month)>7) 
dataa_0315=subset(dataa, dataa$Month=="03" & dataa$Day=="15")
dataa_0401=subset(dataa, dataa$Month=="04" & dataa$Day=="01")
dataa_0415=subset(dataa, dataa$Month=="04" & dataa$Day=="15")
dataa_0501=subset(dataa, dataa$Month=="05" & dataa$Day=="01")
dataa_0515=subset(dataa, dataa$Month=="05" & dataa$Day=="15")
dataa_0601=subset(dataa, dataa$Month=="06" & dataa$Day=="01")
dataa_0615=subset(dataa, dataa$Month=="06" & dataa$Day=="15")
dataa_0701=subset(dataa, dataa$Month=="07" & dataa$Day=="01")

# Create transition layer by selecting the highest value of each pixel across the year
# Sring
data_highest_value_SPRING = dataa_ALLSPRING %>%                                     
  arrange(desc(pred)) %>% 
  group_by(Group.1, Group.2) %>%
  slice(1)
# Autumn
data_highest_value_AUTUMN = dataa_ALLAUTUMN %>%                                     
  arrange(desc(pred)) %>% 
  group_by(Group.1, Group.2) %>%
  slice(1)

# Raster of observations
#Raster_START = Q80_function(Rasterize_function(dataa_START))
#Raster_MID = Q80_function(Rasterize_function(dataa_MID))
#Raster_END= Q80_function(Rasterize_function(dataa_END))
Raster_TRANSITION_SPRING = Rasterize_function(data_highest_value_SPRING)
Raster_TRANSITION_AUTUMN = Rasterize_function(data_highest_value_AUTUMN)
Raster_0315= Q80_function(Rasterize_function(dataa_0315))
Raster_0401= Q80_function(Rasterize_function(dataa_0401))
Raster_0415= Q80_function(Rasterize_function(dataa_0415))
Raster_0501= Q80_function(Rasterize_function(dataa_0501))
Raster_0515= Q80_function(Rasterize_function(dataa_0515))
Raster_0601= Q80_function(Rasterize_function(dataa_0601))
Raster_0615= Q80_function(Rasterize_function(dataa_0615))
Raster_0701= Q80_function(Rasterize_function(dataa_0701))


# Save result
#writeRaster(Raster_MID, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/Pipnat_MID.tif"), overwrite=TRUE)

plot(Raster_START)
plot(Raster_MID)
plot(Raster_END)
plot(Raster_0315)
plot(Raster_0401)
plot(Raster_0415)
plot(Raster_0501)
plot(Raster_0515)
plot(Raster_0601)
plot(Raster_0615)
plot(Raster_0701)

# Crop data in box (optional)
Raster_extent= extent(Raster_0315)
# Raster_START_sub <- crop(Raster_START, Raster_extent)
# Raster_MID_sub=crop(Raster_MID, Raster_extent)
# Raster_END_sub=crop(Raster_END, Raster_extent)
Raster_0315_sub <- crop(Raster_0315, Raster_extent)
Raster_0401_sub <- crop(Raster_0401, Raster_extent)
Raster_0415_sub <- crop(Raster_0415, Raster_extent)
Raster_0501_sub <- crop(Raster_0501, Raster_extent)
Raster_0515_sub <- crop(Raster_0515, Raster_extent)
Raster_0601_sub <- crop(Raster_0601, Raster_extent)
Raster_0615_sub <- crop(Raster_0615, Raster_extent)
Raster_0701_sub <- crop(Raster_0701, Raster_extent)


# Clump pixels and remove small patches
R0315_patches_perimeter = Clump_function(Raster_0315_sub)
R0401_patches_perimeter = Clump_function(Raster_0401_sub)
R0415_patches_perimeter = Clump_function(Raster_0415_sub)
R0501_patches_perimeter = Clump_function(Raster_0501_sub)
R0515_patches_perimeter = Clump_function(Raster_0515_sub)
R0601_patches_perimeter = Clump_function(Raster_0601_sub)
R0615_patches_perimeter = Clump_function(Raster_0615_sub)
R0701_patches_perimeter = Clump_function(Raster_0701_sub)

# START_patches_perimeter = Clump_function(Raster_START_sub)
# MID_patches_perimeter = Clump_function(Raster_MID_sub)
# END_patches_perimeter = Clump_function(Raster_END_sub)

ListPatches = list(R0315_patches_perimeter, R0401_patches_perimeter, R0415_patches_perimeter, 
                   R0501_patches_perimeter, R0515_patches_perimeter, R0601_patches_perimeter, 
                   R0615_patches_perimeter, R0701_patches_perimeter)

ListTimes = c("0315", "0401", "0415", "0501", "0515", "0601", "0615", "0701")

# Save start, mid and end patches
for (k in 1:(length(ListTimes)-1)) {
  
  print(ListTimes[k])

  Origin = as.data.frame(ListPatches[k])
  Goal = as.data.frame(ListPatches[k+1])
  
  # Save result
  fwrite(Origin, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                        Sp, "_", ListTimes[k], "_", "Origin", ".csv"))
  fwrite(Goal, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                      Sp, "_", ListTimes[k], "_", "Goal", ".csv"))
}


# Spring
Raster_TRANSITION_wtNA_SPRING = Raster_TRANSITION_SPRING
Raster_TRANSITION_wtNA_SPRING[is.na(Raster_TRANSITION_wtNA_SPRING)] <- 0 # replace NA by 0 because passage function does not like NA
land_cost_sub_SPRING <- crop(Raster_TRANSITION_wtNA_SPRING, Raster_extent)
land_cost_sub_SPRING <- transition(land_cost_sub_SPRING, transitionFunction = mean, 8)
#land_cond_sub <- geoCorrection(land_cond_sub, type = "r") # IMPORTANT POINT : DO WE NEED IT ?
saveRDS(land_cost_sub_SPRING, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                                     Sp, "_", "Spring_", "Transition", ".rds"))

# Autumn
Raster_TRANSITION_wtNA_AUTUMN = Raster_TRANSITION_AUTUMN
Raster_TRANSITION_wtNA_AUTUMN[is.na(Raster_TRANSITION_wtNA_AUTUMN)] <- 0 # replace NA by 0 because passage function does not like NA
land_cost_sub_AUTUMN <- crop(Raster_TRANSITION_wtNA_AUTUMN, Raster_extent)
land_cost_sub_AUTUMN <- transition(land_cost_sub_AUTUMN, transitionFunction = mean, 8)
#land_cond_sub <- geoCorrection(land_cond_sub, type = "r") # IMPORTANT POINT : DO WE NEED IT ?
saveRDS(land_cost_sub_AUTUMN, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                                     Sp, "_", "Autumn_", "Transition", ".rds"))

END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF

beep(2)


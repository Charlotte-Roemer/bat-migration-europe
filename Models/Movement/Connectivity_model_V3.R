
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
dataa_START=subset(dataa, dataa$Month=="04" & dataa$Day=="01") # START (1rst April? --> need method to determine)
dataa_SPRING=subset(dataa, dataa$Month=="05" & dataa$Day=="15") # for transition matrix
dataa_ALLSPRING = subset(dataa, as.numeric(dataa$Month)<7) 
dataa_MID=subset(dataa, dataa$Month=="07" & dataa$Day=="01") # MID WAY
dataa_AUTUMN=subset(dataa, dataa$Month=="09" & dataa$Day=="01") # for transition matrix
dataa_END=subset(dataa, dataa$Month=="10" & dataa$Day=="15") # END
dataa_ALLAUTUMN = subset(dataa, as.numeric(dataa$Month)>7) 

# # Look at suitability areas
# quantile(dataa_START$pred, c(0.1, 0.55, 0.65, 0.8))
# dataa_START$Class = ifelse(dataa_START$pred<quantile(dataa_START$pred, 0.1), "Not suitable", "Less suitable")
# dataa_START$Class = ifelse(dataa_START$pred>=quantile(dataa_START$pred, 0.55), "Moderately suitable", dataa_START$Class)
# dataa_START$Class = ifelse(dataa_START$pred>=quantile(dataa_START$pred, 0.65), "Suitable", dataa_START$Class)
# dataa_START$Class = ifelse(dataa_START$pred>=quantile(dataa_START$pred, 0.8), "Highly suitable", dataa_START$Class)
# 
# dataa_START$Class = factor(dataa_START$Class, levels=c("Not suitable", "Less suitable", "Moderately suitable", "Suitable", "Highly suitable"))
# 
# ggplot() +
#   geom_point(data = dataa_START, 
#              mapping = aes(x=Group.1, y=Group.2, col=Class)) + 
#   scale_color_viridis(discrete=T)

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
Raster_START = Q80_function(Rasterize_function(dataa_START))
Raster_SPRING = Rasterize_function(dataa_SPRING)
Raster_MID = Q80_function(Rasterize_function(dataa_MID))
Raster_AUTUMN = Rasterize_function(dataa_AUTUMN)
Raster_END= Q80_function(Rasterize_function(dataa_END))
Raster_TRANSITION_SPRING = Rasterize_function(data_highest_value_SPRING)
Raster_TRANSITION_AUTUMN = Rasterize_function(data_highest_value_AUTUMN)

# Save result
#writeRaster(Raster_MID, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/Pipnat_MID.tif"), overwrite=TRUE)

plot(Raster_START)
plot(Raster_MID)
plot(Raster_END)

# Crop data in box (optional)
#Raster_extent_START = extent(-1, 6, 42, 44) 
#Raster_extent_MID = extent(-1, 6, 48, 52) 
Raster_extent= extent(Raster_START)
#Raster_extent = extent(Raster_START)
Raster_START_sub <- crop(Raster_START, Raster_extent)
Raster_MID_sub=crop(Raster_MID, Raster_extent)
Raster_END_sub=crop(Raster_END, Raster_extent)

# Clump pixels and remove small patches
Raster_START_patches <- clump(Raster_START_sub, directions=8, gaps=F)
y <- patches(rast(Raster_START_patches))
rz <- zonal(cellSize(y, unit="ha"), y, sum, as.raster=TRUE)
s <- ifel(rz < 100, NA, y)
#Raster_START_patches_centroids <- get_centroids(s) 
s2 = st_as_sf(as.polygons(s))
START_patches_perimeter = as.data.frame(st_coordinates(s2))
colnames(START_patches_perimeter)[colnames(START_patches_perimeter) == "X"] <- "x"
colnames(START_patches_perimeter)[colnames(START_patches_perimeter) == "Y"] <- "y"
START_patches_perimeter$id = rownames(START_patches_perimeter)

Raster_MID_patches <- clump(Raster_MID_sub, directions=8, gaps=F)
y <- patches(rast(Raster_MID_patches))
rz <- zonal(cellSize(y, unit="ha"), y, sum, as.raster=TRUE)
s <- ifel(rz < 100, NA, y)
#Raster_MID_patches_centroids <- get_centroids(s1) 
s2 = st_as_sf(as.polygons(s))
MID_patches_perimeter = as.data.frame(st_coordinates(s2))
colnames(MID_patches_perimeter)[colnames(MID_patches_perimeter) == "X"] <- "x"
colnames(MID_patches_perimeter)[colnames(MID_patches_perimeter) == "Y"] <- "y"
MID_patches_perimeter$id = rownames(MID_patches_perimeter)

Raster_END_patches <- clump(Raster_END_sub, directions=8, gaps=F)
y <- patches(rast(Raster_END_patches))
rz <- zonal(cellSize(y, unit="ha"), y, sum, as.raster=TRUE)
s <- ifel(rz < 100, NA, y)
#Raster_MID_patches_centroids <- get_centroids(s2) 
s2 = st_as_sf(as.polygons(s))
END_patches_perimeter = as.data.frame(st_coordinates(s2))
colnames(END_patches_perimeter)[colnames(END_patches_perimeter) == "X"] <- "x"
colnames(END_patches_perimeter)[colnames(END_patches_perimeter) == "Y"] <- "y"
END_patches_perimeter$id = rownames(END_patches_perimeter)

# Save start, mid and end patches
START=Sys.time()
for (k in 1:2) {
  
  if (k==1){
    Season = "Spring"
    Raster_1 = Raster_START_sub
    Raster_2 = Raster_MID_sub
    #Origin = Raster_START_patches_centroids
    Origin = START_patches_perimeter
    #Goal = Raster_MID_patches_centroids
    Goal = MID_patches_perimeter
    
  }else{
    Season = "Autumn"
    Raster_1 = Raster_MID_sub
    Raster_2 = Raster_END_sub
    #Origin = Raster_MID_patches_centroids
    Origin = MID_patches_perimeter
    #Goal = Raster_END_patches_centroids
    Goal = END_patches_perimeter
  }
  
  print(Season)
  
  # Save result
  fwrite(Origin, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                                     Sp, "_", Season, "_", "Origin", ".csv"))
  fwrite(Goal, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                                   Sp, "_", Season, "_", "Goal", ".csv"))
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


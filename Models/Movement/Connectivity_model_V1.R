
# Create patches for movement model

library(data.table)
library(tidyverse)
library(raster)
library(sf)
library(gdistance)
library(landscapemetrics)
library(beepr)
library(foreach)
library(doParallel)

# Load acoustic predictions
Name = "weighted_2022-08-11"
Directory <- paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/PredictionsModels/", Name) # repertory with outputs from Predict_act
Sp = "Nyclei"
THETA = 0.1
nb <- 2 # number of pairs of origin-goal points (1000 is a good beginning but it is long)

# Functions needed
comb_func <-  function(pas, pasT){   
  if (class(pasT)!="NULL"){  #"try-error"
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
  Data_Q80[Data_Q80<quantile(Data_Q80, 0.80)] = 0
  Data_Q80[Data_Q80>quantile(Data_Q80, 0.80)] = 1
  
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
dataa_MID=subset(dataa, dataa$Month=="07" & dataa$Day=="01") # MID WAY
dataa_AUTUMN=subset(dataa, dataa$Month=="09" & dataa$Day=="01") # for transition matrix
dataa_END=subset(dataa, dataa$Month=="10" & dataa$Day=="15") # END

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

# Raster of observations
Raster_START = Q80_function(Rasterize_function(dataa_START))
Raster_SPRING = Rasterize_function(dataa_SPRING)
Raster_MID = Q80_function(Rasterize_function(dataa_MID))
Raster_AUTUMN = Rasterize_function(dataa_AUTUMN)
Raster_END= Q80_function(Rasterize_function(dataa_END))

# Save result
#writeRaster(Raster_SPRING, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/Carte.tif"), overwrite=TRUE)

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
Raster_START_patches <- reclassify(Raster_START_patches, cbind(-Inf, 100, NA), right=FALSE) # Remove patches containing less than x pixels
Raster_START_patches_centroids <- get_centroids(Raster_START_patches) 

Raster_MID_patches <- clump(Raster_MID_sub, directions=8, gaps=F)
Raster_MID_patches <- reclassify(Raster_MID_patches, cbind(-Inf, 100, NA), right=FALSE) # Remove patches containing less than x pixels
Raster_MID_patches_centroids <- get_centroids(Raster_MID_patches) 

Raster_END_patches <- clump(Raster_END_sub, directions=8, gaps=F)
Raster_END_patches <- reclassify(Raster_END_patches, cbind(-Inf, 100, NA), right=FALSE) # Remove patches containing less than x pixels
Raster_END_patches_centroids <- get_centroids(Raster_END_patches) 

#random walk (akin to a current map in Circuitscape)
START=Sys.time()
for (k in 1:2) {
  
  if (k==1){
    Season = "Spring"
    Cost = 1/Raster_SPRING
    Raster_1 = Raster_START_sub
    Raster_2 = Raster_MID_sub
    Origin = Raster_START_patches_centroids
    Goal = Raster_MID_patches_centroids
  }else{
    Season = "Autumn"
    Cost = 1/Raster_AUTUMN
    Raster_1 = Raster_MID_sub
    Raster_2 = Raster_END_sub
    Origin = Raster_MID_patches_centroids
    Goal = Raster_END_patches_centroids
  }
  
  print(Season)
  
  land_cost_sub <- crop(Cost, Raster_extent)
  #land_cost_sub <- reclassify(land_cost_sub, cbind(NA, 0))
  land_cond_sub <- transition(land_cost_sub, transitionFunction = mean, 8)
  #land_cond_sub <- geoCorrection(land_cond_sub, type = "r") # IMPORTANT POINT : DO WE NEED IT ?
  
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)

  pas <- NULL
  oper <- foreach (i =1:nb, .packages=c("raster","sp","gdistance","maptools", "rgeos","sf","stringr"), 
                   .combine=comb_func) %dopar% {
    ID_Origin <- sample(Origin$id, size=1) #draw random points (random pairs)
    ID_Goal <- sample(Goal$id, size=1)
    pt_Origin <- SpatialPoints(cbind(Origin$x[Origin$id == ID_Origin], 
                                    Origin$y[Origin$id == ID_Origin])) #get xy for each point
    pt_Goal <- SpatialPoints(cbind(Goal$x[Goal$id == ID_Goal], 
                                  Goal$y[Goal$id == ID_Goal])) #get xy for each point
    
    # Calculate paths
    pasT <- try(passage(land_cond_sub, pt_Origin, pt_Goal, theta=THETA), silent=T)
    
  }
  
  stopCluster(cl)
  
  png(filename=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                      Name, "_", Sp, "_", Season, "_", THETA, ".png"), 
      height=2000, width=2000,res=300)
  plot1 = plot(oper)
  print(plot1)
  dev.off()
  
}

END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF

beep(2)


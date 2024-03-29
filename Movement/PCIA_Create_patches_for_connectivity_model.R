
# Create patches for movement model

library(data.table)
library(tidyverse)
library(raster)
library(sf)
library(gdistance)
library(landscapemetrics)
library(beepr)
library(terra)

# Load acoustic predictions
Name =  "weighted_2024-03-19" #"weighted_2022-08-15"
Directory <- paste0("/mnt/beegfs/croemer/VigieChiro/PredictionsModels/", Name) # repertory with outputs from Predict_act
Species = "All"  # "Myocap"
AggregatedPixel = F # if only a map of the highest value per pixel across all year is needed
#THETA = 0.1
#nb <- 10 # number of pairs of origin-goal points

if(AggregatedPixel){
  dir.create(paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/MaxValuePixel/", Name, "/"))
}
dir.create(paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", Name))

START=Sys.time()

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
  r <- raster::raster(e, ncol=558, nrow=530, crs=4326)
  x <- rasterize(dataa_Month_s[, 1:2], r, dataa_Month_s[,3], fun=mean) 
  
  return(x)
}

# Q80_function <- function(x_raster)
# {
#   # Patches are areas with value > Q80
#   Data_Q80 = x_raster
#   Data_Q80[Data_Q80<quantile(Data_Q80, 0.8)] = 0
#   Data_Q80[Data_Q80>quantile(Data_Q80, 0.8)] = 1
#   
#   return(Data_Q80)
# }

# Function to determine threshold value of activity to create patches based on kurtosis:
# the higher the kurtosis, the higher the threshold
Kurtosis_function <- function(x_raster)
{
  KU = e1071::kurtosis(x_raster$pred)
  
  min_kurtosis = 10   # this value was estimated by checking the data for different species
  max_kurtosis = 160  # this value was estimated by checking the data for different species
  min_y = 0.92
  max_y = 0.99
  
  # Determine the slope of the relationship between kurtosis and y
  slope = (max_y - min_y) / (max_kurtosis - min_kurtosis)
  # Calculate y based on the provided kurtosis value
  y = min_y + slope * (KU - min_kurtosis)
  # Ensure y remains within the valid range
  y = max(min_y, min(max_y, y)) 
  
  # Patches are areas with value > y
  Data_K = Rasterize_function(x_raster)
  Data_K[Data_K<quantile(Data_K, y)] = 0
  Data_K[Data_K>quantile(Data_K, y)] = 1
  
  return(Data_K)
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
if(Species == "All"){
  list_file <- list.files(Directory,recursive=FALSE,pattern="*.csv$")
}else{
  list_file <- list.files(Directory,recursive=FALSE,pattern=paste0("^", Species, ".*.csv$"))
}

ls2 = paste(paste0(Directory,"/",list_file, sep=""))
ld <- lapply(ls2, function(x) read_csv(x))
ld <- mapply(cbind, ld, "Species"=tstrsplit(list_file,split="_")[[1]], SIMPLIFY=F) # add column with species name
ld <- mapply(cbind, ld, "Month"=tstrsplit(tstrsplit(list_file,split="_")[[3]], split="-")[[2]], SIMPLIFY=F) # add column with month name
ld <- mapply(cbind, ld, "Day"=tstrsplit(tstrsplit(list_file,split="_")[[3]], split="-")[[3]], SIMPLIFY=F) # add column with day name

file_bind <- do.call("rbind",ld)

list_species = names(table(sapply(ld, function(x) names(table(x$Species)))))

# Back-transform predictions
file_bind$pred=10^(file_bind$pred)-1

if(Species != "All"){
  list_species = Species
}

for(i in 1:length(list_species)){
  Sp = list_species[i]
  
  print(Sp)
  
  if(!AggregatedPixel){
    
    # Define origin and goal data
    dataa = subset(file_bind, file_bind$Species == Sp)
    dataa_ALLSPRING = subset(dataa, as.numeric(dataa$Month)<6 & as.numeric(dataa$Month)>2) 
    dataa_ALLAUTUMN = subset(dataa, as.numeric(dataa$Month)<11 & as.numeric(dataa$Month)>7) 
    dataa_0315=subset(dataa, dataa$Month=="03" & dataa$Day=="15")
    dataa_0401=subset(dataa, dataa$Month=="04" & dataa$Day=="01")
    dataa_0415=subset(dataa, dataa$Month=="04" & dataa$Day=="15")
    dataa_0501=subset(dataa, dataa$Month=="05" & dataa$Day=="01")
    dataa_0515=subset(dataa, dataa$Month=="05" & dataa$Day=="15")
    dataa_0601=subset(dataa, dataa$Month=="06" & dataa$Day=="01")
    dataa_0615=subset(dataa, dataa$Month=="06" & dataa$Day=="15")
    dataa_0701=subset(dataa, dataa$Month=="07" & dataa$Day=="01")
    dataa_0715=subset(dataa, dataa$Month=="07" & dataa$Day=="15")
    dataa_0801=subset(dataa, dataa$Month=="08" & dataa$Day=="01")
    dataa_0815=subset(dataa, dataa$Month=="08" & dataa$Day=="15")
    dataa_0901=subset(dataa, dataa$Month=="09" & dataa$Day=="01")
    dataa_0915=subset(dataa, dataa$Month=="09" & dataa$Day=="15")
    dataa_1001=subset(dataa, dataa$Month=="10" & dataa$Day=="01")
    dataa_1015=subset(dataa, dataa$Month=="10" & dataa$Day=="15")
    
    print("Select highest value")
    
    # Create transition layer by selecting the highest value of each pixel across the year
    # Sring
    print("Spring")
    data_highest_value_SPRING = dataa_ALLSPRING %>%                                     
      arrange(desc(pred)) %>% 
      group_by(X, Y) %>%
      slice(1)
    # Autumn
    print("Autumn")
    data_highest_value_AUTUMN = dataa_ALLAUTUMN %>%                                     
      arrange(desc(pred)) %>% 
      group_by(X, Y) %>%
      slice(1)
    
    # Raster of observations
    print("Raster of observations")
    Raster_TRANSITION_SPRING = Rasterize_function(data_highest_value_SPRING)
    Raster_TRANSITION_AUTUMN = Rasterize_function(data_highest_value_AUTUMN)
    Raster_0315= Kurtosis_function(dataa_0315)
    Raster_0401= Kurtosis_function(dataa_0401)
    Raster_0415= Kurtosis_function(dataa_0415)
    Raster_0501= Kurtosis_function(dataa_0501)
    Raster_0515= Kurtosis_function(dataa_0515)
    Raster_0601= Kurtosis_function(dataa_0601)
    Raster_0615= Kurtosis_function(dataa_0615)
    Raster_0701= Kurtosis_function(dataa_0701)
    Raster_0715= Kurtosis_function(dataa_0715)
    Raster_0801= Kurtosis_function(dataa_0801)
    Raster_0815= Kurtosis_function(dataa_0815)
    Raster_0901= Kurtosis_function(dataa_0901)
    Raster_0915= Kurtosis_function(dataa_0915)
    Raster_1001= Kurtosis_function(dataa_1001)
    Raster_1015= Kurtosis_function(dataa_1015)
    
    # Save result
    #writeRaster(Raster_MID, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/Pipnat_MID.tif"), overwrite=TRUE)
    
    # plot(Raster_0315)
    
    # Crop data in box (optional)
    print("Crop")
    Raster_extent= extent(Raster_0315)
    Raster_0315_sub <- crop(Raster_0315, Raster_extent)
    Raster_0401_sub <- crop(Raster_0401, Raster_extent)
    Raster_0415_sub <- crop(Raster_0415, Raster_extent)
    Raster_0501_sub <- crop(Raster_0501, Raster_extent)
    Raster_0515_sub <- crop(Raster_0515, Raster_extent)
    Raster_0601_sub <- crop(Raster_0601, Raster_extent)
    Raster_0615_sub <- crop(Raster_0615, Raster_extent)
    Raster_0701_sub <- crop(Raster_0701, Raster_extent)
    Raster_0715_sub <- crop(Raster_0715, Raster_extent)
    Raster_0801_sub <- crop(Raster_0801, Raster_extent)
    Raster_0815_sub <- crop(Raster_0815, Raster_extent)
    Raster_0901_sub <- crop(Raster_0901, Raster_extent)
    Raster_0915_sub <- crop(Raster_0915, Raster_extent)
    Raster_1001_sub <- crop(Raster_1001, Raster_extent)
    Raster_1015_sub <- crop(Raster_1015, Raster_extent)
    
    
    # Clump pixels and remove small patches
    print("Clump")
    R0315_patches_perimeter = Clump_function(Raster_0315_sub)
    R0401_patches_perimeter = Clump_function(Raster_0401_sub)
    R0415_patches_perimeter = Clump_function(Raster_0415_sub)
    R0501_patches_perimeter = Clump_function(Raster_0501_sub)
    R0515_patches_perimeter = Clump_function(Raster_0515_sub)
    R0601_patches_perimeter = Clump_function(Raster_0601_sub)
    R0615_patches_perimeter = Clump_function(Raster_0615_sub)
    R0701_patches_perimeter = Clump_function(Raster_0701_sub)
    R0715_patches_perimeter = Clump_function(Raster_0715_sub)
    R0801_patches_perimeter = Clump_function(Raster_0801_sub)
    R0815_patches_perimeter = Clump_function(Raster_0815_sub)
    R0901_patches_perimeter = Clump_function(Raster_0901_sub)
    R0915_patches_perimeter = Clump_function(Raster_0915_sub)
    R1001_patches_perimeter = Clump_function(Raster_1001_sub)
    R1015_patches_perimeter = Clump_function(Raster_1015_sub)
    
    ListPatches = list(R0315_patches_perimeter, R0401_patches_perimeter, R0415_patches_perimeter, 
                       R0501_patches_perimeter, R0515_patches_perimeter, R0601_patches_perimeter, 
                       R0615_patches_perimeter, R0701_patches_perimeter, R0715_patches_perimeter,
                       R0801_patches_perimeter, R0815_patches_perimeter, R0901_patches_perimeter,
                       R0915_patches_perimeter, R1001_patches_perimeter, R1015_patches_perimeter)
    
    ListTimes = c("0315", "0401", "0415", "0501", "0515", "0601", "0615", "0701", "0715",
                  "0801", "0815", "0901", "0915", "1001", "1015")
    
    # Save origin and goal patches
    for (k in 1:(length(ListTimes)-1)) {
      
      print(ListTimes[k])
      
      Origin = as.data.frame(ListPatches[k])
      Goal = as.data.frame(ListPatches[k+1])
      
      # Save result
      fwrite(Origin, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                            Name, "/", Sp, "_", ListTimes[k], "_", "Origin", ".csv"))
      fwrite(Goal, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                          Name, "/", Sp, "_", ListTimes[k], "_", "Goal", ".csv"))
    }
    
    
    # Spring
    print("Transition Spring")
    Raster_TRANSITION_wtNA_SPRING = Raster_TRANSITION_SPRING
    Raster_TRANSITION_wtNA_SPRING[is.na(Raster_TRANSITION_wtNA_SPRING)] <- 0 # replace NA by 0 because passage function does not like NA
    land_cost_sub_SPRING <- crop(Raster_TRANSITION_wtNA_SPRING, Raster_extent)
    land_cost_sub_SPRING <- transition(land_cost_sub_SPRING, transitionFunction = mean, 8)
    land_cost_sub_SPRING <- geoCorrection(land_cost_sub_SPRING, type = "r", scl = T)
    saveRDS(land_cost_sub_SPRING, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                                         Name, "/", Sp, "_", "Spring_", "Transition", ".rds"))
    
    # Autumn
    print("Transition Autumn")
    Raster_TRANSITION_wtNA_AUTUMN = Raster_TRANSITION_AUTUMN
    Raster_TRANSITION_wtNA_AUTUMN[is.na(Raster_TRANSITION_wtNA_AUTUMN)] <- 0 # replace NA by 0 because passage function does not like NA
    land_cost_sub_AUTUMN <- crop(Raster_TRANSITION_wtNA_AUTUMN, Raster_extent)
    land_cost_sub_AUTUMN <- transition(land_cost_sub_AUTUMN, transitionFunction = mean, 8)
    land_cost_sub_AUTUMN <- geoCorrection(land_cost_sub_AUTUMN, type = "r", scl = T)
    saveRDS(land_cost_sub_AUTUMN, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/", 
                                         Name, "/", Sp, "_", "Autumn_", "Transition", ".rds"))
    
  }
  
  if(AggregatedPixel){
    print("AggregatedPixel")
    # select the highest value of each pixel across the year (march to october)
    dataaPix = subset(file_bind, file_bind$Species == Sp)
    dataa_ALLYEAR = subset(dataaPix, as.numeric(dataaPix$Month)<11 & as.numeric(dataaPix$Month)>2) 
    
    data_highest_value_YEAR = dataa_ALLYEAR %>%                                     
      arrange(desc(pred)) %>% 
      group_by(X, Y) %>%
      slice(1)
    
    Raster_TRANSITION_YEAR = Rasterize_function(data_highest_value_YEAR)
    writeRaster(Raster_TRANSITION_YEAR, 
                filename=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/MaxValuePixel/", Name, "/",
                                Sp, "_", "YEAR_", "highestvaluepixel", ".tif"), 
                overwrite=TRUE)
  }
}


END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF

beep(2)


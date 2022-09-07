

# Create patches for movement model

library(data.table)
library(tidyverse)
library(viridis)
library(raster)
library(sf)
library(gdistance)
library(rgeos)
library(igraph)
library(landscapemetrics)
library(beepr)

# Load acoustic predictions
arg <- "C:/Users/croemer01/Documents/Donnees vigie-chiro/PredictionsModels/weighted_2022-08-11" # repertory with outputs from Predict_act
Sp = "Nyclei"

list_file <- list.files(arg[1],recursive=FALSE,pattern="*.csv$")
ls2 = paste(paste0(arg[1],"/",list_file, sep=""))
ld <- lapply(ls2, function(x) read_csv(x))
ld <- mapply(cbind, ld, "Species"=tstrsplit(list_file,split="_")[[1]], SIMPLIFY=F) # add column with species name
ld <- mapply(cbind, ld, "Month"=tstrsplit(tstrsplit(list_file,split="_")[[3]], split="-")[[2]], SIMPLIFY=F) # add column with month name
ld <- mapply(cbind, ld, "Day"=tstrsplit(tstrsplit(list_file,split="_")[[3]], split="-")[[3]], SIMPLIFY=F) # add column with day name

file_bind <- do.call("rbind",ld)

# Back-transform predictions
file_bind$pred=10^(file_bind$pred)

dataa = subset(file_bind, file_bind$Species == Sp)
dataa_START=subset(dataa, dataa$Month=="04" & dataa$Day=="01") # START (1rst April? --> need method to determine)
dataa_SPRING=subset(dataa, dataa$Month=="05" & dataa$Day=="15") # for transition matrix
dataa_MID=subset(dataa, dataa$Month=="07" & dataa$Day=="01") # MID WAY
dataa_AUTUMN=subset(dataa, dataa$Month=="09" & dataa$Day=="01") # for transition matrix
dataa_END=subset(dataa, dataa$Month=="10" & dataa$Day=="15") # END

quantile(dataa_START$pred, c(0.1, 0.55, 0.65, 0.8))
dataa_START$Class = ifelse(dataa_START$pred<quantile(dataa_START$pred, 0.1), "Not suitable", "Less suitable")
dataa_START$Class = ifelse(dataa_START$pred>=quantile(dataa_START$pred, 0.55), "Moderately suitable", dataa_START$Class)
dataa_START$Class = ifelse(dataa_START$pred>=quantile(dataa_START$pred, 0.65), "Suitable", dataa_START$Class)
dataa_START$Class = ifelse(dataa_START$pred>=quantile(dataa_START$pred, 0.8), "Highly suitable", dataa_START$Class)

# Check
dataa_START$Class = factor(dataa_START$Class, levels=c("Not suitable", "Less suitable", "Moderately suitable", "Suitable", "Highly suitable"))

ggplot() +
  geom_point(data = dataa_START, 
             mapping = aes(x=Group.1, y=Group.2, col=Class)) + 
  scale_color_viridis(discrete=T)

# Raster of observations (START)
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

Raster_START = Q80_function(Rasterize_function(dataa_START))
#dataa_START_Q80 = dataa_START %>%  filter(pred>quantile(pred, 0.8))
Raster_SPRING = Rasterize_function(dataa_SPRING)
Raster_MID = Q80_function(Rasterize_function(dataa_MID))
#dataa_MID_Q80 = dataa_MID %>%  filter(pred>quantile(pred, 0.8))
Raster_AUTUMN = Rasterize_function(dataa_AUTUMN)
Raster_END= Q80_function(Rasterize_function(dataa_END))
#dataa_END_Q80 = dataa_END %>%  filter(pred>quantile(pred, 0.8))

cost_SPRING = 1/Raster_SPRING
tr.cost_SPRING <- gdistance::transition(cost_SPRING, transitionFunction=mean, directions=8) 

# Crop data in box (optional)
Raster_extent_START = extent(-1, 6, 42, 44) 
Raster_extent_MID = extent(-1, 6, 48, 52) 
#Raster_extent = extent(Raster_START)
Raster_START_sub <- crop(Raster_START, Raster_extent_START)
Raster_MID_sub=crop(Raster_MID, Raster_extent_MID)
land_cost_sub <- crop(cost_SPRING, Raster_extent)
#land_cost_sub <- reclassify(land_cost_sub, cbind(NA, 0))
land_cond_sub <- transition(land_cost_sub, transitionFunction = mean, 8)
#land_cond_sub <- geoCorrection(land_cond_sub, type = "r")

# Clump pixels and remove small patches
Raster_START_patches <- clump(Raster_START_sub, directions=8, gaps=F)
Raster_START_patches <- reclassify(Raster_START_patches, cbind(-Inf, 100, NA), right=FALSE) # Remove patches containing less than x pixels
Raster_START_patches_centroids <- get_centroids(Raster_START_patches) 

Raster_MID_patches <- clump(Raster_MID_sub, directions=8, gaps=F)
Raster_MID_patches <- reclassify(Raster_MID_patches, cbind(-Inf, 100, NA), right=FALSE) # Remove patches containing less than x pixels
Raster_MID_patches_centroids <- get_centroids(Raster_MID_patches) 

# Create origin and goal points
Origin = SpatialPoints(cbind(Raster_START_patches_centroids$x, Raster_START_patches_centroids$y))
Goal = SpatialPoints(cbind(Raster_MID_patches_centroids$x, Raster_MID_patches_centroids$y))

# # Calculate shortest paths between first cell and goals
# fpwr_ossf_lcp <- shortestPath(land_cond_sub,
#                               Origin, Goal,
#                               output = "SpatialLines")
# 
# # Plot
# plot(land_cost_sub, axes = F)
# #plot(Raster_START)
# plot(Raster_START_patches, add = T)
# points(Origin, col = "black", pch="+")
# points(Goal, col = "white", pch="+")
# lines(fpwr_ossf_lcp, col = "red")

# fpwr.cost <- accCost(land_cond_sub, Origin)
# ossf.cost <- accCost(land_cond_sub, Goal)
# 
# leastcost_corridor <- overlay(fpwr.cost, ossf.cost, fun = function
#                               (x, y){return (x + y)})
# 
# quantile10 <- quantile(leastcost_corridor, probs = 0.10, na.rm =
#                          TRUE)
# leastcost_corridor10 <- leastcost_corridor
# values(leastcost_corridor10) <- NA
# leastcost_corridor10[leastcost_corridor < quantile10] <- 1
# 
# plot(leastcost_corridor10,legend = F,axes = F)
# points(Origin, col = "black")
# lines(fpwr_ossf_lcp, col = "red")

#random walk (akin to a current map in Circuitscape)
passage.map_t0 <- passage(land_cond_sub,
                          Origin, Goal, theta = 0)

#alter theta to converge on a lcp
passage.map_t001 <- passage(land_cond_sub,
                            Origin, Goal, theta = 0.1)
plot(passage.map_t0)
plot((passage.map_t001))
points(Origin, col = "black", pch="+")
points(Goal, col = "black", pch="+")

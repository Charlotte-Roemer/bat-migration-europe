
# First run SpNuit_ModActPt

# Then test spatial correlation

library(beepr)
library(spatialRF)

n_rows=10 # resample by dividing dataset by n to go faster
a <- seq(1, nrow(DataSaison), by = n_rows)
DataSaisonSample <- DataSaison[a,]

xy93 = DataSaisonSample[,c("longitude", "latitude", "nb_contacts")] %>% # transform to L93 to obtain spatial distances in km
  st_as_sf(coords=c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(2154) 
xy93 = xy93 %>% 
  mutate(lat = unlist(map(xy93$geometry,1)),
         long = unlist(map(xy93$geometry,2))) %>% 
  as.data.frame() %>% 
  select(lat, long, nb_contacts)
xy93$lat_km = xy93$lat/1000
xy93$long_km = xy93$long/1000
xy93$x = xy93$long
xy93$y = xy93$lat

# https://blasbenito.github.io/spatialRF/

dist.matrix = as.matrix(dist(cbind(xy93$x, xy93$y)))
dep.name = "ActLog10"
pred.names = colnames(Predictors)
xy <- xy93[, c("x", "y")]

#distance thresholds (same units as distance_matrix)
distance.thresholds <- c(0, 50, 100, 200, 300, 500, 1000, 2000, 5000, 10000, 20000)

# Build non spatial model
model.non.spatial <- spatialRF::rf(
  data = DataSaisonSample,
  dependent.variable.name = dep.name,
  predictor.variable.names = pred.names,
  distance.matrix = dist.matrix,
  distance.thresholds = distance.thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  verbose = FALSE
)

# Plot diagnostic of the residuals
png(filename=paste(Output, "/", Sp,"_Moran_I_residuals.png",sep=""),
    height=1000, width=2000,res=150)
plot5=spatialRF::plot_residuals_diagnostics(
  model.non.spatial,
  verbose = FALSE
)
print(plot5)
dev.off()
# 
# # Only the Moran's I
# spatialRF::plot_moran(
#   model.non.spatial,
#   verbose = FALSE
# )
# 
# # Build spatial model
# model.spatial <- spatialRF::rf_spatial(
#   model = model.non.spatial,
#   method = "mem.moran.sequential", #default method
#   verbose = FALSE
# )
# 
# # Plot Moran's I
# spatialRF::plot_moran(
#   model.spatial,
#   verbose = FALSE
# )
# 
# # Model performance
# spatialRF::print_performance(model.non.spatial)

# # Importance
# png(filename=paste(Output, "/", Sp,"_importance.png",sep=""),
#     height=5000, width=500,res=150)
# plot4 = spatialRF::plot_importance(
#   model.non.spatial,
#   verbose = FALSE
# )
# print(plot4)
# dev.off()
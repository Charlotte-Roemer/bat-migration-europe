
# First run SpNuit_ModActPt

# Then test spatial correlation

library(beepr)
#library(spatialRF)
library(randomForest)

Sp="Nyclei"
Threshold="weighted"

ModRF_file=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VCweightedPG_2022-08-11/ModRFActLog_",
                  Sp,"VC",Threshold,"_PG.learner")
load(ModRF_file) # Load random forest model

# ! Problem : le RF construit se base sur plein de jeux de données train de tailles différentes
# donc on ne peux pas calculer les résidus classiquement : ResidualsRF = DataSaison$ActLog10 - predict(ModRF)
# il faut faire un prédict sur le jeu de données complet, puis soustraire les résidus !!

DataSaison_for_residuals = cbind(DataSaison[,..Prednames], 
                                 ActLog10 = DataSaison$ActLog10)



library(viridis)
ggplot(DataSaison, aes(x=longitude, 
                             y=latitude, 
                             col=Residuals_RF)) +
  geom_point() +
  scale_color_viridis()


# Moran's I
library(ncf)

n_rows=10 # resample by dividing dataset by n to go faster
a <- seq(1, nrow(DataSaison_for_residuals), by = n_rows)
DataSaisonSample <- DataSaison_for_residuals[a,]

Predicts_RF = predict(ModRF, DataSaisonSample)
Residuals_RF =  DataSaisonSample$ActLog10 - Predicts_RF

ncf.cor <- correlog(DataSaisonSample$longitude, DataSaisonSample$latitude, 
                    Residuals_RF,
                    increment=2, resamp=500)






DataSaisonSample$CESCO = ifelse(DataSaisonSample$observateur %in% c("Alexis LAFORGE",
                                                                    "Aurélie Lacoeuilhe",
                                                                    "Camille Leroux",
                                                                    "christian kerbiriou",
                                                                    "Clementine Azam",
                                                                    "Fabien CLAIREAU",
                                                                    "Jérémy Froidevaux",
                                                                    "Julie Pauwels",
                                                                    "Kévin Barré",
                                                                    "Léa MARITON",
                                                                    "Tiphaine Devaux"),
                                "OUI", "NON")

DataSaisonSample=subset(DataSaisonSample, CESCO == "NON")



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

# Check spatial distribution of residuals
library(viridis)
ggplot(DataSaisonSample, aes(x=DataSaisonSample$longitude, 
                             y=DataSaisonSample$latitude, 
                             col=log10(model.non.spatial$residuals$values+2))) +
         geom_point() +
  scale_color_viridis()

# Check CESCO data
ggplot(DataSaisonSample, aes(x=DataSaisonSample$longitude, 
                             y=DataSaisonSample$latitude, 
                             col=DataSaisonSample$CESCO)) +
  geom_point()





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
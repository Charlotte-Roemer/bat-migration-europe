

library(glmmTMB)
library(beepr)
library(tidyverse)
library(ggeffects)
library(DHARMa)
library(sf)
library(bestNormalize)
library(performance)

Sp = "Nyclei"
variableName = "SpHO16_17S" # Percentage of coniferous + deciduous forests in 50m

Directory = "C:/Users/croemer01/Documents/Donnees vigie-chiro/Influence variables sur Probas/" # where to put the results
MaterialCorresp = read_delim("C:/Users/croemer01/Documents/Donnees vigie-chiro/Matos_correspondances.csv") # correspondance between names given by participants and reality
ListRecorders = c("Anabat Swift", "Audiomoth", "Batlogger", "D500", "PassiveRecorder", "SM2BAT", # list of recorders for fix points
                  "SM2BAT+", "SM3", "SM4")
DataSaisonSub = read_delim(paste0(Directory, Sp, "_DataSaisonProbabilitePaysage_202309.csv")) %>%  # table with probabilities for each sequence
  filter(detecteur_enregistreur_type %in% ListRecorders) %>% 
  filter(micro0_type != "Micro externe avec cornet")

# Filter data out of France mainland
FRANCE <- map_data("world", region = "France")
FRANCE_sf = st_as_sf(
  FRANCE, coords = c("long", "lat"), crs=4326, remove=FALSE)  %>%
  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

Site_localite_sf = st_as_sf(
  DataSaisonSub, coords = c("longitude", "latitude"), crs=4326, remove=FALSE)

rm(DataSaisonSub)

Site_localite_FRANCE_sf = st_filter (Site_localite_sf, FRANCE_sf) %>% 
  as.data.frame() %>% 
  select(-geometry)

rm(Site_localite_sf)

# Add combination microphone/recorder
# Keep only samples of the first night for each site to avoid unbalanced sampling
DataSaisonSub0 = left_join(Site_localite_FRANCE_sf, MaterialCorresp) %>% 
  group_by(longitude, latitude) %>%
  filter(DateNuit==first(DateNuit))

# Rewrite mic name
DataSaisonSub0$detecteur_micro[which(DataSaisonSub0$detecteur_micro=="PassiveRecorder Autre micro externe")] = "PassiveRecorder Micro externe sans cornet"

# Keep only combinations that have entries which, summed up, make more than 1% of the dataset
if(Sp == "Pippip"){
  TEST = names(subset(table(DataSaisonSub0$detecteur_micro), table(DataSaisonSub0$detecteur_micro)>10/100*nrow(DataSaisonSub0)))
  DataSaisonSub00 = subset(DataSaisonSub0, DataSaisonSub0$detecteur_micro %in% TEST)
}else{
  TEST = names(subset(table(DataSaisonSub0$detecteur_micro), table(DataSaisonSub0$detecteur_micro)>1/100*nrow(DataSaisonSub0)))
  DataSaisonSub00 = subset(DataSaisonSub0, DataSaisonSub0$detecteur_micro %in% TEST)
}

# Plot material distribution
new_order <- with(subset(DataSaisonSub00, DataSaisonSub00$detecteur_micro != "SM2BAT Autre micro externe"), 
                  reorder(detecteur_micro , SpHO16_17S, median , na.rm=T))
DataSaisonSub00 %>% 
  filter(detecteur_micro != "SM2BAT Autre micro externe") %>% 
  ggplot(aes(x=new_order, y=SpHO16_17S)) +
  geom_violin(width = 3) +
  stat_summary(fun.y=median, geom="point", size=2, color="red") +
  theme(axis.text.x=element_text(angle=45,hjust=1, size = 15))

# Keep only combinaisons that have a balanced sample of the forests, i.e., with a SD > 0.1 and median between 0.2 and 0.8
SD_table = DataSaisonSub00 %>% 
  drop_na(SpHO16_17S) %>% 
  group_by(detecteur_micro) %>% 
  summarise(Median = median(SpHO16_17S), SD = sd(SpHO16_17S))
MaterialOK = SD_table$detecteur_micro[which(SD_table$Median>0.2 & SD_table$Median<0.8 & SD_table$SD > 0.1)]
DataSaisonSub000 = DataSaisonSub00 %>% 
  filter(detecteur_micro %in% MaterialOK)

# To use a beta family, needs to transform 1 to 0.99 (allowed because very few cases)
DataSaisonSub2 = DataSaisonSub000
DataSaisonSub2$probabilite[which(DataSaisonSub2$probabilite==1)]=0.99
# To use a beta family with zero inflated, needs to transform 1 to 0.99 (allowed because very few cases)
# and then model the inverse response
DataSaisonSub3 = DataSaisonSub000
DataSaisonSub3$probabilite = 1-DataSaisonSub3$probabilite
#DataSaisonSub3$probabilite[which(DataSaisonSub3$probabilite==1)]=0.99
DataSaisonSub3$probabilite[which(DataSaisonSub3$probabilite<0.011)]=0 # inflated not in 0 but in this value
# #Normalize data
# ReponseNormalize = bestNormalize(DataSaisonSub000$probabilite)
# orderNorm_proba<-orderNorm (DataSaisonSub000$probabilite)
# DataSaisonSub4 = cbind(DataSaisonSub000, orderNorm_proba$x.t)
# names(DataSaisonSub4)[which(names(DataSaisonSub4)=="...12")]="proba_normalize"
# #Normalize data + ZI
# DataSaisonSub5 = DataSaisonSub4
# DataSaisonSub5$proba_normalize = 1-DataSaisonSub5$proba_normalize
# DataSaisonSub5$proba_normalize[which(DataSaisonSub5$proba_normalize<0.011)]=0 # inflated not in 0 but in this value

Formula=as.formula(paste0("probabilite~", 
                          #variableName, "+I(", variableName, "^2)", 
                          variableName,
                          "+detecteur_micro",
                          "+ (1|idsite/nom)"))

# FormulaNormalize=as.formula(paste0("proba_normalize~", 
#                           variableName, "+I(", variableName, "^2)", 
#                           #variableName,
#                           "+detecteur_micro",
#                           "+ (1|idsite/nom)"))


ModBinomialProbit = glmmTMB(Formula,
                            data = DataSaisonSub000,
                            family = binomial (link = "probit"))
ModBeta = glmmTMB(Formula,
                  data = DataSaisonSub2,
                  family = beta_family()) 
ModBetaZI = glmmTMB(Formula,
                    data = DataSaisonSub3,
                    ziformula = ~1,
                    family = beta_family())

# ModBetaNormalize = glmmTMB(FormulaNormalize,
#                   data = DataSaisonSub4,
#                   family = gaussian()) 
# ModBetaNormalizeZI = glmmTMB(FormulaNormalize,
#                            data = DataSaisonSub4,
#                            ziformula = ~1,
#                            family = gaussian()) 



# NON !!!! car les variables réponse ont été transformées
# AIC(ModBinomialProbit)
# AIC(ModBinomialProbit1_0.99)
# AIC(ModBeta)
# AIC(ModBetaZI)
# AIC(ModBetaNormalize)

R2_BinomialProbit = r2(ModBinomialProbit)
R2_Beta = r2(ModBeta)
R2_BetaZI = r2_zeroinflated(ModBetaZI)
# r2(ModBetaNormalize)
# r2_zeroinflated(ModBetaNormalizeZI)

simulationOutput_BinomialProbit <- simulateResiduals(fittedModel = ModBinomialProbit, plot = F)
simulationOutput_Beta <- simulateResiduals(fittedModel = ModBeta, plot = F)
simulationOutput_BetaZI <- simulateResiduals(fittedModel = ModBetaZI, plot = F)

png(filename=paste(Directory, Sp,"_BinomialProbit.png",sep=""), height=700, width=900,res=150)
print(plotQQunif(simulationOutput = simulationOutput_BinomialProbit))
dev.off()

png(filename=paste(Directory, Sp,"_Beta.png",sep=""), height=700, width=900,res=150)
plotQQunif(simulationOutput = simulationOutput_Beta)
dev.off()

png(filename=paste(Directory, Sp,"_BetaZI.png",sep=""), height=700, width=900,res=150)
plotQQunif(simulationOutput = simulationOutput_BetaZI)
dev.off()

# # KS test for correct distribution of residuals
# KS_Beta = testUniformity(simulationOutput_Beta)
# KS_BetaZI = testUniformity(simulationOutput_BetaZI)
# 
# # tests under and overdispersion
# Dispersion_Beta = testDispersion(simulationOutput_Beta)
# Dispersion_BetaZI = testDispersion(simulationOutput_BetaZI)

BinomialProbitFIXEF <- as.data.frame(coef(summary(ModBinomialProbit))$cond)
BetaFIXEF <- as.data.frame(coef(summary(ModBeta))$cond)
BetaZIFIXEF <- as.data.frame(coef(summary(ModBetaZI))$cond)

# Build summary of all models
Summary_Models = data.frame("Model" = c(rep("BinomialProbit",2), 
                                        rep("Beta",2), rep("BetaZI",2)),
                            "Type" = c(rep(c("Conditional R2", "Marginal R2"),2), "R2", "Adjusted R2"),
                            "R2" = c(R2_BinomialProbit$R2_conditional[[1]], 
                                     R2_BinomialProbit$R2_marginal[[1]], 
                                     R2_Beta$R2_conditional[[1]], 
                                     R2_Beta$R2_marginal[[1]], 
                                     R2_BetaZI$R2[[1]],
                                     R2_BetaZI$R2_adjusted[[1]]),
                            "Estimate Forest" = c(rep(BinomialProbitFIXEF$Estimate[which(rownames(BinomialProbitFIXEF)==variableName)], 2),
                                                  rep(BetaFIXEF$Estimate[which(rownames(BetaFIXEF)==variableName)], 2),
                                                  rep(-BetaZIFIXEF$Estimate[which(rownames(BetaZIFIXEF)==variableName)], 2)), # inverse of estimate
                            "Std. Error Forest" = c(rep(BinomialProbitFIXEF$'Std. Error'[which(rownames(BinomialProbitFIXEF)==variableName)], 2),
                                                  rep(BetaFIXEF$'Std. Error'[which(rownames(BetaFIXEF)==variableName)], 2),
                                                  rep(BetaZIFIXEF$'Std. Error'[which(rownames(BetaZIFIXEF)==variableName)], 2)),
                            "p-value Forest" = c(rep(BinomialProbitFIXEF$'Pr(>|z|)'[which(rownames(BinomialProbitFIXEF)==variableName)], 2),
                                                    rep(BetaFIXEF$'Pr(>|z|)'[which(rownames(BetaFIXEF)==variableName)], 2),
                                                    rep(BetaZIFIXEF$'Pr(>|z|)'[which(rownames(BetaZIFIXEF)==variableName)], 2))
)

write_csv(Summary_Models, paste0(Directory, Sp, "_Summary_Table.csv"))

#plot(simulationOutput)
# testZeroInflation(simulationOutput)
# testDispersion(simulationOutput)
# testOutliers(simulationOutput)

#plot(simulationOutputZI)
# testZeroInflation(simulationOutputZI)

# DataSaisonSub000$coords <- paste(DataSaisonSub000$longitude,", ",DataSaisonSub000$latitude)
# coords <- c(unique(DataSaisonSub000$coords))
# x_unique <- c(str_extract(coords, "^.+(?=,)"))
# y_unique <- c(str_extract(coords, "(?<=, ).+$"))
# 
# Recalculate_Residuals = recalculateResiduals(simulationOutput, group = coords)
# plot(Recalculate_Residuals, quantreg = FALSE)
# testSpatialAutocorrelation(simulationOutput = Recalculate_Residuals, x = x_unique, y= y_unique)

#plot(simulationOutputProbit)
# testZeroInflation(simulationOutputProbit)
# 
#plot(simulationOutputProbit1_0.99)
# testZeroInflation(simulationOutputProbit1_0.99)

# countOnes <- function(x) sum(x == 1) 
# countZeros <- function(x) sum(x == 0) 
# testGeneric(simulationOutput, summary = countOnes, alternative = "greater")
# testGeneric(simulationOutputZI, summary = countZeros, alternative = "greater")
# 
# residualsModRF = residuals(ModBeta)
# xy93 = DataSaisonSub2[,c("longitude", "latitude", "probabilite")] %>% # transform to L93 to obtain spatial distances in km
#   st_as_sf(coords=c("longitude", "latitude")) %>% 
#   st_set_crs(4326) %>% 
#   st_transform(2154) 
# xy93 = xy93 %>% 
#   mutate(lat = unlist(map(xy93$geometry,1)),
#          long = unlist(map(xy93$geometry,2))) %>% 
#   as.data.frame() %>% 
#   select(lat, long, probabilite)
# xy=cbind(xy93$long, xy93$lat)
# pgi_cor <- pgirmess::correlog(coords=xy, z=residualsModRF, 
#                               method="Moran", nbclass=10) 
# plot(pgi_cor)

# saveRDS(ModBinomialProbit, paste0(Directory, "ModBinomialProbit.rds"))
# saveRDS(ModBetaZI, paste0(Directory, "ModBetaZI.rds"))
# saveRDS(ModBinomialProbit1_0.99, paste0(Directory, "ModBinomialProbit1_0.99.rds"))
# saveRDS(ModBeta, paste0(Directory, "ModBeta.rds"))



# prediction plot
# pr1 = ggpredict(ModBinomialProbit, c(terms = variableName))
#pr2 = ggpredict(ModBetaZI, c(terms = variableName))
# pr3 = ggpredict(ModBinomialProbit1_0.99, c(terms = variableName))
pr4 = ggpredict(ModBeta, c(terms = variableName))

# png(filename=paste(Directory, "/", Sp,"_Predict_ModBinomialProbit_", variableName, ".png",sep=""), 
#     height=700, width=900,res=300)
# plot1=ggplot(pr1, aes(x, predicted)) +
#   geom_line(size=1)  +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
#   labs(x = variableName,
#        y = "Probability of ID") +
#   scale_fill_discrete(guide=FALSE)+
#   ggtitle(Sp) + 
#   theme_bw(base_size = 15)
# print(plot1)
# dev.off()
# 
# png(filename=paste(Directory, "/", Sp,"_Predict_ModBetaZI_", variableName, ".png",sep=""),
#     height=700, width=900,res=300)
# plot2=ggplot(pr2, aes(x, 1-predicted)) +
#   geom_line(linewidth=1)  +
#   geom_ribbon(aes(ymin = 1-conf.low, ymax = 1-conf.high), alpha = .1)+
#   labs(x = variableName,
#        y = "Probability of ID") +
#   scale_fill_discrete(guide=FALSE)+
#   ggtitle(Sp) +
#   theme_bw(base_size = 15)
# print(plot2)
# dev.off()
# 
# png(filename=paste(Directory, "/", Sp,"_Predict_ModBinomialProbit1_0.99_", variableName, ".png",sep=""), 
#     height=700, width=900,res=300)
# plot3=ggplot(pr3, aes(x, predicted)) +
#   geom_line(size=1)  +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
#   labs(x = variableName,
#        y = "Probability of ID") +
#   scale_fill_discrete(guide=FALSE)+
#   ggtitle(Sp) + 
#   theme_bw(base_size = 15)
# print(plot3)
# dev.off()

png(filename=paste(Directory, Sp,"_Predict_ModBeta_", variableName, ".png",sep=""), 
    height=700, width=900,res=300)
plot4=ggplot(pr4, aes(x, predicted)) +
  geom_line(linewidth=1)  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  labs(x = variableName,
       y = "Probability of ID") +
  scale_fill_discrete(guide="none")+
  ggtitle(Sp) + 
  theme_bw(base_size = 15)
print(plot4)
dev.off()

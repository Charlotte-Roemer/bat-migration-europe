
library(tidyverse)
library(hydroGOF)
library(viridis)

Sp_list = c("Nyclei", "Nycnoc", "Eptser", "Pippip", "Pippyg", "Minsch", 
            "Pipkuh", "Pipnat", "Rhifer", "Barbar")

DF = data.frame()
for (i in 1:length(Sp_list)){
  
  Sp = Sp_list[i]
  ModWeighted = read_csv (paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VCweightedPG_2024-03-29/",
                           "ModRFActLog_",Sp,"_VC","weighted_2024-03-29","_evaluationrotated.csv"))
  ModWeightedBoruta = read_csv (paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VCweightedPG_2024-03-29/", 
                                 "ModRFActLog_",Sp,"_VC","weighted_2024-03-29","_evaluationpolar.csv"))
  
  NRMSE_Weighted_tree = ModWeighted %>%
    group_by(N_tree) %>%
    summarise(NRMSE = nrmse(simFinal , obsFinal , norm="maxmin"))
  NRMSE_Weighted_Boruta_tree = ModWeightedBoruta %>% 
    group_by(N_tree) %>% 
    summarise(NRMSE = nrmse(simFinal , obsFinal , norm="maxmin"))
  
  # NRMSE_Weighted =  nrmse(ModWeighted$simFinal , ModWeighted$obsFinal , norm="maxmin")
  # NRMSE_Weighted_Boruta =  nrmse(ModWeightedBoruta$simFinal , ModWeightedBoruta$obsFinal , norm="maxmin")
  
  DF_temp = data.frame(Threshold = c(rep("Weighted_rotated", 500), rep("Weighted_polar", 500)),
                       NRMSE = c(NRMSE_Weighted_tree$NRMSE, NRMSE_Weighted_Boruta_tree$NRMSE),
                       Species=Sp)
  
  DF = rbind(DF, DF_temp)
  
}

DF$Threshold = factor(DF$Threshold, levels=c("Weighted_rotated", "Weighted_polar"))
DF$Species = factor(DF$Species, levels=c("Eptser", "Nyclei", "Nycnoc", 
                                         "Pippip", "Pippyg", "Minsch", 
                                         'Pipnat', "Pipkuh",
                                         "Rhifer", "Barbar"))

png(filename=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/", "NRMSE_models_rotated_polar.png"), height=1500, width=2000,res=300)
plot2 = ggplot(DF, aes(x=Threshold, y = NRMSE, col=Threshold)) +
  geom_boxplot() +
  facet_wrap(~ Species, scales = "free") +
  scale_color_viridis(discrete=TRUE) +
  theme_classic()
print(plot2)
dev.off()


library(tidyverse)
library(hydroGOF)
library(viridis)

Sp_list = c("Nyclei", "Nycnoc", "Eptser", "Pippip", "Pippyg", "Pipkuh", "Pipnat", "Minsch")

DF = data.frame()
for (i in 1:length(Sp_list)){
  
  Sp = Sp_list[i]
  Mod0 = read_csv (paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VC0PG_2022-08-11/", 
                          "ModRFActLog_",Sp,"_VC","0","_evaluation.csv"))
  Mod50 = read_csv (paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VC50PG_2022-08-11/", 
                           "ModRFActLog_",Sp,"_VC","50","_evaluation.csv"))
  Mod90 = read_csv (paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VC90PG_2022-08-11/", 
                           "ModRFActLog_",Sp,"_VC","90","_evaluation.csv"))
  ModWeighted = read_csv (paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VCweightedPG_2022-08-11/", 
                                 "ModRFActLog_",Sp,"_VC","weighted","_evaluation.csv"))
  
  NRMSE_0_tree = Mod0 %>% 
    group_by(N_tree) %>% 
    summarise(NRMSE = nrmse(simFinal , obsFinal , norm="maxmin"))
  NRMSE_50_tree = Mod50 %>% 
    group_by(N_tree) %>% 
    summarise(NRMSE = nrmse(simFinal , obsFinal , norm="maxmin"))
  NRMSE_90_tree = Mod90 %>% 
    group_by(N_tree) %>% 
    summarise(NRMSE = nrmse(simFinal , obsFinal , norm="maxmin"))
  NRMSE_Weighted_tree = ModWeighted %>% 
    group_by(N_tree) %>% 
    summarise(NRMSE = nrmse(simFinal , obsFinal , norm="maxmin"))
  
  NRMSE_0 =  nrmse(Mod0$simFinal , Mod0$obsFinal , norm="maxmin")
  NRMSE_50 =  nrmse(Mod50$simFinal , Mod50$obsFinal , norm="maxmin")
  NRMSE_90 =  nrmse(Mod90$simFinal , Mod90$obsFinal , norm="maxmin")
  NRMSE_Weighted =  nrmse(ModWeighted$simFinal , ModWeighted$obsFinal , norm="maxmin")
  
  # NRMSE_0
  # mean(NRMSE_0_tree$NRMSE)
  # NRMSE_50
  # mean(NRMSE_50_tree$NRMSE)
  # NRMSE_90
  # mean(NRMSE_90_tree$NRMSE)
  # NRMSE_Weighted
  # mean(NRMSE_Weighted_tree$NRMSE)
  
  DF_temp = data.frame(Threshold = c(rep("0", 500), rep("50", 500), rep("90", 500), rep("Weighted", 500)),
                       NRMSE = c(NRMSE_0_tree$NRMSE, NRMSE_50_tree$NRMSE,
                                 NRMSE_90_tree$NRMSE, NRMSE_Weighted_tree$NRMSE),
                       Species=Sp)
  
  DF = rbind(DF, DF_temp)
  
}

DF$Threshold = factor(DF$Threshold, levels=c("0", "50", "90", "Weighted"))
DF$Species = factor(DF$Species, levels=c("Eptser", "Nyclei", "Nycnoc", "Pippip", "Pippyg", "Minsch", 'Pipnat', "Pipkuh"))

png(filename=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/", "NRMSE_models.png"), height=1500, width=2000,res=300)
plot2 = ggplot(DF, aes(x=Threshold, y = NRMSE, col=Threshold)) +
  geom_boxplot() +
  facet_wrap(~ Species, scales = "free") +
  scale_color_viridis(discrete=TRUE) +
  theme_classic()
print(plot2)
dev.off()

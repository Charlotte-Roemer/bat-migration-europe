
library(tidyverse)
library(hydroGOF)
library(viridis)
library(ggh4x)

Sp_list = c("Nyclei", "Nycnoc", "Eptser", "Pippip", "Pippyg", "Minsch", 
            "Pipkuh", "Pipnat", "Rhifer", "Barbar")

DF = data.frame()
for (i in 1:length(Sp_list)){
  
  Sp = Sp_list[i]
  Mod0 = read_csv (paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VC0PG_2023-11-17/", 
                          "ModRFActLog_",Sp,"_VC","0","_2021-12-31", "_evaluation.csv"))
  Mod50 = read_csv (paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VC50PG_2023-11-17/", 
                           "ModRFActLog_",Sp,"_VC","50","_2021-12-31", "_evaluation.csv"))
  Mod90 = read_csv (paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VC90PG_2023-11-17/", 
                           "ModRFActLog_",Sp,"_VC","90","_2021-12-31", "_evaluation.csv"))
  ModWeighted = read_csv (paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VCweightedPG_2023-11-17/", 
                                 "ModRFActLog_",Sp,"_VC","weighted","_2021-12-31", "_evaluation.csv"))
  
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

# Read number of observations per species per threshold
DF0 =data.table::fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/SpNuit2_0_DataLP_PF_exportTot.csv")
DF50 =data.table::fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/SpNuit2_50_DataLP_PF_exportTot.csv")
DF90 =data.table::fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/SpNuit2_90_DataLP_PF_exportTot.csv")
DFweighted =data.table::fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/SpNuit2_weighted_DataLP_PF_exportTot.csv")
Species_List = data.table::fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/SpeciesList.csv")

Dim_0 = DF0 %>% 
  filter(espece %in% Species_List$Esp[Species_List$Group=="bat"],
         num_micro == 0) %>% 
  count(espece, sort = TRUE) %>% 
  mutate(Threshold = "0") %>% 
  rename(Species = espece)
Dim_50 = DF50 %>% 
  filter(espece %in% Species_List$Esp[Species_List$Group=="bat"],
         num_micro == 0) %>% 
  count(espece, sort = TRUE) %>% 
  mutate(Threshold = "50") %>% 
  rename(Species = espece)
Dim_90 = DF90 %>% 
  filter(espece %in% Species_List$Esp[Species_List$Group=="bat"],
         num_micro == 0) %>% 
  count(espece, sort = TRUE) %>% 
  mutate(Threshold = "90") %>% 
  rename(Species = espece)
Dim_weighted = DFweighted %>% 
  filter(espece %in% Species_List$Esp[Species_List$Group=="bat"],
         num_micro == 0) %>% 
  count(espece, sort = TRUE) %>% 
  mutate(Threshold = "Weighted") %>% 
  rename(Species = espece)

Dim_DF = rbind(Dim_0, Dim_50) %>% 
  rbind(Dim_90) %>% 
  rbind(Dim_weighted) %>% 
  filter(Species %in% DF$Species)

Dim_DF = Dim_DF %>% 
  arrange(
    factor(Species, levels = c("Eptser", "Nyclei", "Nycnoc", 
                               "Pippip", "Pippyg", "Minsch", 
                               'Pipnat', "Pipkuh",
                               "Rhifer", "Barbar")),
    factor(Threshold, levels = c("0", "50", "90", "Weighted"))
  )

DF2 = left_join(DF, Dim_DF)

DF2$n=as.character(DF2$n)
# DF2$Species = factor(DF2$Species, levels=c("Eptser", "Nyclei", "Nycnoc",
#                                          "Pippip", "Pippyg", "Minsch",
#                                          'Pipnat', "Pipkuh",
#                                          "Rhifer", "Barbar"))
DF2$Species_Full[DF2$Species=="Nyclei"] = "Nyctalus leisleri"
DF2$Species_Full[DF2$Species=="Eptser"] = "Eptesicus serotinus"
DF2$Species_Full[DF2$Species=="Nycnoc"] = "Nyctalus noctula"
DF2$Species_Full[DF2$Species=="Pippip"] = "Pipistrellus pipistrellus"
DF2$Species_Full[DF2$Species=="Pippyg"] = "Pipistrellus pygmaeus"
DF2$Species_Full[DF2$Species=="Minsch"] = "Miniopterus schreibersii"
DF2$Species_Full[DF2$Species=="Pipnat"] = "Pipistrellus nathusii"
DF2$Species_Full[DF2$Species=="Pipkuh"] = "Pipistrellus kuhlii"
DF2$Species_Full[DF2$Species=="Rhifer"] = "Rhinolophus ferrumequinum"
DF2$Species_Full[DF2$Species=="Barbar"] = "Barbastella barbastellus"

DF2$Species_Full = factor(DF2$Species_Full, levels=c("Eptesicus serotinus", "Nyctalus leisleri", "Nyctalus noctula",
                                                     "Pipistrellus pipistrellus", "Pipistrellus pygmaeus", "Miniopterus schreibersii",
                                                     'Pipistrellus nathusii', "Pipistrellus kuhlii",
                                                     "Rhinolophus ferrumequinum", "Barbastella barbastellus"))
DF2$Threshold = factor(DF2$Threshold, levels=c("0", "50", "90", "Weighted"))

design <- "
ABC
DEF
GH#
IJ#
"

png(filename=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/", "NRMSE_models.png"), height=2000, width=2000,res=300)
plot2 = ggplot(DF2, aes(x=Threshold, y = NRMSE, col=Threshold)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width=0.1, size=0.1) +
  scale_color_viridis(discrete=TRUE, guide="none") +
  theme_classic() +
  stat_summary(geom = 'text', label = Dim_DF$n, fun = max, vjust = -1,
               colour="black",
               size = 2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  facet_manual(~Species_Full, scales = "free", design = design)
print(plot2)
dev.off()

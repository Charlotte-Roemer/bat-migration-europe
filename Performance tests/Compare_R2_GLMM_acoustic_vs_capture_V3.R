
library(hydroGOF)
library(tidyverse)
library(viridis)

# Compare with Boruta ?
DoBoruta = F

ModRF_directory="C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/"

# Load NRMSE tables
list_NRMSE = list.files(ModRF_directory, pattern = "*_r2_capture_acoustic_500m.csv")
list_NRMSE2 = lapply(paste0(ModRF_directory, list_NRMSE), function(x) read_csv(x))
list_NRMSE_all = list_NRMSE2

# Join
Table_NRMSE = list_NRMSE_all %>%  
  bind_rows()

ListSp = c("Eptser", "Nyclei", "Nycnoc", "Pippip", "Pippyg", "Minsch", "Pipnat", "Pipkuh", "Rhifer", "Barbar")

# # Prepare table for plot
# Table_NRMSE2 = Table_NRMSE %>% 
#   pivot_longer(-Type, names_to = "Species", values_to = "NRMSE") %>% 
#   as.data.frame()
# Table_AIC2 = Table_AIC %>% 
#   pivot_longer(-Type, names_to = "Species", values_to = "AIC") %>% 
#   as.data.frame()

Table_NRMSE2 = subset(Table_NRMSE,Table_NRMSE$Type == "Marginal R2")

Table_NRMSE2$Group = NA
Table_NRMSE2$Group = ifelse(Table_NRMSE2$Species %in% c("Rhifer", "Barbar"), "Group 4", Table_NRMSE2$Group)
Table_NRMSE2$Group = ifelse(Table_NRMSE2$Species %in% c("Eptser", "Nyclei", "Nycnoc"), "Group 3", Table_NRMSE2$Group)
Table_NRMSE2$Group = ifelse(Table_NRMSE2$Species %in% c("Pippip", "Pippyg", "Minsch"), "Group 2", Table_NRMSE2$Group)
Table_NRMSE2$Group = ifelse(Table_NRMSE2$Species %in% c("Pipkuh", "Pipnat"), "Group 1", Table_NRMSE2$Group)

# Plot
Table_NRMSE2$Species = factor(Table_NRMSE2$Species, levels=c(ListSp))
Table_NRMSE2$Group = factor(Table_NRMSE2$Group, levels=c("Group 1", "Group 2", "Group 3", "Group 4"))

png(filename=paste0(ModRF_directory, "R2_Capture-acoustic.png"), height=1500, width=1500,res=300)
plot1=Table_NRMSE2 %>% 
  ggplot(aes(Species, R2, col=Threshold)) +
  facet_wrap(~ Species, ncol= 3, scales = "free") +
  geom_jitter(size=2, width = 0.15) +
  #scale_y_log10() +
  scale_color_viridis(discrete=TRUE) 
print(plot1)
dev.off()







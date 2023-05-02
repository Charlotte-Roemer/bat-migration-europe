
library(hydroGOF)
library(tidyverse)
library(viridis)


# Load NRMSE tables
ModRF_directory="C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/"
Serotules = read_csv(paste0(ModRF_directory, "Serotules", "_NRMSE_capture_acoustic.csv"))
Pip50 = read_csv(paste0(ModRF_directory, "Pip50", "_NRMSE_capture_acoustic.csv"))
Pip35 = read_csv(paste0(ModRF_directory, "Pip35", "_NRMSE_capture_acoustic.csv"))

# Load AIC tables
AIC_Serotules = read_csv(paste0(ModRF_directory, "Serotules", "_AIC_capture_acoustic.csv"))
AIC_Pip50 = read_csv(paste0(ModRF_directory, "Pip50", "_AIC_capture_acoustic.csv"))
AIC_Pip35 = read_csv(paste0(ModRF_directory, "Pip35", "_AIC_capture_acoustic.csv"))

# Join
Table_NRMSE = left_join(Serotules, Pip50) %>% 
  left_join (Pip35) %>% 
  as.data.frame()
Table_AIC = left_join(AIC_Serotules, AIC_Pip50) %>% 
  left_join (AIC_Pip35) %>% 
  as.data.frame()

# Calculate mean
ListSp = c("Eptser", "Nyclei", "Nycnoc", "Pippip", "Pippyg", "Minsch", "Pipnat", "Pipkuh")
Table_NRMSE$Mean = apply(Table_NRMSE[, colnames(Table_NRMSE) %in% ListSp], 1, function(x) mean(x))
#Table_AIC$Mean = apply(Table_AIC[, colnames(Table_AIC) %in% ListSp], 1, function(x) mean(x))
Table_AIC$Eptser_rank = rank(Table_AIC$Eptser)
Table_AIC$Nyclei_rank = rank(Table_AIC$Nyclei)
Table_AIC$Nycnoc_rank = rank(Table_AIC$Nycnoc)
Table_AIC$Pippip_rank = rank(Table_AIC$Pippip)
Table_AIC$Pippyg_rank = rank(Table_AIC$Pippyg)
Table_AIC$Minsch_rank = rank(Table_AIC$Minsch)
Table_AIC$Pipkuh_rank = rank(Table_AIC$Pipkuh)
Table_AIC$Pipnat_rank = rank(Table_AIC$Pipnat)
#Table_AIC$Mean_rank = apply(Table_AIC[, colnames(Table_AIC) %in% paste0(ListSp, "_rank")], 1, function(x) mean(x))
Table_AIC$Eptser_rank = NULL
Table_AIC$Nyclei_rank = NULL
Table_AIC$Nycnoc_rank = NULL
Table_AIC$Pippip_rank = NULL
Table_AIC$Pippyg_rank = NULL
Table_AIC$Minsch_rank = NULL
Table_AIC$Pipkuh_rank = NULL
Table_AIC$Pipnat_rank = NULL

# Prepare table for plot
Table_NRMSE2 = Table_NRMSE %>% 
  pivot_longer(-Type, names_to = "Species", values_to = "NRMSE") %>% 
  as.data.frame()
Table_AIC2 = Table_AIC %>% 
  pivot_longer(-Type, names_to = "Species", values_to = "AIC") %>% 
  as.data.frame()

Table_NRMSE2$Group = ifelse(Table_NRMSE2$Species %in% c("Eptser", "Nyclei", "Nycnoc"), "Group 3", "Group 2")
Table_NRMSE2$Group = ifelse(Table_NRMSE2$Species %in% c("Pipkuh", "Pipnat"), "Group 1", Table_NRMSE2$Group)
Table_NRMSE2$Group = ifelse(Table_NRMSE2$Species == "Mean", "Mean", Table_NRMSE2$Group)
Table_AIC2$Group = ifelse(Table_AIC2$Species %in% c("Eptser", "Nyclei", "Nycnoc"), "Group 3", "Group 2")
Table_AIC2$Group = ifelse(Table_AIC2$Species %in% c("Pipkuh", "Pipnat"), "Group 1", Table_AIC2$Group)
#Table_AIC2$Group = ifelse(Table_AIC2$Species == "Mean_rank", "Mean_rank", Table_AIC2$Group)

# Plot
Table_NRMSE2$Species = factor(Table_NRMSE2$Species, levels=c(ListSp, "Mean"))
Table_NRMSE2$Group = factor(Table_NRMSE2$Group, levels=c("Group 1", "Group 2", "Group 3", "Mean"))

png(filename=paste0(ModRF_directory, "NRMSE.png"), height=1000, width=1500,res=300)
plot1=Table_NRMSE2 %>% 
  ggplot(aes(Species, NRMSE, col=Type)) +
  facet_wrap(~ Group, scales = "free_x") +
  geom_jitter(size=2, width = 0.15) +
  scale_y_log10() +
  scale_color_viridis(discrete=TRUE) 
print(plot1)
dev.off()

Table_AIC2$Species = factor(Table_AIC2$Species, levels=c(ListSp))
Table_AIC2$Group = factor(Table_AIC2$Group, levels=c("Group 1", "Group 2", "Group 3"))

png(filename=paste0(ModRF_directory, "AIC.png"), height=1500, width=1500,res=300)
plot2 = Table_AIC2 %>% 
  ggplot(aes(Species, AIC, col=Type)) +
  facet_wrap(~ Species, scales = "free") +
  geom_point(size=3) +
  scale_y_log10() +
  scale_color_viridis(discrete=TRUE) +
  theme_gray() +
  scale_x_discrete(breaks = NULL) +
  theme(axis.text.x = element_blank())
print(plot2)
dev.off()







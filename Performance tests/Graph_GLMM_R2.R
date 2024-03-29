
library(tidyverse)
library(viridis)


ModRF_directory="C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/"
Table_R2 = read_delim(paste0(ModRF_directory, "r2_capture_acoustic_500m.csv"))

png(filename=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/", "r2_capture_acoustic_500m.png"), 
    height=1500, width=2000,res=300)
plot2 = Table_R2 %>% 
  filter(Type == "Marginal R2") %>% 
  ggplot(aes(Species, R2, col=Threshold)) +
  facet_wrap(~ Species, scales = "free", nrow =2) +
  geom_point(size=3) +
  #scale_y_log10() +
  scale_color_viridis(discrete=TRUE) +
  theme_gray() +
  scale_x_discrete(breaks = NULL) +
  theme(axis.text.x = element_blank())
print(plot2)
dev.off()


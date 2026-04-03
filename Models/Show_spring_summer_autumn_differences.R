
library(raster)
library(tidyverse)
library(viridis)
#library(rasterVis)

Summer_TIF<-'/home/charlotte/Bureau/Thomas/images_test3/acticlass/VC90_all_acticlass_None_2026-02-25/RFspat_VC90_2026-02-25_noSpace_all_acticlass_None_Nyclei_france_met_summer.tif' 
Autumn_TIF<-'/home/charlotte/Bureau/Thomas/images_test3/acticlass/VC90_all_acticlass_None_2026-02-25/RFspat_VC90_2026-02-25_noSpace_all_acticlass_None_Nyclei_france_met_autumn.tif' 
Summer_raster=raster(Summer_TIF)
Autumn_raster=raster(Autumn_TIF)

diff_raster = Autumn_raster-Summer_raster
diff_raster_df = as.data.frame(diff_raster, xy = TRUE) %>% 
  drop_na(layer)

mid <- mean(diff_raster_df$layer, na.rm=T)

# MaxScale=quantile(subset(diff_raster_df$layer,diff_raster_df$layer>2),0.98)
# ScaleLimit=c(0, MaxScale)
# ggplot() +
#   geom_raster(data = Summer_raster_df , aes(x = x, y = y, fill = mean)) +
#   scale_fill_viridis_c(name = "Number of \nbat passes/night",
#                        limits = ScaleLimit, 
#                        #trans = "pseudo_log",
#                        oob = scales::squish,
#                        option = "A",
#                        na.value = alpha("lightgrey", 0)) 

ggplot()+
  geom_raster(data = diff_raster_df, aes(x=x, y=y, fill = layer)) +
  scale_fill_gradientn(colors = c("darkred", "red", "white", "blue", "darkblue"),
                       name = "Difference in activity",
                       values = scales::rescale(c(min(diff_raster_df$layer, na.rm=T),
                                                  mid-1,mid,mid+1, 
                                                  max(diff_raster_df$layer, na.rm=T))) ) +
  guides(scale = "none", alpha = "none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.subtitle = element_text(size=12)) +
  
  # labs(title = paste0(full_latin_name, "\n", "Difference between Summer and Autumn"),
  #      subtitle = "red = left from, blue = arrived to") +
  labs(title = paste0("Difference between Summer and Autumn"),
       subtitle = "red = left from, blue = arrived to") +
  ylab("") +
  xlab("")




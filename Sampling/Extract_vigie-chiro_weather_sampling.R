
library(tidyverse)
library(sf)

name = "rr"
nbr <- 0
path <- paste0("E:/SIG/Meteo_Lea_Eobs/", name, "_ens_mean_0.25deg_reg_v27.0e.nc")
results <- read_delim("C:/Users/croemer01/Documents/Donnees vigie-chiro/SpNuit2_DI_0_DataLP_PF_exportTot.csv") %>% 
  select(participation, Date = Nuit)
export <- read_delim("C:/Users/croemer01/Documents/Donnees vigie-chiro/p_export.csv") %>% 
  select(participation, idsite, point)
sites_localites <- read_delim("C:/Users/croemer01/Documents/Donnees vigie-chiro/sites_localites.csv", delim=";") %>% 
  filter(protocole == "POINT_FIXE") %>% 
  select(idsite=id_site, point = nom, Longitude = longitude, Latitude = latitude)

export_sites_localites = left_join(export, sites_localites)
results_export_sites_localites = left_join(results, export_sites_localites)

surveysData = results_export_sites_localites %>% 
  select(Date, Longitude, Latitude) %>% 
  distinct() %>% 
  drop_na() %>% 
  as.data.frame()
surveysData$Date=as.Date(surveysData$Date)

FRANCE= map_data("world", region = "France") %>% 
  st_as_sf(coords = c("long", "lat"), crs=4326, remove=FALSE)  %>%
  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") 

source("C:/Users/croemer01/Documents/R/bat-migration-europe/bat-migration-europe/Sampling/Extract_eobs.R")
surveysDataNew <- extract_eobs(path,surveysData,nbr,FRANCE)


write_csv(surveysDataNew, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Meteo/data/", name, "_extract.csv"))








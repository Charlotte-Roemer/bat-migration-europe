
library(tidyverse)

ListExports = list.files("/mnt/beegfs/ybas/VigieChiro/Raw", pattern = "^DataLP_PF_export_", full.names = T)
DataGIS=read_delim("/mnt/beegfs/croemer/VigieChiro/GI_FR_sites_localites.csv") %>%  #table with spatial variables (habitat and climate)
  select(X, Y, SpHO16S, SpHO17S)
ListSp = read_delim("/mnt/beegfs/croemer/VigieChiro/SpeciesList.csv") %>% 
  filter(Group=="bat")

print(ListSp$Esp)

ListRecorders = c("Anabat Swift", "Audiomoth", "Batlogger", "D500", "PassiveRecorder", "SM2BAT",
                  "SM2BAT+", "SM3", "SM4")

for (j in 1:length(ListSp$Esp)) {
  Sp = ListSp$Esp[j]
  print(Sp)

  rm(ExportTableTot)
for (i in 1:length(ListExports)){
  ExportTable = read_csv(ListExports[i], show_col_types = FALSE) %>% 
    filter(is.na(micro1_type), espece == Sp, detecteur_enregistreur_type %in% ListRecorders) %>% # only fix points
    select(espece, probabilite, idsite, nom, DateNuit, 
           detecteur_enregistreur_type, micro0_type,
           longitude, latitude)
  print(paste0("nrow ExportTable : ", nrow(ExportTable)))
  print(paste0(i, " ", ListExports[i]))
  if(exists("ExportTableTot")){
    ExportTableTot = rbind(ExportTable, ExportTableTot)
  }else{
    ExportTableTot = ExportTable 
  }
}

DataSaison = left_join(ExportTableTot, DataGIS, by=c("longitude"="X", "latitude"="Y"))

DataSaison$SpHO16_17S = DataSaison$SpHO16S + DataSaison$SpHO17S

DataSaisonSub = dplyr::select(DataSaison, probabilite, idsite, nom, micro0_type, DateNuit, longitude, latitude,
                              detecteur_enregistreur_type, SpHO16_17S)

write_csv(DataSaisonSub, paste0("/mnt/beegfs/croemer/VigieChiro/Landscape_IIC/", Sp, "_DataSaisonProbabilitePaysage_202309.csv"))

}
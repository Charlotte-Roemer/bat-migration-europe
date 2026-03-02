

library(glmmTMB)
library(beepr)
library(tidyverse) 
library(sf)
library(ordbetareg)
library(brms)

Sp_List = c("Minsch", "Barbar", "Nyclei", "Nycnoc", "Eptser", "Pipkuh", "Pipnat", 
            "Pippip", "Pippyg", "Rhifer")
variableName = "SpHO16_17S" # Percentage of coniferous + deciduous forests in 50m

Directory = "/home/charlotte/Documents/Donnees vigie-chiro/Influence variables sur Probas/" # where to put the results
MaterialCorresp = read_delim("/home/charlotte/Documents/Donnees vigie-chiro/Matos_correspondances.csv") # correspondance between names given by participants and reality
ListRecorders = c("Anabat Swift", "Audiomoth", "Batlogger", "D500", "PassiveRecorder", "SM2BAT", # list of recorders for fix points
                  "SM2BAT+", "SM3", "SM4")

for(i in 1:length(Sp_List)){
  Sp = Sp_List[i]
  print(Sp)
  
  DataSaisonSub = read_delim(paste0(Directory, Sp, "_DataSaisonProbabilitePaysage_202309.csv")) %>%  # table with probabilities for each sequence
    filter(detecteur_enregistreur_type %in% ListRecorders) %>% 
    filter(micro0_type != "Micro externe avec cornet") %>% 
    group_by(idsite, DateNuit) %>%
    sample_n(if(n() < 5) n() else 5) %>% # subsample to facilitate modelling
    ungroup()
  
  print(dim(DataSaisonSub))
  
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
  
  # # Plot material distribution
  # new_order <- with(subset(DataSaisonSub00, DataSaisonSub00$detecteur_micro != "SM2BAT Autre micro externe"), 
  #                   reorder(detecteur_micro , SpHO16_17S, median , na.rm=T))
  # DataSaisonSub00 %>% 
  #   filter(detecteur_micro != "SM2BAT Autre micro externe") %>% 
  #   ggplot(aes(x=new_order, y=SpHO16_17S)) +
  #   geom_violin(width = 3) +
  #   stat_summary(fun.y=median, geom="point", size=2, color="red") +
  #   theme(axis.text.x=element_text(angle=45,hjust=1, size = 15))
  
  # Keep only combinaisons that have a balanced sample of the forests, i.e., with a SD > 0.1 and median between 0.2 and 0.8
  # or keep only SM4BAT
  SD_table = DataSaisonSub00 %>% 
    drop_na(SpHO16_17S) %>% 
    group_by(detecteur_micro) %>% 
    summarise(Median = median(SpHO16_17S), SD = sd(SpHO16_17S))
  MaterialOK = SD_table$detecteur_micro[which(SD_table$Median>0.2 & SD_table$Median<0.8 & SD_table$SD > 0.1)]
  DataSaisonSub000 = DataSaisonSub00 %>% 
    #filter(detecteur_micro %in% MaterialOK)
    filter(detecteur_micro=="SM4 SMM-U2") %>% 
    drop_na(SpHO16_17S)
  
  # # To use a beta family, needs to transform 1 to 0.99 (allowed because very few cases)
  # DataSaisonSub2 = DataSaisonSub000
  # DataSaisonSub2$probabilite[which(DataSaisonSub2$probabilite==1)]=0.99
  # 
  # To use a ordbetareg it is better to have inflations exactly in 0 or in 1
  DataSaisonSub4 = DataSaisonSub000
  DataSaisonSub4$probabilite[which(DataSaisonSub4$probabilite>=0.99)]=1
  DataSaisonSub4$probabilite[which(DataSaisonSub4$probabilite<0.1)]=0
  
  Formula=as.formula(paste0("probabilite~", 
                            #variableName, "+I(", variableName, "^2)", 
                            variableName,
                            #"+detecteur_micro",
                            "+ (1|idsite/nom)"))
  
  ModBetaReg = ordbetareg(bf(Formula), 
                          data = DataSaisonSub4, 
                          iter = 2000,
                          cores=3, chains=3)
  
  # Save models
  saveRDS(ModBetaReg, paste0(Directory, "/", Sp, "_ModBetaReg.rds"))
  
  #ModBetaReg = readRDS(paste0(Directory, "/", Sp, "_ModBetaReg.rds"))
  
  png(filename=paste(Directory, Sp,"_BetaReg.png",sep=""), height=700, width=900,res=150)
  plot1 = pp_check(ModBetaReg)
  print(plot1)
  dev.off()
  
  Summary_Models = as.data.frame(fixef(ModBetaReg))
  Summary_Models$Type = rownames(Summary_Models)
  rownames(Summary_Models) = NULL
  
  write_csv(Summary_Models, paste0(Directory, Sp, "_Summary_Table.csv"))
  
  # Predict plot betareg
  ord_pred <- conditional_effects(ModBetaReg)[[1]]
  
  png(filename=paste(Directory, Sp,"_Forest_BetaReg.png",sep=""), height=700, width=900,res=150)
  plot2 = ord_pred %>% 
    ggplot(aes(y=estimate__,x=SpHO16_17S)) +
    geom_ribbon(aes(ymin=lower__,
                    ymax=upper__),fill="blue",
                alpha=0.5) +
    geom_hline(yintercept=1,linetype=2) +
    geom_line() +
    scale_y_continuous(labels=scales::percent_format(),
                       limits=c(0,1))+
    labs(y="Probability of ID",
         x="% Forest in 50 m")
  print(plot2)
  dev.off()
  
}

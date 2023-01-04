

library(tidyverse)
library(data.table)
library(sf)
library(spdep)
library(beepr)
library(randomForest)
library(hydroGOF)
library(glmmTMB)

# WARNING THIS WORKS FOR SPECIES GROUPS OF MAX 3 SPECIES #

Date_Mod = "2022-08-11"
ModRF_directory="C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/"
SpeciesGroupName = "Pip35" # Serotules, Pip35 or Pip50

if(SpeciesGroupName == "Serotules") {
  SpeciesGroup = c("Eptser", "Nyclei", "Nycnoc")
}else if(SpeciesGroupName == "Pip35"){
  SpeciesGroup = c("Pipkuh", "Pipnat")
}else if(SpeciesGroupName == "Pip50"){
  SpeciesGroup = c("Pippip", "Pippyg", "Minsch")
}

#### Load capture data ####
TableCapture_CoordSIG = fread("C:/Users/croemer01/Documents/Post-Doc/CACCHI/Data/Capture_CoordSIG_800m.csv")
TableCapture = fread("C:/Users/croemer01/Documents/Post-Doc/CACCHI/Data/Capture.csv")


# Species correspondance
TableCapture$JulianDay = yday(as.Date(TableCapture$DATE, format = "%d/%d/%Y"))
TableCapture$SpeciesGroup = ifelse(TableCapture$Sp==SpeciesGroup[1], "1", "0")
TableCapture$SpeciesGroup = ifelse(TableCapture$Sp==SpeciesGroup[2], "2", TableCapture$SpeciesGroup)
if(length(SpeciesGroup)>2){
  TableCapture$SpeciesGroup = ifelse(TableCapture$Sp==SpeciesGroup[3], "3", TableCapture$SpeciesGroup)
}

# Compute prop prediction by municipal corportion for capture
TableCapture_Commune_Day0 = TableCapture %>% 
  group_by(INSEE_COM, NOM_COM = COMMUNE, JulianDay) %>% 
  count(SpeciesGroup) %>% 
  mutate(Species_1 = sum(n[SpeciesGroup=="1"]),
         Species_2 = sum(n[SpeciesGroup=="2"]),
         Species_3 = sum(n[SpeciesGroup=="3"])) %>% 
  select(INSEE_COM, NOM_COM, JulianDay, Species_1, Species_2, Species_3) %>% 
  unique() 

if(length(SpeciesGroup)>2){
  TableCapture_Commune_Day = TableCapture_Commune_Day0 %>% 
    group_by(INSEE_COM, NOM_COM, JulianDay) %>% 
    summarise(Prop_1 = Species_1 / sum(Species_1, Species_2, Species_3),
              Prop_2 = Species_2 / sum(Species_1, Species_2, Species_3),
              Prop_3 = Species_3 / sum(Species_1, Species_2, Species_3))
}else{
  TableCapture_Commune_Day = TableCapture_Commune_Day0 %>% 
    group_by(INSEE_COM, NOM_COM, JulianDay) %>% 
    summarise(Prop_1 = Species_1 / sum(Species_1, Species_2),
              Prop_2 = Species_2 / sum(Species_1, Species_2))
}

#### Load models for all species of the group #### 
patternSp = paste(SpeciesGroup, collapse = "|")

ThresholdSort_possible = c("weighted", "0", "50", "90")

START=Sys.time()
TableAcousticPred_Commune_Day_FINAL = data.frame()
for (m in 1:length(ThresholdSort_possible)){
  
  ThresholdSort = ThresholdSort_possible[m]
  
  print(ThresholdSort)
  
  Directory_Model = paste0(ModRF_directory, "VC", ThresholdSort, "PG_",Date_Mod)
  
  ModRF.list <- list.files(Directory_Model, full.names = TRUE, pattern='*.learner', recursive = TRUE)
  ModRF.list.group = subset(ModRF.list, grepl(patternSp, ModRF.list))
  
  load(ModRF.list.group[1]) # Load random forest models
  ModRF_1 = ModRF
  load(ModRF.list.group[2])
  ModRF_2 = ModRF
  if(length(ModRF.list.group)>2){
    load(ModRF.list.group[3])
    ModRF_3 = ModRF
  }
  
  # Test for missing variables
  test=match(row.names(ModRF$importance),names(TableCapture_CoordSIG))
  MissingVar=subset(row.names(ModRF$importance),is.na(test))
  print("missing:")
  print(MissingVar)
  if(length(MissingVar)>0)
  {
    for (j in 1:length(MissingVar))
    {
      TableCapture_CoordSIG$temp=0
      names(TableCapture_CoordSIG)[ncol(TableCapture_CoordSIG)]=MissingVar[j]
    }
  }
  TableCapture_CoordSIG[is.na(TableCapture_CoordSIG)]=0
  
  # Make predictions
  pred_1<-predict(ModRF_1,newdata=TableCapture_CoordSIG, type="response")
  pred_2<-predict(ModRF_2,newdata=TableCapture_CoordSIG, type="response")
  if(length(ModRF.list.group)>2){
    pred_3<-predict(ModRF_3,newdata=TableCapture_CoordSIG, type="response")
  }
  
  # Bind
  if(length(ModRF.list.group)>2){
    TableAcousticPred = data.frame(pred_1, pred_2, pred_3, TableCapture_CoordSIG)
  }else{
    TableAcousticPred = data.frame(pred_1, pred_2, TableCapture_CoordSIG)
  }
  
  # Back-transform predictions
  TableAcousticPred2=TableAcousticPred
  TableAcousticPred2[,1:length(ModRF.list.group)] = apply(TableAcousticPred[,1:length(ModRF.list.group)], 2, function(x) (10^(x)-1))
  
  # Julian Day
  TableAcousticPred2$JulianDay = yday(as.Date(TableAcousticPred2$DATE, format = "%d/%d/%Y"))
  
  # # Save predict !!! TOO LARGE DATASET !!!
  # SavePred = TableAcousticPred2
  # names(SavePred) = c(SpeciesGroup, names(TableCapture_CoordSIG), "JulianDay")
  # write_csv(SavePred, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Predicts_acoustic_for_comparison_capture/",
  #                            Date_Mod, "_predict_", ThresholdSort, "_", SpeciesGroupName, ".csv"))
  
  # Compute prop prediction by municipal corportion for acoustics
  if(length(ModRF.list.group)>2){
    TableAcousticPred_Commune_Day0 = TableAcousticPred2 %>% 
      group_by(INSEE_COM, NOM_COM, JulianDay) %>% 
      summarise(Mean_pred_1 = mean(pred_1),
                Mean_pred_2 = mean(pred_2),
                Mean_pred_3 = mean(pred_3)) 
    
    TableAcousticPred_Commune_Day = TableAcousticPred_Commune_Day0 %>% 
      group_by(INSEE_COM, NOM_COM, JulianDay) %>% 
      summarise(Prop_pred_1 = Mean_pred_1 / sum(Mean_pred_1, Mean_pred_2, Mean_pred_3),
                Prop_pred_2 = Mean_pred_2 / sum(Mean_pred_1, Mean_pred_2, Mean_pred_3),
                Prop_pred_3 = Mean_pred_3 / sum(Mean_pred_1, Mean_pred_2, Mean_pred_3))
  }else{
    TableAcousticPred_Commune_Day0 = TableAcousticPred2 %>% 
      group_by(INSEE_COM, NOM_COM, JulianDay) %>% 
      summarise(Mean_pred_1 = mean(pred_1),
                Mean_pred_2 = mean(pred_2)) 
    
    TableAcousticPred_Commune_Day = TableAcousticPred_Commune_Day0 %>% 
      group_by(INSEE_COM, NOM_COM, JulianDay) %>% 
      summarise(Prop_pred_1 = Mean_pred_1 / sum(Mean_pred_1, Mean_pred_2),
                Prop_pred_2 = Mean_pred_2 / sum(Mean_pred_1, Mean_pred_2))
  }
  
  # Compare acoustic predictions with capture data
  Comparison = left_join(TableCapture_Commune_Day, TableAcousticPred_Commune_Day, by=c("INSEE_COM", "JulianDay"))
  Comparison2 = Comparison[!is.nan(Comparison$Prop_1),]
  
  NRMSE_1 = nrmse(obs=Comparison2$Prop_1, sim=Comparison2$Prop_pred_1, norm = "maxmin")
  NRMSE_2 = nrmse(obs=Comparison2$Prop_2, sim=Comparison2$Prop_pred_2, norm = "maxmin")
  if(length(ModRF.list.group)>2){
    NRMSE_3 = nrmse(obs=Comparison2$Prop_3, sim=Comparison2$Prop_pred_3, norm = "maxmin")
  }
  
  if(length(ModRF.list.group)>2){
    if(ThresholdSort == "weighted"){
      NRMSE_weighted = data.frame(Species_1 = NRMSE_1, Species_2 = NRMSE_2, Species_3 = NRMSE_3, type = ThresholdSort)
    }else{
      if(ThresholdSort == "0"){
        NRMSE_0 = data.frame(Species_1 = NRMSE_1, Species_2 = NRMSE_2, Species_3 = NRMSE_3, type = ThresholdSort)
      }else{
        if(ThresholdSort == "50"){
          NRMSE_50 = data.frame(Species_1 = NRMSE_1, Species_2 = NRMSE_2, Species_3 = NRMSE_3, type = ThresholdSort)
        }else{
          if(ThresholdSort == "90"){
            NRMSE_90 = data.frame(Species_1 = NRMSE_1, Species_2 = NRMSE_2, Species_3 = NRMSE_3, type = ThresholdSort)
          }}}}
  }else{
    if(ThresholdSort == "weighted"){
      NRMSE_weighted = data.frame(Species_1 = NRMSE_1, Species_2 = NRMSE_2, type = ThresholdSort)
    }else{
      if(ThresholdSort == "0"){
        NRMSE_0 = data.frame(Species_1 = NRMSE_1, Species_2 = NRMSE_2, type = ThresholdSort)
      }else{
        if(ThresholdSort == "50"){
          NRMSE_50 = data.frame(Species_1 = NRMSE_1, Species_2 = NRMSE_2, type = ThresholdSort)
        }else{
          if(ThresholdSort == "90"){
            NRMSE_90 = data.frame(Species_1 = NRMSE_1, Species_2 = NRMSE_2, type = ThresholdSort)
          }}}}
  }
  
  TableAcousticPred_Commune_Day_Temp = TableAcousticPred_Commune_Day %>% 
    mutate(Type = ThresholdSort)
  
  TableAcousticPred_Commune_Day_FINAL = rbind(TableAcousticPred_Commune_Day_FINAL, 
                                              TableAcousticPred_Commune_Day_Temp)
  
}

rm(TableAcousticPred)
rm(TableAcousticPred2)

# Create GLMM
TableCapture_Success = TableCapture %>% 
  group_by(INSEE_COM, NOM_COM = COMMUNE, JulianDay, SpeciesGroup, Sp) %>% 
  mutate(Species_1 = ifelse(SpeciesGroup=="1", 1, 0),
         Species_2 = ifelse(SpeciesGroup=="2", 1, 0),
         Species_3 = ifelse(SpeciesGroup=="3", 1, 0)) %>% 
  filter(SpeciesGroup!="0") %>% # remove all individuals that are not in the SpeciesGroup
  select(INSEE_COM, NOM_COM, JulianDay, Species_1, Species_2, Species_3, Sp, SpeciesGroup) %>% 
  as.data.frame()

Comparison_success0 = left_join(TableCapture_Success, TableAcousticPred_Commune_Day_FINAL, by=c("INSEE_COM", "JulianDay"))

Table_AIC_FINAL = data.frame()
for (n in 1:length(ThresholdSort_possible)){
  ThresholdSort = ThresholdSort_possible[n]
  print(ThresholdSort)
  
  Comparison_success = Comparison_success0 %>% 
    filter(Type == ThresholdSort)
  
  Mod0_Species_1 = glmmTMB(cbind(Species_1 ,(1-Species_1 )) ~ Prop_pred_1 + (1|INSEE_COM),
                           data = Comparison_success,
                           family = binomial)
  Mod0_Species_2 = glmmTMB(cbind(Species_2 ,(1-Species_2 )) ~ Prop_pred_2 + (1|INSEE_COM),
                           data = Comparison_success,
                           family = binomial)
  if(length(SpeciesGroup)>2){
    Mod0_Species_3 = glmmTMB(cbind(Species_3 ,(1-Species_3 )) ~ Prop_pred_3 + (1|INSEE_COM),
                             data = Comparison_success,
                             family = binomial)
    
    Table_AIC_Temp = data.frame("AIC_1" = AIC(Mod0_Species_1), 
                                "AIC_2" = AIC(Mod0_Species_2),
                                "AIC_3" = AIC(Mod0_Species_3), 
                                "Type" = ThresholdSort)
  }else{
    Table_AIC_Temp = data.frame("AIC_1" = AIC(Mod0_Species_1), 
                                "AIC_2" = AIC(Mod0_Species_2),
                                "Type" = ThresholdSort)
  }
  
  Table_AIC_FINAL = rbind(Table_AIC_FINAL, Table_AIC_Temp)
}

colnames(Table_AIC_FINAL) = c(SpeciesGroup, "Type")

Table_NRMSE = rbind(NRMSE_weighted, NRMSE_0, NRMSE_50, NRMSE_90)
colnames(Table_NRMSE) = c(SpeciesGroup, "Type")

fwrite(Table_NRMSE,paste0(ModRF_directory, SpeciesGroupName, "_NRMSE_capture_acoustic_800m.csv"))
fwrite(Table_AIC_FINAL,paste0(ModRF_directory, SpeciesGroupName, "_AIC_capture_acoustic_800m.csv"))

END=Sys.time()
END-START

beep(2)

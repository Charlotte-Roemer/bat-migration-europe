# a) # transformer Species en proportion sur la nuit 
#       --> construite de la meme façon à la variable acoustique
# b) effet random (vérifier) :
#   - (1|INSEE_COM/Date) --> mais est-ce qu'on a au moins 50% des communes qui possèdent plusieurs dates ?
# - ou (1|INSEE_COM) + (1|Date) mais pas top
# --> comparer les AIC pour choisir la meilleure formule

library(tidyverse)
library(data.table)
library(sf)
library(spdep)
library(beepr)
library(randomForest)
library(hydroGOF)
library(glmmTMB)
library(DHARMa)
library(performance)

# WARNING THIS WORKS FOR SPECIES GROUPS OF MAX 3 SPECIES #

Date_Mod = "2023-11-17"
ModRF_directory="C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/"
SpeciesGroupName = "Pip50" # Serotules, Pip35, Pip50 or Control

if(SpeciesGroupName == "Serotules") {
  SpeciesGroup = c("Eptser", "Nyclei", "Nycnoc")
}else if(SpeciesGroupName == "Pip35"){
  SpeciesGroup = c("Pipkuh", "Pipnat")
}else if(SpeciesGroupName == "Pip50"){
  SpeciesGroup = c("Minsch", "Pippip", "Pippyg")
}else if(SpeciesGroupName == "Control"){
  SpeciesGroup = c("Barbar", "Rhifer")
}

#### Load capture data ####
#TableCapture_CoordSIG = fread("C:/Users/croemer01/Documents/Post-Doc/CACCHI/Data/Capture_CoordSIG_800m.csv")
TableCapture_CoordSIG = fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/Capture_CoordSIG_500m.csv")
TableCapture0 = fread("C:/Users/croemer01/Documents/Post-Doc/CACCHI/Data/Capture.csv")

# Remove all data before 2014 because no acoustic data to compare
TableCapture = TableCapture0 %>% 
  filter(ANNEE >2013)

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
  filter(SpeciesGroup>0) %>% 
  mutate(Species_1 = sum(n[SpeciesGroup=="1"]),
         Species_2 = sum(n[SpeciesGroup=="2"]),
         Species_3 = sum(n[SpeciesGroup=="3"])) %>% 
  select(INSEE_COM, NOM_COM, JulianDay, Species_1, Species_2, Species_3) 

if(length(SpeciesGroup)>2){
  TableCapture_Commune_Day = TableCapture_Commune_Day0 %>% 
    group_by(INSEE_COM, NOM_COM, JulianDay) %>% 
    reframe(Prop_1 = Species_1 / sum(Species_1, Species_2, Species_3),
            Prop_2 = Species_2 / sum(Species_1, Species_2, Species_3),
            Prop_3 = Species_3 / sum(Species_1, Species_2, Species_3))
}else{
  TableCapture_Commune_Day = TableCapture_Commune_Day0 %>% 
    group_by(INSEE_COM, NOM_COM, JulianDay) %>% 
    reframe(Prop_1 = Species_1 / sum(Species_1, Species_2),
            Prop_2 = Species_2 / sum(Species_1, Species_2))
}

#### Load models for all species of the group #### 
patternSp = paste(SpeciesGroup, collapse = "|")

ThresholdSort_possible = c("weighted", "0", "50", "90")

START=Sys.time()
TableAcousticPred_Commune_Day_FINAL = data.frame()
for (m in 1:length(ThresholdSort_possible))
  {
  
  ThresholdSort = ThresholdSort_possible[m]
  
  print(ThresholdSort)
  
  Directory_Model = paste0(ModRF_directory, "VC", ThresholdSort, "PG_",Date_Mod)
  
  ModRF.list <- list.files(Directory_Model, full.names = TRUE, pattern='*.learner', recursive = TRUE)
  ModRF.list.group = subset(ModRF.list, grepl(patternSp, ModRF.list) & !grepl ("Boruta", ModRF.list))
  
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
  
  # Compute prop prediction by municipal corportion for acoustics
  if(length(ModRF.list.group)>2){
    TableAcousticPred_Commune_Day0 = TableAcousticPred2 %>% 
      group_by(INSEE_COM, NOM_COM, DATE, JulianDay) %>% 
      summarise(Mean_pred_1 = mean(pred_1),
                Mean_pred_2 = mean(pred_2),
                Mean_pred_3 = mean(pred_3)) 
    
    TableAcousticPred_Commune_Day = TableAcousticPred_Commune_Day0 %>% 
      group_by(INSEE_COM, NOM_COM, DATE, JulianDay) %>% 
      summarise(Prop_pred_1 = Mean_pred_1 / sum(Mean_pred_1, Mean_pred_2, Mean_pred_3),
                Prop_pred_2 = Mean_pred_2 / sum(Mean_pred_1, Mean_pred_2, Mean_pred_3),
                Prop_pred_3 = Mean_pred_3 / sum(Mean_pred_1, Mean_pred_2, Mean_pred_3))
  }else{
    TableAcousticPred_Commune_Day0 = TableAcousticPred2 %>% 
      group_by(INSEE_COM, NOM_COM, DATE, JulianDay) %>% 
      summarise(Mean_pred_1 = mean(pred_1),
                Mean_pred_2 = mean(pred_2)) 
    
    TableAcousticPred_Commune_Day = TableAcousticPred_Commune_Day0 %>% 
      group_by(INSEE_COM, NOM_COM, DATE, JulianDay) %>% 
      summarise(Prop_pred_1 = Mean_pred_1 / sum(Mean_pred_1, Mean_pred_2),
                Prop_pred_2 = Mean_pred_2 / sum(Mean_pred_1, Mean_pred_2))
  }
  
  TableAcousticPred_Commune_Day_Temp = TableAcousticPred_Commune_Day %>% 
    mutate(Type = ThresholdSort)
  
  TableAcousticPred_Commune_Day_FINAL = rbind(TableAcousticPred_Commune_Day_FINAL, 
                                              TableAcousticPred_Commune_Day_Temp)
  
}

rm(TableAcousticPred)
rm(TableAcousticPred2)

# Create GLMM
TableCapture_Commune_Day0 = TableCapture %>% 
  group_by(INSEE_COM, NOM_COM = COMMUNE, DATE) %>% 
  count(SpeciesGroup) %>% 
  filter(SpeciesGroup>0) %>% 
  mutate(Species_1 = sum(n[SpeciesGroup=="1"]),
         Species_2 = sum(n[SpeciesGroup=="2"]),
         Species_3 = sum(n[SpeciesGroup=="3"])) %>% 
  select(INSEE_COM, NOM_COM, DATE, Species_1, Species_2, Species_3)

if(length(SpeciesGroup)>2){
  TableCapture_Success = TableCapture_Commune_Day0 %>% 
    group_by(INSEE_COM, NOM_COM, DATE) %>% 
    reframe(Prop_1 = Species_1 / sum(Species_1, Species_2, Species_3),
            Prop_2 = Species_2 / sum(Species_1, Species_2, Species_3),
            Prop_3 = Species_3 / sum(Species_1, Species_2, Species_3))
}else{
  TableCapture_Success = TableCapture_Commune_Day0 %>% 
    group_by(INSEE_COM, NOM_COM, DATE) %>% 
    reframe(Prop_1 = Species_1 / sum(Species_1, Species_2),
            Prop_2 = Species_2 / sum(Species_1, Species_2))
}

Comparison_success0 = left_join(TableCapture_Success, TableAcousticPred_Commune_Day_FINAL, by=c("INSEE_COM", "DATE"))

Summary_Models_FINAL = data.frame()
for (n in 1:length(ThresholdSort_possible)){
  ThresholdSort = ThresholdSort_possible[n]
  print(ThresholdSort)
  
  Comparison_success = Comparison_success0 %>% 
    filter(Type == ThresholdSort)
  
  #Because apparently random forest models sometimes predict negative values (but very close to 0)
  Comparison_success$Prop_pred_1 = ifelse(Comparison_success$Prop_pred_1<0, abs(Comparison_success$Prop_pred_1), Comparison_success$Prop_pred_1)
  Comparison_success$Prop_pred_2 = ifelse(Comparison_success$Prop_pred_2<0, abs(Comparison_success$Prop_pred_2), Comparison_success$Prop_pred_2)
  
  # library(bestNormalize)
  # bestNormalize(Comparison_success$Prop_pred_2)
  # orderNorm (Comparison_success$Prop_pred_2)$x.t
  
  Mod0_Species_1 = glmmTMB(Prop_pred_1 ~ Prop_1  + (1|INSEE_COM) + (1|DATE), 
                           data = Comparison_success,
                           family = beta_family()) 
  Mod0_Species_2 = glmmTMB(Prop_pred_2 ~ Prop_2 + (1|INSEE_COM) + (1|DATE),
                           data = Comparison_success,
                           family = gaussian()) 
  
  simulationOutput_1 <- simulateResiduals(fittedModel = Mod0_Species_1, plot = F)
  simulationOutput_2 <- simulateResiduals(fittedModel = Mod0_Species_2, plot = F)
  png(filename=paste(ModRF_directory, SpeciesGroup[1], "_", ThresholdSort, "_Residuals_diagnostic.png",sep=""), height=700, width=1500,res=150)
  plot(simulationOutput_1)
  dev.off()
  png(filename=paste(ModRF_directory, SpeciesGroup[2], "_", ThresholdSort,"_Residuals_diagnostic.png",sep=""), height=700, width=1500,res=150)
  plot(simulationOutput_2)
  dev.off()
  
  if(length(SpeciesGroup)>2){
    #Because apparently random forest models sometimes predict negative values (but very close to 0)
    Comparison_success$Prop_pred_3 = ifelse(Comparison_success$Prop_pred_3<0, abs(Comparison_success$Prop_pred_3), Comparison_success$Prop_pred_3)
    
    Mod0_Species_3 = glmmTMB(Prop_pred_3 ~ Prop_3 + (1|INSEE_COM) + (1|JulianDay),
                             data = Comparison_success,
                             family = beta_family())
    
    simulationOutput_3 <- simulateResiduals(fittedModel = Mod0_Species_3, plot = F)
    png(filename=paste(ModRF_directory, SpeciesGroup[3], "_", ThresholdSort, "_Residuals_diagnostic.png",sep=""), height=700, width=1500,res=150)
    plot(simulationOutput_3)
    dev.off()
    
    # Build summary of all models
    FIXEF1 <- as.data.frame(coef(summary(Mod0_Species_1))$cond)
    FIXEF2 <- as.data.frame(coef(summary(Mod0_Species_2))$cond)
    FIXEF3 <- as.data.frame(coef(summary(Mod0_Species_3))$cond)
    
    Summary_Models = data.frame("Threshold" = rep(ThresholdSort, 6),
                                "Species" = c(rep(SpeciesGroup[1],2), 
                                            rep(SpeciesGroup[2],2), 
                                            rep(SpeciesGroup[3],2)),
                                "Type" = c(rep(c("Conditional R2", "Marginal R2"),3)),
                                "R2" = c(r2(Mod0_Species_1)$R2_conditional[[1]], 
                                         r2(Mod0_Species_1)$R2_marginal[[1]], 
                                         r2(Mod0_Species_2)$R2_conditional[[1]], 
                                         r2(Mod0_Species_2)$R2_marginal[[1]], 
                                         r2(Mod0_Species_3)$R2_conditional[[1]],
                                         r2(Mod0_Species_3)$R2_marginal[[1]]),
                                "Estimate Forest" = c(rep(FIXEF1$Estimate[grepl("Prop", rownames(FIXEF1))], 2),
                                                      rep(FIXEF2$Estimate[grepl("Prop", rownames(FIXEF1))], 2),
                                                      rep(FIXEF3$Estimate[grepl("Prop", rownames(FIXEF1))], 2)), # inverse of estimate
                                "Std. Error Forest" = c(rep(FIXEF1$'Std. Error'[grepl("Prop", rownames(FIXEF1))], 2),
                                                        rep(FIXEF2$'Std. Error'[grepl("Prop", rownames(FIXEF1))], 2),
                                                        rep(FIXEF3$'Std. Error'[grepl("Prop", rownames(FIXEF1))], 2)),
                                "p-value Forest" = c(rep(FIXEF1$'Pr(>|z|)'[grepl("Prop", rownames(FIXEF1))], 2),
                                                     rep(FIXEF2$'Pr(>|z|)'[grepl("Prop", rownames(FIXEF1))], 2),
                                                     rep(FIXEF3$'Pr(>|z|)'[grepl("Prop", rownames(FIXEF1))], 2))
    )
    
  }else{
    # Build summary of all models
    FIXEF1 <- as.data.frame(coef(summary(Mod0_Species_1))$cond)
    FIXEF2 <- as.data.frame(coef(summary(Mod0_Species_2))$cond)
    
    Summary_Models = data.frame("Threshold" = rep(ThresholdSort, 4),
                                "Species" = c(rep(SpeciesGroup[1],2), 
                                              rep(SpeciesGroup[2],2)),
                                "Type" = c(rep(c("Conditional R2", "Marginal R2"),2)),
                                "R2" = c(r2(Mod0_Species_1)$R2_conditional[[1]], 
                                         r2(Mod0_Species_1)$R2_marginal[[1]], 
                                         r2(Mod0_Species_2)$R2_conditional[[1]], 
                                         r2(Mod0_Species_2)$R2_marginal[[1]]),
                                "Estimate Forest" = c(rep(FIXEF1$Estimate[grepl("Prop", rownames(FIXEF1))], 2),
                                                      rep(FIXEF2$Estimate[grepl("Prop", rownames(FIXEF1))], 2)),
                                "Std. Error Forest" = c(rep(FIXEF1$'Std. Error'[grepl("Prop", rownames(FIXEF1))], 2),
                                                        rep(FIXEF2$'Std. Error'[grepl("Prop", rownames(FIXEF1))], 2)),
                                "p-value Forest" = c(rep(FIXEF1$'Pr(>|z|)'[grepl("Prop", rownames(FIXEF1))], 2),
                                                     rep(FIXEF2$'Pr(>|z|)'[grepl("Prop", rownames(FIXEF1))], 2))
    )
    
  }
  
  Summary_Models_FINAL = rbind(Summary_Models_FINAL, Summary_Models)
}

fwrite(Summary_Models_FINAL,paste0(ModRF_directory, SpeciesGroupName, "_r2_capture_acoustic_500m.csv"))

END=Sys.time()
END-START

beep(2)

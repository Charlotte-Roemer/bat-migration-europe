# Et essayer de rajouter des données de capture supplémentaires
# PROBLEME : dates inférieures à 2014 !!!!!!! --> changer script qui crée TableCapture_CoordSIG

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
library(bestNormalize)

Date_Mod = "2023-11-17"
ModRF_directory="C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/"
SpeciesList <- "C:/Users/croemer01/Documents/Donnees vigie-chiro/SpeciesList.csv" # species list
TableCapture_CoordSIG = fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/Capture_CoordSIG_500m.csv")
TableDetection = fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/Detection_Barataud.csv")

# Remove all capture data before 2014 because no acoustic data to compare
# Remove declared roosts
TableCapture_CoordSIG = TableCapture_CoordSIG %>%
  filter(SpYear >2013) %>%
  filter(SpGite ==0 | is.na(SpGite)) # 1 = roost, NA = no declaration

#Load species list
sp_list <- fread(SpeciesList)

#### Load models for all species of the group #### 
ThresholdSort_possible = c("weighted", "0", "50", "90")

START=Sys.time()
TableAcousticPred_Commune_Day_FINAL = data.frame()
# for (m in 1:length(ThresholdSort_possible))
m=1
{
  ThresholdSort = ThresholdSort_possible[m]
  print(ThresholdSort)
  
  Directory_Model = paste0(ModRF_directory, "VC", ThresholdSort, "PG_",Date_Mod)
  ModRF.list <- list.files(Directory_Model, full.names = TRUE, pattern='*.learner', recursive = TRUE)
  
  TableAcousticPred = data.frame()
  for (k in 1:length(ModRF.list)){
    Sp = sp_list$Esp[which(str_detect(ModRF.list[k], sp_list$Esp))]
    print(Sp)
    
    load(subset(ModRF.list[k], grepl(Sp, ModRF.list[k])))
    
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
    pred_1<-predict(ModRF,newdata=TableCapture_CoordSIG, type="response")
    pred = data.frame(pred_1)
    # Back-transform predictions
    pred$pred_1 = 10^(pred$pred_1)-1
    # # Divide by Detection coefficient
    pred$NewAct = pred$pred_1/TableDetection$Ddet[which(TableDetection$Species==Sp)]
    names(pred) = c(Sp, paste0(Sp, "_cor"))
    
    # Bind
    if(nrow(TableAcousticPred)==0){
      TableAcousticPred = cbind(TableCapture_CoordSIG, pred)
    }else{
      TableAcousticPred = cbind(TableAcousticPred, pred)
    }
  }
  
  TableAcousticPred2 = TableAcousticPred
  
  List_pred_cor = names(TableAcousticPred2)[which(str_detect(names(TableAcousticPred2), "_cor"))]
  
  for (i in 1:length(List_pred_cor)){
    Sp = sp_list$Esp[which(str_detect(List_pred_cor[i], sp_list$Esp))]
    print(Sp)
    
    test2 = which(names(TableAcousticPred2)==List_pred_cor[i])
    test = which(names(TableAcousticPred2) %in% List_pred_cor)
    Prop = TableAcousticPred2[, ..test2]/
      rowSums(TableAcousticPred2[, ..test])
    
    TableAcousticPred2 = cbind(TableAcousticPred2, Prop)
    names(TableAcousticPred2)=c(names(TableAcousticPred2[,1:(ncol(TableAcousticPred2)-1)]), paste0(Sp, "_prop"))
    
  }
  
  TableAcousticPred_Commune_Day_Temp = TableAcousticPred2 %>% 
    mutate(Type = ThresholdSort)
  
  TableAcousticPred_Commune_Day_FINAL = rbind(TableAcousticPred_Commune_Day_FINAL, 
                                              TableAcousticPred_Commune_Day_Temp)
  
}

# rm(TableAcousticPred)
# rm(TableAcousticPred2)

# Calculate mean values for municipality*date
TableAcousticPred_Commune_Day_FINAL2 = TableAcousticPred_Commune_Day_FINAL %>% 
  select(ends_with("_cor"), DATE, INSEE_COM, Type) %>% 
  group_by (DATE, INSEE_COM, Type) %>% 
  summarise(across(everything(), mean))

#### Load capture data ####
TableCapture0 = fread("C:/Users/croemer01/Documents/Post-Doc/CACCHI/Data/Capture.csv")
TableCapture = TableCapture0

# List of species for which we predict activity data
# to make sure that we compare proportion in the same set of species (for acoustic or capture)
ind = which(outer(sp_list$Esp, ModRF.list, Vectorize(grepl)), arr.ind = T)
List_Sp = sp_list$Esp[ind[,1]]

# Prepare table for GLMM
TableCapture_Commune_Day0 = TableCapture %>% 
  group_by(INSEE_COM, NOM_COM = COMMUNE, DATE) %>% 
  count(Sp) %>% 
  filter(Sp %in% List_Sp) %>% 
  pivot_wider(names_from = Sp, values_from = n, values_fill=0)

# Calculate proportion of captured bats
for (i in 1:length(List_Sp)){
  Sp = List_Sp[i]
  print(Sp)
  
  test2 = which(names(TableCapture_Commune_Day0)==List_Sp[i])
  test = which(names(TableCapture_Commune_Day0) %in% List_Sp)
  Prop = TableCapture_Commune_Day0[, test2]/
    rowSums(TableCapture_Commune_Day0[, test])
  names(Prop)=paste0(Sp, "_prop_capture")
  
  TableCapture_Commune_Day0 = cbind(TableCapture_Commune_Day0, Prop)
}

# Add suffix Capture
names(TableCapture_Commune_Day0)[which(names(TableCapture_Commune_Day0) %in% List_Sp)] = paste0(names(TableCapture_Commune_Day0[List_Sp]) , 
                                                                                                "_capture")
Comparison_success0 = TableAcousticPred_Commune_Day_FINAL2 %>% 
  #select(-NOM_COM) %>% 
  left_join(TableCapture_Commune_Day0, by=c("INSEE_COM", "DATE")) #%>% 
  #drop_na(NOM_COM) # these are the dates of capture without any bat from the List_Sp (e.g. only small myotis)

# Transform capture numbers to presence/absence
f = function(x) {
  ifelse(x > 0, 1, 0)
} 

Comparison_success0 = Comparison_success0 %>% 
  mutate_at(vars(matches(paste0("^", List_Sp, "_capture$"))), list(PresAbs = f))

# transform date variable to class date
Comparison_success0$DATE = as.Date(Comparison_success0$DATE, format = "%d/%m/%Y")

# Remove all data before 2014 because no acoustic data to compare
Comparison_success0 = Comparison_success0 %>%
  filter(as.Date(DATE, format = "%d/%m/%Y")  > as.Date("2014-01-01"))

# Summary_Models_FINAL = data.frame()
# for (n in 1:length(ThresholdSort_possible)){
n=1
ThresholdSort = ThresholdSort_possible[n]
print(ThresholdSort)

Comparison_success = Comparison_success0 %>% 
  filter(Type == ThresholdSort)

# Summary_Models_Sp = data.frame()
# for (i in 1:length(List_Sp)){
i=4
print(List_Sp[i])

# Normalise response
testx = which(names(Comparison_success)==List_Sp[i])
Comparison_success$Response_normal = orderNorm(as.numeric(Comparison_success[, ..testx][[1]]))$x.t
Comparison_success$JJulien = yday(Comparison_success$DATE)

## Build different response variables to test them :

# focus species capture proportion
# testR = grep(paste0("^", List_Sp[i],
#                     "_prop", 
#                     "$"), names(Comparison_success))
# Response = names(Comparison_success[, ..testR]) 

# sum of all species activity without the focus species
testR2 = grep(paste0("_cor", "$"), names(Comparison_success))
testR2b = grep(paste0("^", List_Sp[i], 
                      "_cor", 
                      "$"), names(Comparison_success))
testR2 = testR2[which(testR2 != testR2b)]
Comparison_success$TOT_act_cor=NULL
Comparison_success$TOT_act_cor = apply(Comparison_success[, testR2], 1, FUN = sum) 

# focus species activity
testR3 = grep(paste0("^", List_Sp[i], "_cor", "$"), names(Comparison_success))
Response2 = names(Comparison_success[, testR3]) 

## Build different fix effects to test them
testFE = grep(paste0("^", List_Sp[i], "_prop_capture", "$"), names(Comparison_success)) # proportion of capture of the species among all others
#testFE = grep(paste0("^", List_Sp[i], "_capture_PresAbs", "$"), names(Comparison_success))
FixEffect = names(Comparison_success[, testFE])

## Different formulae to test them
# Formula1=as.formula(paste0("Response_normal~",
#                            FixEffect,
#                            "+ (1|INSEE_COM) + (1|DATE)"))
# Formula2=as.formula(paste0("as.integer(", Response, ")", "~",
#                            FixEffect,
#                            "+ (1|INSEE_COM) + (1|DATE)"))
# Formula3=as.formula(paste0("Response_normal~",
#                            FixEffect, "+ poly(JJulien, 2)",
#                            "+ (1|INSEE_COM) + (1|SpYear)"))
# Formula4=as.formula(paste0(Response, "~",
#                            FixEffect, "+ poly(JJulien, 2)",
#                            "+ (1|INSEE_COM) + (1|SpYear)"))
# Formula5=as.formula(paste0("cbind(", Response2, ", TOT_act_cor)", "~",
#                            FixEffect, "+ poly(JJulien, 2)",
#                            "+ (1|INSEE_COM) + (1|SpYear)"))
# Formula6=as.formula(paste0("cbind(", Response2, ", TOT_act_cor)", "~",
#                            FixEffect, "+ poly(JJulien, 3)",
#                            "+ (1|INSEE_COM) + (1|SpYear)"))
Formula7=as.formula(paste0("cbind(", Response2, ", TOT_act_cor)", "~",
                           FixEffect, "+ poly(JJulien, 2)",
                           "+ (1|INSEE_COM)"))

## Different models
# Mod_Species_1 = glmmTMB(Formula1,
#                         data = Comparison_success,
#                         family = gaussian())
# Mod_Species_2 = glmmTMB(Formula2, 
#                         data = Comparison_success,
#                         family = nbinom2()) 
# Mod_Species_3 = glmmTMB(Formula3,
#                         data = Comparison_success,
#                         family = gaussian())
# Mod_Species_4 = glmmTMB(Formula4,
#                         data = Comparison_success,
#                         family = nbinom2())
# Mod_Species_5 = glmmTMB(Formula5,
#                         data = Comparison_success,
#                         family = binomial())
# Mod_Species_6 = glmmTMB(Formula6,
#                         data = Comparison_success,
#                         family = binomial())
Mod_Species_7 = glmmTMB(Formula7,
                        data = Comparison_success,
                        family = binomial())

# simulationOutput_1 <- simulateResiduals(fittedModel = Mod_Species_1, plot = F)
# simulationOutput_2 <- simulateResiduals(fittedModel = Mod_Species_2, plot = F)
#simulationOutput_3 <- simulateResiduals(fittedModel = Mod_Species_3, plot = F)
simulationOutput_7 <- simulateResiduals(fittedModel = Mod_Species_7, plot = F)

# png(filename=paste(ModRF_directory, List_Sp[i], "_", ThresholdSort, "_Residuals_diagnostic_gaussian.png",sep=""), height=700, width=1500,res=150)
# plot(simulationOutput_1)
# dev.off()

# png(filename=paste(ModRF_directory, List_Sp[i], "_", ThresholdSort, "_Residuals_diagnostic_nbinom2.png",sep=""), height=700, width=1500,res=150)
# plot(simulationOutput_2)
# dev.off()

# png(filename=paste(ModRF_directory, List_Sp[i], "_", ThresholdSort, "_Residuals_diagnostic_gaussian_JJulien.png",sep=""), height=700, width=1500,res=150)
# plot(simulationOutput_3)
# dev.off()

# png(filename=paste(ModRF_directory, List_Sp[i], "_", ThresholdSort, "_Residuals_diagnostic_nbinom_JJulien.png",sep=""), height=700, width=1500,res=150)
# plot(simulationOutput_7)
# dev.off()

print(summary(Mod_Species_6))

# Build summary of all models
FIXEF1 <- as.data.frame(coef(summary(Mod_Species_6))$cond)

Summary_Models = data.frame("Threshold" = rep(ThresholdSort, 2),
                            "Species" = rep(List_Sp[i],2),
                            "Type" = c("Conditional R2", "Marginal R2"),
                            "R2" = c(r2(Mod_Species_6)$R2_conditional[[1]], 
                                     r2(Mod_Species_6)$R2_marginal[[1]]),
                            "Estimate Forest" = c(rep(FIXEF1$Estimate[grepl(List_Sp[i], rownames(FIXEF1))], 2)),
                            "Std. Error Forest" = c(rep(FIXEF1$'Std. Error'[grepl(List_Sp[i], rownames(FIXEF1))], 2)),
                            "p-value Forest" = c(rep(FIXEF1$'Pr(>|z|)'[grepl(List_Sp[i], rownames(FIXEF1))], 2))
)

Summary_Models_Sp = rbind(Summary_Models_Sp, Summary_Models)
# }

Summary_Models_FINAL = rbind(Summary_Models_FINAL, Summary_Models_Sp)
# }

# fwrite(Summary_Models_FINAL,paste0(ModRF_directory, SpeciesGroupName, "_r2_capture_acoustic_500m.csv"))
fwrite(Summary_Models_FINAL,paste0(ModRF_directory, "r2_capture_acoustic_500m.csv"))

END=Sys.time()
END-START

beep(2)

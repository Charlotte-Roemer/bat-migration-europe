
# Author : Sarah Chayrigues (modified by Charlotte Roemer)
###
# Calculate sequence length
###

# This script calculates the sequence duration: aggregates bat sequences when they follow
# one another by less than X seconds (X is chosen in arg[4]) for each species

# Output : 
# Participation #
# Sequence #
# Species
# TempsDebutSequence : Time of the beginning of the sequence
# TempsFinSequence : Time of the end of the sequence
# DureeSequence : Duration of the sequence in seconds

library(tidyverse)
library(data.table)
library(lubridate)

arg = "" #participation chosen
#arg[2] = "C:/Users/croemer01/Documents/Donnees vigie-chiro/" #directory with raw data
arg[2] = "/mnt/beegfs/ybas/VigieChiro/Raw/" #directory with raw data
arg[3] = 2 #for short-range and mid-range echolocators, how many seconds of silence separate sequences? (by default 2)
arg[4] = 4 #for long-range echolocators, how many seconds of silence separate sequences? (by default 2)
#arg[5] = "C:/Users/croemer01/Documents/Donnees vigie-chiro/SpeciesList.csv" #path for the list of species
arg[5] = "/mnt/beegfs/ybas/VigieChiro/SpeciesList.csv" #path for the list of species
#arg[6] = "C:/Users/croemer01/Documents/Donnees vigie-chiro/SequenceDuration/" #output directory
arg[6] = "/mnt/beegfs/croemer/VigieChiro/SequenceDuration/" #output directory

# List of Long-range echolocators
LRE = c("nycnoc", "nyclas", "nyclei", "tadten")

# Read species list
list_esp <- read_delim(arg[5], delim=";", show_col_types = FALSE)
list_esp$Esp <- str_to_lower(list_esp$Esp) #Mettre tous les noms d'espèce en minuscule (pbm de compatibilité des 2 fichiers)

# List export files
ListExport = list.files(arg[2], pattern = "^DataLP_PF_export_", full.names = T)

# For each export file
rm(fichier_final)
for (k in 1:length(ListExport)){
  
  print(paste0(k, " out of ", length(ListExport), " export files"))
  
  # Load export
  fichier <- fread(ListExport[k])
  fichier=as.data.frame(fichier)
  
  #Only keep bats
  #Mettre tous les noms d'espèce en minuscule (pbm de compatibilité des 2 fichiers)
  fichier$espece <- str_to_lower(fichier$espece)
  fichier <- inner_join(fichier, list_esp[,c("Esp","Group")], by = c("espece"="Esp"))
  
  fichier <- subset(fichier, fichier$Group=="bat" )
  
  # For each participation, calculate sequence length for each species
  rm(export_l)
  for (l in 1: dim(table(fichier$participation))){
    
    Participation_l = names(table(fichier$participation))[l]
    print(paste0(l, " ", Participation_l))
    
    # Keep only rows of the chosen participation
    esp_part <- fichier[which(fichier$participation==Participation_l),]
    #esp_part <- subset(esp_tot,esp_tot$participation==arg[1])
    
    #Add datetime
    timestamps = gsub(".*?(([0-9]{8}_[0-9]{6})(_([0-9]{3}))?).*?$", "\\2.\\4", esp_part$donnee)
    times = as.POSIXct(timestamps, format="%Y%m%d_%H%M%OS")
    esp_part$date_esp = as.POSIXct(format(times, "%Y-%m-%d %H:%M:%OS5"))
    #Milli = substr(timestamps, nchar(timestamps)-2, nchar(timestamps))
    
    #esp_part$date_esp <- f2pPF(esp_part$donnee)
    esp_part=esp_part[order(esp_part$date_esp),]
    
    ListespP=unique(esp_part$espece)
    esp_part$NumeroSequence=NA
    
    
    compteur=0
    espSeqAggr=data.frame()
    for (i in 1:length(ListespP)) #For each species
    {
      espi=subset(esp_part,esp_part$espece==ListespP[i]) 
      compteur=compteur+1 #each time we change of species, we change the sequence
      print(paste(ListespP[i],nrow(espi),Sys.time())) #infos
      if(nrow(espi)==1){ #if only 1 file for the species
        espi$NumeroSequence=compteur 
      }else{ #if several files for the species
        for (j in 1:(nrow(espi)-1)) #we compare a row with the following row so stopping at the second to last
        {
          espi$NumeroSequence[j]=compteur 
          TimeEnd=espi$date_esp[j]+espi$temps_fin[j] #Time of the end of the sequence
          TimeStartNext=espi$date_esp[j+1]+espi$temps_debut[j+1] #Time of the start of the sequence
          if(ListespP[i] %in% LRE){ # if species is Long-range-echolocator
            if(TimeStartNext>(TimeEnd+as.numeric(arg[4]))) #if more than X seconds of silence between this sequence and the next one, it becomes a new sequence, else it is the same
            {
              compteur=compteur+1 
            }
          }else{
            if(TimeStartNext>(TimeEnd+as.numeric(arg[3]))) #if more than X seconds of silence between this sequence and the next one, it becomes a new sequence, else it is the same
            {
              compteur=compteur+1 
            }
          }
        }
        espi$NumeroSequence[nrow(espi)]=compteur #info for last row (no comparison)
      }
      espSeqAggr=rbind(espSeqAggr,espi) #add data for the species to the final table
    }
    length(unique(espSeqAggr$NumeroSequence)) #total number of sequences
    aggregate(espSeqAggr$NumeroSequence,by=list(espSeqAggr$espece)
              ,FUN=function(x) (length(unique(x)))) #number of sequences by species
    table(espSeqAggr$espece) #number of files by species (for comparison)
    
    # Calculates sequence duration
    
    especeSequence = aggregate(espSeqAggr$espece, by = list(espSeqAggr$NumeroSequence), 
                               unique)
    
    espSeqAggr$TimeStart=espSeqAggr$date_esp+espSeqAggr$temps_debut
    espSeqAggr$TimeEnd=espSeqAggr$date_esp+espSeqAggr$temps_fin
    
    TempsDebutSequence=aggregate(espSeqAggr$TimeStart
                                 ,by=list(espSeqAggr$NumeroSequence)
                                 ,min)
    TempsFinSequence=aggregate(espSeqAggr$TimeEnd
                               ,by=list(espSeqAggr$NumeroSequence)
                               ,max)
    DureeSequence=as.numeric(TempsFinSequence$x-TempsDebutSequence$x)
    
    
    ##
    # Final file
    ##
    
    fichier_Temp <- data.frame(Participation = Participation_l,
                               NumeroSequence = unique(espSeqAggr$NumeroSequence),
                               Espece = especeSequence[,2],
                               TempsDebutSequence = TempsDebutSequence[,2], 
                               TempsFinSequence = TempsFinSequence[,2], 
                               DureeSequence)
    
    if(exists("export_l")){
      export_l = rbind(export_l, fichier_Temp)
    }else{
      export_l = fichier_Temp
    }
  }
  
  if(exists("fichier_final")){
    fichier_final = rbind(fichier_final, export_l)
  }else{
    fichier_final = export_l
  }
  
}


write.csv(fichier_final, paste0(arg[6], "Sequence_length",".csv"), row.names = F)

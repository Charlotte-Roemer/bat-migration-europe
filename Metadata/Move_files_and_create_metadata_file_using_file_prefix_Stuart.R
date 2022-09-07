
library(stringr)
library(filesstrings)
library(data.table)
library(tidyverse)

pattern = "ta" # extension name of files (usually wav, ta or raw)
Directory = "E:/TEST2" # Directory name
FileDataName = "E:/Norfolk_Bat_Survey/Norfolk Bat Survey 201314.csv" # file containing coordinates
MetadataName = "C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Metadonnees/Return_from_partners_FINAL/Stuart_newson_metadata.csv" # final file with metadata
  
# List directory
lswav <- list.files(Directory,include.dirs = FALSE, recursive=TRUE,pattern=paste0("*.", pattern,"$"))
lswavDIR <- list.files(Directory,full.names = TRUE, recursive=TRUE,pattern=paste0("*.", pattern,"$"))

# Find files prefix
if(pattern=="ta"){
  prefix_lswav = substr(lswav,nchar(lswav)-100,nchar(lswav)-19)
}else{
  prefix_lswav = substr(lswav,nchar(lswav)-100,nchar(lswav)-20)
}
prefix_lswav = ifelse(prefix_lswav == "", "NONAME", prefix_lswav)

# Create site directories using filename prefix
list.outputs = paste0(Directory, "/", unique(prefix_lswav))
sapply(list.outputs, dir.create) # creates site directories

# Move files to correct site directories
matchdir = match(prefix_lswav,  sub(".*/", "",  list.outputs))
for (i in 1:length(lswav)){
  file.move(lswavDIR[i], list.outputs[matchdir[i]])
}

# Create participation directories using filename date (=1 folder per night)
for (i in 1:length(list.outputs)){ # does this for each site
  lswav.participation <- list.files(list.outputs[i],include.dirs = FALSE, recursive=TRUE,pattern=paste0("*.", pattern,"$"))
  lswav.participationDIR <- list.files(list.outputs[i],full.names = TRUE, recursive=TRUE,pattern=paste0("*.", pattern,"$"))
  
  if(pattern == "ta"){
    Date_Night= ifelse(str_sub(lswav.participation,-9,-9)==0,
                       as.character(as.Date(paste(str_sub(lswav.participation,-18,-15),
                                                  str_sub(lswav.participation,-14,-13),
                                                  str_sub(lswav.participation,-12,-11),
                                                  sep="-"))-1),
                       as.character(as.Date(paste(str_sub(lswav.participation,-18,-15),
                                                  str_sub(lswav.participation,-14,-13),
                                                  str_sub(lswav.participation,-12,-11),
                                                  sep="-"))))
  }
  if(pattern == "wav" | pattern == "raw"){
    Date_Night= ifelse(str_sub(lswav.participation,-10,-10)==0,
                       as.character(as.Date(paste(str_sub(lswav.participation,-19,-16),
                                                  str_sub(lswav.participation,-15,-14),
                                                  str_sub(lswav.participation,-13,-12),
                                                  sep="-"))-1),
                       as.character(as.Date(paste(str_sub(lswav.participation,-19,-16),
                                                  str_sub(lswav.participation,-15,-14),
                                                  str_sub(lswav.participation,-13,-12),
                                                  sep="-"))))
  }
  if(pattern != "ta" & pattern !="wav" & pattern !="raw") {
    print("pattern is not recognised to calculate date")}
  
  list.outputs2 = paste0(list.outputs[i], "/", unique(Date_Night))
  sapply(list.outputs2, dir.create) # sites
  
  # Move files to participation directories
  matchdir = match(Date_Night,  sub(".*/", "",  list.outputs2))
  for (j in 1:length(lswav.participationDIR)){
    file.move(lswav.participationDIR[j], list.outputs2[matchdir[j]])
  }
}

###                              ###
#### WORKS VERY WELL UNTIL HERE ####
###                              ###

# Find coordinates in stuarts file using prefix
FileData = fread(FileDataName)
FileData$Site = substr(FileData$`RECORDING FILE NAME`,
                             nchar(FileData$`RECORDING FILE NAME`)-100,
                             nchar(FileData$`RECORDING FILE NAME`)-20)
FileData$Site = ifelse(FileData$Site == "", "NONAME", FileData$Site)
FileData$Participation= ifelse(str_sub(FileData$`RECORDING FILE NAME`,-10,-10)==0,
                   as.character(as.Date(paste(str_sub(FileData$`RECORDING FILE NAME`,-19,-16),
                                              str_sub(FileData$`RECORDING FILE NAME`,-15,-14),
                                              str_sub(FileData$`RECORDING FILE NAME`,-13,-12),
                                              sep="-"))-1),
                   as.character(as.Date(paste(str_sub(FileData$`RECORDING FILE NAME`,-19,-16),
                                              str_sub(FileData$`RECORDING FILE NAME`,-15,-14),
                                              str_sub(FileData$`RECORDING FILE NAME`,-13,-12),
                                              sep="-"))))
FileData2 = FileData %>% 
  select(X=LONGITUDE, Y=LATITUDE, Site, Participation) %>% 
  unique()

Metadata = fread(MetadataName)

Metadata2 = plyr::rbind.fill(Metadata, FileData2)
Metadata2$FirstName = Metadata2$FirstName[1]
Metadata2$FamilyName = Metadata2$FamilyName[1]
Metadata2$Email = Metadata2$Email[1]
Metadata2$Country = Metadata2$Country[1]
Metadata2$Affiliation = Metadata2$Affiliation[1]

Metadata2 = Metadata2[-1,]

Metadata2$StartDate = as.Date(Metadata2$Participation)
Metadata2$EndDate = Metadata2$StartDate + 1
Metadata2$StartTime = "18:00:00"
Metadata2$EndTime = NA # Need Info
Metadata2$TypeStudy = 1
Metadata2$MicHeight = NA # 3 m ?
Metadata2$Recorder = 18
Metadata2$Mic = 1
Metadata2$GainRecorder = 48
Metadata2$HPF = 1 # or 8 kHz ?
Metadata2$LPF = 999
Metadata2$FreqMin = 4
Metadata2$FreqMax = 999
Metadata2$TriggerLevel = NA # Need Info
Metadata2$MinDur= NA # Need Info
Metadata2$MaxDur = NA # Need Info
Metadata2$TrigWin = 2 
Metadata2$Pause = 0
Metadata2$TrigWinMax = NA # Need Info
Metadata2$FileSplittingLength = 5
Metadata2$NoiseFilter = NA # Need Info


# save file




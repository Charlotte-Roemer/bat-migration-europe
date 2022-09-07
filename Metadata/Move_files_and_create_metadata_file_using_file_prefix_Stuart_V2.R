
library(stringr)
library(filesstrings)
library(data.table)
library(tidyverse)
library(beepr)

pattern = "ta" # extension name of files (usually wav, ta or raw)
Directory = "D:/Ukraine_and_Belarus_SNEWSON/Ta_files/Hard_drive_xx01/ta_files" # Directory name where files are
Directory_out = "D:/Ukraine_and_Belarus_SNEWSON/Ta_files/organised" # Directory name where to copy the files
FileDataName = "D:/Ukraine_and_Belarus_SNEWSON/Checked_results_Polesia_2019_2021.csv" # file containing coordinates
MetadataDir = "C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Metadonnees/Return_from_partners_FINAL/"
MetadataName = "Stuart_newson_Belarus-Ukraine" # file with metadata

nx = nchar(pattern)
nwav = 3

# Create Site names
FileData = fread(FileDataName)
FileData = FileData %>% 
  mutate(Site = paste0("X", str_sub(LONGITUDE, 0, 50), 
                       "Y",str_sub(LATITUDE, 0, 50))) %>% 
  as.data.frame()

# If midnight is written 0 instead of 00, adds a 0
FileData$`RECORDING FILE NAME` = ifelse(str_sub(FileData$`RECORDING FILE NAME`,(-8),(-8))=="_",
                                        ifelse(str_sub(FileData$`RECORDING FILE NAME`,(-14),(-14))=="_",
                                               paste0(str_sub(FileData$`RECORDING FILE NAME`, -100,-14), "0", str_sub(FileData$`RECORDING FILE NAME`,-13,-1)),
                                               FileData$`RECORDING FILE NAME`),
                                        FileData$`RECORDING FILE NAME`)

# If end of the name contains species name, removes it
pats = c("_Barbar\\(social\\)_Barbar|_Barbar\\(social\\)|_Myobra\\(social\\)|_Myodau\\(social\\)|_Myomys\\(social\\)|_Myonat\\(social\\)|_Nyclei\\(social\\)|_Nycnoc\\(social\\)|_Pipnat\\(social\\)|_Pleaur\\(social\\)|_Nycnoc\\(buzz\\)")
FileData$`RECORDING FILE NAME` = str_replace_all(FileData$`RECORDING FILE NAME`, pats, "")

FileData$`RECORDING FILE NAME` = ifelse(is.na(suppressWarnings(as.numeric(str_sub(FileData$`RECORDING FILE NAME`, 
                              (-nwav-2),(-nwav-2))))),
                              paste0(str_sub(FileData$`RECORDING FILE NAME`, -100,-12), ".wav"),
                              FileData$`RECORDING FILE NAME`)

#### CREATE METADATA FILE ####

# Create participation names
for(i in 1:nrow(FileData)){
  if(str_sub(FileData$`RECORDING FILE NAME`[i], (-nwav-5),(-nwav-5))=="_"){
    FileData$Participation[i]= ifelse(str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-11),(-nwav-11))==0,
                                      as.character(as.Date(paste(str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-20),(-nwav-17)),
                                                                 str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-16),(-nwav-15)),
                                                                 str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-14),(-nwav-13)),
                                                                 sep="-"))-1),
                                      as.character(as.Date(paste(str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-20),(-nwav-17)),
                                                                 str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-16),(-nwav-15)),
                                                                 str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-14),(-nwav-13)),
                                                                 sep="-"))))
  }else{
    FileData$Participation[i]= ifelse(str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-7),(-nwav-7))==0,
                                      as.character(as.Date(paste(str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-16),(-nwav-13)),
                                                                 str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-12),(-nwav-11)),
                                                                 str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-10),(-nwav-9)),
                                                                 sep="-"))-1),
                                      as.character(as.Date(paste(str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-16),(-nwav-13)),
                                                                 str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-12),(-nwav-11)),
                                                                 str_sub(FileData$`RECORDING FILE NAME`[i],(-nwav-10),(-nwav-9)),
                                                                 sep="-")))) 
  }
}

if(TRUE %in% names(table(is.na(FileData$Participation)))){
  print(paste0("WARNING : ", as.numeric(table(is.na(FileData$Participation))[2]),
               " Participations don't have a name"))
}

beep(2)

head(which(is.na(FileData$Participation)))
head(FileData[which(is.na(FileData$Participation)),])

# List participations in metadata file
FileData2 = FileData %>% 
  select(X=LONGITUDE, Y=LATITUDE, Site, Participation) %>% 
  unique()

Metadata = fread(paste0(MetadataDir, MetadataName, ".csv"))

Metadata2 = plyr::rbind.fill(Metadata, FileData2)
Metadata2$FirstName = Metadata2$FirstName[1]
Metadata2$FamilyName = Metadata2$FamilyName[1]
Metadata2$Email = Metadata2$Email[1]
Metadata2$Country = Metadata2$Country[1]
Metadata2$Affiliation = Metadata2$Affiliation[1]

Metadata2$StartDate = as.Date(Metadata2$Participation)
Metadata2$EndDate = Metadata2$StartDate + 1

Metadata2[,c("StartTime", "EndTime", "TypeStudy", "MicHeight", "Recorder", "Mic", "GainRecorder", 
             "HPF", "LPF", "FreqMin", "FreqMax", "TriggerLevel", "MinDur", "MaxDur", "TrigWin", "Pause", 
             "TrigWinMax", "FileSplittingLength", "NoiseFilter", "Partner", "Comment")] = Metadata2[1,c("StartTime", "EndTime", "TypeStudy", "MicHeight", "Recorder", "Mic", "GainRecorder", 
                                                                                                        "HPF", "LPF", "FreqMin", "FreqMax", "TriggerLevel", "MinDur", "MaxDur", "TrigWin", "Pause", 
                                                                                                        "TrigWinMax", "FileSplittingLength", "NoiseFilter", "Partner", "Comment")]
Metadata3 = Metadata2[-1,]

# save file
fwrite(Metadata3, paste0(MetadataDir, MetadataName, "_Polesia_2019-2021_Metadata_table.csv"))

#### MOVE FILES TO SITES ####

# List files and directory
lswav <- list.files(Directory,include.dirs = FALSE, recursive=TRUE,pattern=paste0("*.", pattern,"$"))
lswavDIR <- list.files(Directory,full.names = TRUE, recursive=TRUE,pattern=paste0("*.", pattern,"$"))

# If midnight is written 0 instead of 00, adds a 0 and rename files
lswavDIR_to_rename = ifelse(str_sub(lswavDIR,(-nx-5),(-nx-5))=="_",
                            ifelse(str_sub(lswavDIR,(-nx-11),(-nx-11))=="_",
                                   paste0(str_sub(lswavDIR, -100,(-nx-11)), "0", str_sub(lswavDIR,(-nx-10),(-1))),
                                   lswavDIR),
                            lswavDIR)

# If end of the name contains species name, removes it
pats = c("_Barbar\\(social\\)_Barbar|_Barbar\\(social\\)|_Myobra\\(social\\)|_Myodau\\(social\\)|_Myomys\\(social\\)|_Myonat\\(social\\)|_Nyclei\\(social\\)|_Nycnoc\\(social\\)|_Pipnat\\(social\\)|_Pleaur\\(social\\)|_Nycnoc\\(buzz\\)")
lswavDIR_to_rename = str_replace_all(lswavDIR_to_rename, pats, "")

lswavDIR_to_rename = ifelse(is.na(suppressWarnings(as.numeric(str_sub(lswavDIR_to_rename, 
                                                                                  (-nwav-2),(-nwav-2))))),
                                        paste0(str_sub(lswavDIR_to_rename, -100,-12), ".wav"),
                            lswavDIR_to_rename)


for (i in 1:length(lswavDIR)){
  if(lswavDIR[i] != lswavDIR_to_rename[i]){
    file.rename(lswavDIR[i], lswavDIR_to_rename[i])
  }
}

# Match directory name and filename (match only until date, ignore time because only files with bats are in the FileData)
matchname = match(str_sub(lswav, -100,(-9-nx)),
                  str_sub(FileData$`RECORDING FILE NAME`, -100, -12))
matchdir = FileData$Site[matchname]

# Create site directories if their file is present
list.outputs = paste0(Directory_out, "/", unique(matchdir[!is.na(matchdir)]))
sapply(list.outputs, dir.create) # creates site directories

# Move files to correct site directories
for (i in 1:length(lswav)){
  if(!is.na(matchdir[i])){
    file.move(lswavDIR[i], paste0(Directory_out, "/", matchdir[i]))
  }
}

#### MOVE FILES TO PARTICIPATIONS ####

# Especially useful if you had to do the previous operation in several times
list.outputs = list.dirs(Directory_out, full.names=T, recursive = F) 

# Create participation directories using filename date (=1 folder per night)
for (j in 1:length(list.outputs)){ # does this for each site
  lswav.participation <- list.files(list.outputs[j],include.dirs = FALSE, recursive=TRUE,pattern=paste0("*.", pattern,"$"))
  lswav.participationDIR <- list.files(list.outputs[j],full.names = TRUE, recursive=TRUE,pattern=paste0("*.", pattern,"$"))
  
  Date_Night=vector()
  for(i in 1:length(lswav.participation)){
    if(str_count(lswav.participationDIR[i], "/")==4){
      if(str_sub(lswav.participation[i], (-nx-5),(-nx-5))=="_"){
        Date_Night[i]= ifelse(str_sub(lswav.participation[i],(-nx-11),(-nx-11))==0,
                              as.character(as.Date(paste(str_sub(lswav.participation[i],(-nx-20),(-nx-17)),
                                                         str_sub(lswav.participation[i],(-nx-16),(-nx-15)),
                                                         str_sub(lswav.participation[i],(-nx-14),(-nx-13)),
                                                         sep="-"))-1),
                              as.character(as.Date(paste(str_sub(lswav.participation[i],(-nx-20),(-nx-17)),
                                                         str_sub(lswav.participation[i],(-nx-16),(-nx-15)),
                                                         str_sub(lswav.participation[i],(-nx-14),(-nx-13)),
                                                         sep="-"))))
      }else{
        Date_Night[i]= ifelse(str_sub(lswav.participation[i],(-nx-7),(-nx-7))==0,
                              as.character(as.Date(paste(str_sub(lswav.participation[i],(-nx-16),(-nx-13)),
                                                         str_sub(lswav.participation[i],(-nx-12),(-nx-11)),
                                                         str_sub(lswav.participation[i],(-nx-10),(-nx-9)),
                                                         sep="-"))-1),
                              as.character(as.Date(paste(str_sub(lswav.participation[i],(-nx-16),(-nx-13)),
                                                         str_sub(lswav.participation[i],(-nx-12),(-nx-11)),
                                                         str_sub(lswav.participation[i],(-nx-10),(-nx-9)),
                                                         sep="-")))) 
      }
    }
  }
  list.outputs2 = paste0(list.outputs[j], "/", unique(na.omit(Date_Night)))
  sapply(list.outputs2, dir.create) # participations
  
  # Move files to participation directories
  matchdir = match(Date_Night,  sub(".*/", "",  list.outputs2))
  for (k in 1:length(lswav.participationDIR)){
    if(!is.na(list.outputs2[matchdir[k]])){
      file.move(lswav.participationDIR[k], list.outputs2[matchdir[k]])
    }
  }
}


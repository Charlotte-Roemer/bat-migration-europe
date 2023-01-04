

library(data.table)
library(tidyverse)
library(lubridate)
library(sf)


setwd("C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Metadonnees/Return_from_partners_FINAL")

#### List and load metadata files ####
csv.list <- list.files("C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Metadonnees/Return_from_partners_FINAL",
                       pattern='*Metadata_table.csv', recursive = TRUE)

csv.df.list <- lapply(csv.list, fread, na.strings=c("-")) 
# /!\ some tables have a weird behaviour (e.g. when asking head(table Arnold Andreasson) after formatting time)
# This is probably due to the fread function
# There was no problem until the step of rbind
# The workaround is to do as.data.frame() for all tables

# Check if number and names of columns is right
name_col=c("FirstName",	"FamilyName",	"Email",	"Affiliation",	"Country",	"Site",	"X",	"Y",	
           "Participation",	"StartDate",	"EndDate",	"StartTime",	"EndTime",	"TypeStudy",	
           "TypeStudyOther",	"PositionNacelle",	"MicHeight",	"Recorder",	"RecorderOther",	
           "Mic",	"MicOther",	"GainRecorder",	"HPF",	"LPF",	"FreqMin",	"FreqMax",	"TriggerLevel",	
           "MinDur",	"MaxDur",	"TrigWin",	"Pause",	"TrigWinMax",	"FileSplittingLength",	
           "NoiseFilter",	"Partner",	"Comment")

for (i in 1:length(csv.df.list)){
  Affiliation = csv.df.list[[i]]$Affiliation[1]
  if(ncol(csv.df.list[[i]]) != 36) {
    print(paste0("error: number of columns is incorrect for ", Affiliation))
    print(paste0("n columns =  ", ncol(csv.df.list[[i]]) ))
  }
  matchCOL=match (colnames(csv.df.list[[i]]), name_col)
  if(TRUE %in% is.na(matchCOL)){
    print(paste0("error: column names are incorrect for ", Affiliation))
    print(paste0("missing columns: ", colnames(csv.df.list[[i]])[which(is.na(matchCOL))]))
  }
}

#### Check column format ####
StandardFormat = c(rep("character",6), rep("numeric", 2), rep("character",5),
                   "integer",  rep("character", 2), "numeric", "integer", "character",
                   "integer", "character", rep("integer", 12), rep("character",3))

for (i in 1:length(csv.df.list)){ # Next time, make of function of this and run it after step TIME format
  Affiliation = csv.df.list[[i]]$Affiliation[1]
  print("")
  print(Affiliation)
  for (j in 1:length(StandardFormat)){
    if(!class(as.data.frame(csv.df.list[[i]])[,j])[1]=="POSIXct" &
       !class(as.data.frame(csv.df.list[[i]])[,j])[1]=="IDate"){
      if(names(table(as.data.frame(lapply(csv.df.list[[i]], class)[j]))) != StandardFormat[j]) {
        if(!all(is.na(csv.df.list[[i]][,..j]))  &
           !all((csv.df.list[[i]][,..j]) == "NA") ){ # if the column is not empty
          if (!((names(table(as.data.frame(lapply(csv.df.list[[i]], class)[j]))) == "integer" &
                 StandardFormat[j] == "numeric") | # considers that integer = numeric
                (names(table(as.data.frame(lapply(csv.df.list[[i]], class)[j]))) == "numeric" &
                 StandardFormat[j] == "integer"))) { # considers that numeric = integer
            if(!(j==22 & "Audiomoth" %in% csv.df.list[[i]]$Recorder)){
              print(paste0("wrong format for ",  names(as.data.frame(lapply(csv.df.list[[i]], class)[j])),
                           ": found ", names(table(as.data.frame(lapply(csv.df.list[[i]], class)[j]))), 
                           " instead of ", StandardFormat[j]))
            }
          }
        }
      }
    }
  }
}

#### Remove units in values and change format to numeric ####
csv.df.list = csv.df.list %>% 
  map(~ .x %>% 
        mutate(MicHeight = gsub("[^0-9.-]", "", MicHeight))) %>% 
  map(~ .x %>% 
        mutate(LPF = gsub("[^0-9.-]", "", LPF))) %>%  
  lapply(transform, MicHeight = as.numeric(MicHeight)) %>% 
  lapply(transform, LPF = as.numeric(LPF)) %>% 
  lapply(transform, HPF = as.numeric(HPF)) %>% 
  lapply(transform, Participation = as.character(Participation)) %>% 
  lapply(transform, GainRecorder = as.character(GainRecorder)) %>% # because of Audiomoths
  lapply(transform, FreqMax = as.numeric(FreqMax))


for(i in 1:length(csv.df.list)){ # Check that people did not only write characters in these columns
  Affiliation = csv.df.list[[i]]$Affiliation[1]
  if(all(is.na(csv.df.list[[i]]$MicHeight))) {
    print(paste0("warning: column MicHeight is empty for ", Affiliation))
  }
  if(all(is.na(csv.df.list[[i]]$LPF))) {
    print(paste0("warning: column LPF is empty for ", Affiliation))
  }
}

#### Replace characters by numeric (e.g. Recorder = Audiomoth instead of 3) ####
for (i in 1:length(csv.df.list)){ # Y coordinates should be greater values than X
  Affiliation = csv.df.list[[i]]$Affiliation[1]
  print("")
  print(Affiliation)
  Break1 = FALSE
  Break2 = FALSE
  for(j in 1:nrow(csv.df.list[[i]])){
    if(is.character(csv.df.list[[i]]$TypeStudy[j])){
      if(csv.df.list[[i]]$TypeStudy[j]=="ground-level" |
         csv.df.list[[i]]$TypeStudy[j]=="Ground-level"){
        csv.df.list[[i]]$TypeStudy[j]=1 
        if(Break1 == FALSE){
          print(paste0("warning: replacing TypeStudy character by a numeric"))
          Break1=TRUE
        }
      }
      if(csv.df.list[[i]]$TypeStudy[j]=="Tree canopy" |
         csv.df.list[[i]]$TypeStudy[j]=="tree canopy"){
        csv.df.list[[i]]$TypeStudy[j]=2 
        if(Break1 == FALSE){
          print(paste0("warning: replacing TypeStudy character by a numeric"))
          Break1=TRUE
        }
      }
      if(csv.df.list[[i]]$TypeStudy[j]=="Wind mast" |
         csv.df.list[[i]]$TypeStudy[j]=="wind mast"){
        csv.df.list[[i]]$TypeStudy[j]=3
        if(Break1 == FALSE){
          print(paste0("warning: replacing TypeStudy character by a numeric"))
          Break1=TRUE
        }
      }
      if(csv.df.list[[i]]$TypeStudy[j]=="Wind turbine" |
         csv.df.list[[i]]$TypeStudy[j]=="wind turbine"){
        csv.df.list[[i]]$TypeStudy[j]=4
        if(Break1 == FALSE){
          print(paste0("warning: replacing TypeStudy character by a numeric"))
          Break1=TRUE
        }
      }
      if(csv.df.list[[i]]$TypeStudy[j]=="Building" |
         csv.df.list[[i]]$TypeStudy[j]=="building"){
        csv.df.list[[i]]$TypeStudy[j]=5
        if(Break1 == FALSE){
          print(paste0("warning: replacing TypeStudy character by a numeric"))
          Break1=TRUE
        }
      }
      if(csv.df.list[[i]]$TypeStudy[j]=="Other" |
         csv.df.list[[i]]$TypeStudy[j]=="other"){
        csv.df.list[[i]]$TypeStudy[j]=6
        if(Break1 == FALSE){
          print(paste0("warning: replacing TypeStudy character by a numeric"))
          Break1=TRUE
        }
      }
    }
    if(is.character(csv.df.list[[i]]$Recorder[j])){
      if(csv.df.list[[i]]$Recorder[j]=="SM2BAT"){
        csv.df.list[[i]]$Recorder[j]=17 
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Audiomoth"){
        csv.df.list[[i]]$Recorder[j]=3
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="AudioMoth" |
         csv.df.list[[i]]$Mic[j]=="Audiomoth"){
        csv.df.list[[i]]$Mic[j]=24
        if(Break2 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="SMX-US"){
        csv.df.list[[i]]$Mic[j]=1
        if(Break2 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break2=TRUE
        }
      }
    }
  }
}

#### Change format again to numeric ####
csv.df.list = csv.df.list %>% 
  lapply(transform, Recorder = as.numeric(Recorder)) %>% 
  lapply(transform, Mic = as.numeric(Mic)) %>% 
  lapply(transform, TypeStudy = as.numeric(TypeStudy))

#### Make DATE format ####
for(i in 1:length(csv.df.list)){
  if(grepl("/", csv.df.list[[i]]$StartDate[1])){
    csv.df.list[[i]]$StartDate = dmy(csv.df.list[[i]]$StartDate)
    csv.df.list[[i]]$EndDate = dmy(csv.df.list[[i]]$EndDate)
  }
}
for(i in 1:length(csv.df.list)){
  if(grepl(".", csv.df.list[[i]]$StartDate[1]) &
     nchar(gsub("\\..*","",csv.df.list[[i]]$StartDate[1]))==4){
    csv.df.list[[i]]$StartDate = ymd(csv.df.list[[i]]$StartDate)
    csv.df.list[[i]]$EndDate = ymd(csv.df.list[[i]]$EndDate)
  }
}
csv.df.list = lapply(csv.df.list,function(x) { # /!\ RUN ONLY ONCE
  x$StartDate <- as.Date( x$StartDate, format="%Y-%m-%d");
  x})
csv.df.list = lapply(csv.df.list,function(x) { # /!\ RUN ONLY ONCE
  x$EndDate <- as.Date( x$EndDate, format="%Y-%m-%d");
  x})
for(i in 1:length(csv.df.list)){ # Check that date columns are not empty
  Affiliation = csv.df.list[[i]]$Affiliation[1]
  if(all(is.na(csv.df.list[[i]]$StartDate))) {
    print(paste0("warning: column StartDate is empty for ", Affiliation))
  }
  if(all(is.na(csv.df.list[[i]]$EndDate))) {
    print(paste0("warning: column EndDate is empty for ", Affiliation))
  }
}

#### Make TIME format ####
for(i in 1:length(csv.df.list)){
  if(!all(is.na(csv.df.list[[i]]$StartTime))){
    if(grepl("PM", csv.df.list[[i]]$StartTime[1]) |
       grepl("AM", csv.df.list[[i]]$StartTime[1])){
      csv.df.list[[i]]$StartTime =  hms(format(strptime(csv.df.list[[i]]$StartTime, "%I:%M:%S %p"), "%H:%M:%S"))
      csv.df.list[[i]]$EndTime =  hms(format(strptime(csv.df.list[[i]]$EndTime, "%I:%M:%S %p"), "%H:%M:%S"))
    }else{
      if(str_count(csv.df.list[[i]]$StartTime[1], ":")==1){
        csv.df.list[[i]]$StartTime = hm(csv.df.list[[i]]$StartTime) #PROBLEM
        csv.df.list[[i]]$EndTime = hm(csv.df.list[[i]]$EndTime) #PROBLEM
      }else{
        csv.df.list[[i]]$StartTime = hms(csv.df.list[[i]]$StartTime)
        csv.df.list[[i]]$EndTime = hms(csv.df.list[[i]]$EndTime)
      }
    }
  }
}
for(i in 1:length(csv.df.list)){ # Check that time columns are not empty
  Affiliation = csv.df.list[[i]]$Affiliation[1]
  if(all(is.na(csv.df.list[[i]]$StartTime))) {
    print(paste0("warning: column StartTime is empty for ", Affiliation))
  }
  if(all(is.na(csv.df.list[[i]]$EndTime))) {
    print(paste0("warning: column EndTime is empty for ", Affiliation))
  }
}

#### Check coordinates ####
for (i in 1:length(csv.df.list)){ # Y coordinates should be greater values than X
  Affiliation = csv.df.list[[i]]$Affiliation[1]
  if(csv.df.list[[i]]$Y[1]<csv.df.list[[i]]$X[1]){
    print(paste0("warning: X coord greater than Y coord for ", Affiliation))
    print("thus I am inversing the column values")
    YTemp = csv.df.list[[i]]$X
    XTemp = csv.df.list[[i]]$Y
    csv.df.list[[i]]$X = XTemp
    csv.df.list[[i]]$Y = YTemp
  }
  for(j in 1:nrow(csv.df.list[[i]]))
    if(is.na(csv.df.list[[i]]$Y[j])){
      print(j)
      print(paste0("warning: missing Y coordinates for ", Affiliation))
    }
  if(is.na(csv.df.list[[i]]$X[j])){
    print(paste0("warning: missing X coordinates for ", Affiliation))
  }
}

countries <- c(
  "Andorra", "Belarus", "Bulgaria", "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands", "Norway(?!:Svalbard)", "Monaco", "Montenegro", 
  "Sweden", "Finland", "Latvia", "Lithuania", "Luxembourg", "Albania", "Serbia",
  "Denmark", "Poland", "Italy", "Estonia", "North Macedonia", 
  "Croatia", "Slovenia", "Hungary", "Slovakia", "Greece", "Ireland", "Kosovo", "Malta", "Moldova",
  "Czech republic", "Ukraine", "Romania", "Liechtenstein", 
  "Bosnia and Herzegovina", "Russia:32"
)

Europe <- ggplot2::map_data("world", region = countries) # Load Europe boundaries

Europe_sf = st_as_sf( # Make spatial object
  Europe, coords = c("long", "lat"), crs=4326, remove=FALSE)  %>%
  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

for (i in 1:length(csv.df.list)){ # Make spatial object
  Affiliation = csv.df.list[[i]]$Affiliation[1]
  Site_localite_sf = st_as_sf( 
    csv.df.list[[i]], coords = c("X", "Y"), crs=4326, remove=FALSE) %>% 
    st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
  Site_localite_Europe_sf = Site_localite_sf %>% # Crop data in European boundaries
    st_crop(st_bbox(Europe_sf))
  
  if(nrow(Site_localite_Europe_sf)<nrow(Site_localite_sf)){ # check that coordinates are in boundaries
    print(paste0("warning: coordinates out of Europe for ", Affiliation))
  }
}

# Plot
X_Y_for_plot = do.call(rbind, lapply(csv.df.list, subset, select=c("X", "Y")))
X_Y_for_plot_sf = st_as_sf( 
  X_Y_for_plot, coords = c("X", "Y"), crs=4326, remove=FALSE) %>% 
  st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
ggplot() +
  geom_sf(data=Europe_sf, fill="white") +
  geom_sf(data=X_Y_for_plot_sf)

#### Check that values are in the expected ranges and correct mistakes between 0 and 999####
csv.df.list2 = csv.df.list
RecordersAbsoluteTrigger = c(3, 7, 12, 13, 14, 16:21)
for (i in 1:length(csv.df.list2)){
  Affiliation = csv.df.list2[[i]]$Affiliation[1]
  print("")
  print(paste0(Affiliation, ":"))
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list2[[i]])){
    if(csv.df.list2[[i]]$TypeStudy[j] > 6 & Break == FALSE) { 
      print(paste0("warning: wrong TypeStudy "))
      Break = TRUE
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list2[[i]])){
    if(!is.na(csv.df.list2[[i]]$Recorder[j])){
      if(as.numeric(csv.df.list2[[i]]$Recorder[j]) > 24 & Break == FALSE) { 
        print(paste0("warning: wrong Recorder"))
        Break = TRUE
      }
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list2[[i]])){
    if(!is.na(csv.df.list2[[i]]$Mic[j])){
      if(csv.df.list2[[i]]$Mic[j] > 25 & Break == FALSE) { 
        print(paste0("warning: wrong Mic"))
        Break = TRUE
      }
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list2[[i]])){
    if(!is.na(csv.df.list2[[i]]$GainRecorder[j]) &
       csv.df.list2[[i]]$GainRecorder[j] != 999 &
       csv.df.list2[[i]]$GainRecorder[j] > 100 & Break == FALSE) { 
      print(paste0("warning: wrong GainRecorder"))
      Break = TRUE
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list2[[i]])){
    if(!is.na(csv.df.list2[[i]]$HPF[j]) &
       csv.df.list2[[i]]$HPF[j] != 0 &
       csv.df.list2[[i]]$HPF[j] > 20 & Break == FALSE) { 
      print(paste0("warning: wrong HPF"))
      Break = TRUE
    }
  }
  
  Break1= FALSE
  Break2= FALSE
  for(j in 1:nrow(csv.df.list2[[i]])){
    if(!is.na(csv.df.list2[[i]]$LPF[j]) &
       csv.df.list2[[i]]$LPF[j] != 999 &
       (csv.df.list2[[i]]$LPF[j] < 50 | csv.df.list2[[i]]$LPF[j] > 300) &
       Break2 == FALSE) # Avoids printing message for each row
    {
      if(csv.df.list2[[i]]$LPF[j] == 0 ){
        csv.df.list2[[i]]$LPF = 999
        if(Break1 == FALSE){
          print(paste0("warning: LPF has value 0 instead of 999"))
          print("thus I am replacing by 999")
          Break1=TRUE
        }
      }else{
        print(paste0("warning: wrong LPF"))
        Break2=TRUE
      }
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list2[[i]])){
    if(!is.na(csv.df.list2[[i]]$FreqMin[j]) &
       csv.df.list2[[i]]$FreqMin[j] != 0 &
       csv.df.list2[[i]]$FreqMin[j] > 20 & Break == FALSE) { 
      print(paste0("warning: wrong FreqMin"))
      Break = TRUE
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list2[[i]])){
    if(!is.na(csv.df.list2[[i]]$FreqMax[j]) &
       csv.df.list2[[i]]$FreqMax[j] != 999 &
       (csv.df.list2[[i]]$FreqMax[j] < 50 |  csv.df.list2[[i]]$FreqMax[j]  > 300) &
       Break == FALSE) { 
      print(paste0("warning: wrong FreqMax"))
      Break = TRUE
    }
  }
  
  Break1 = FALSE
  Break2 = FALSE
  for(j in 1:nrow(csv.df.list2[[i]])) {
    if(!is.na(csv.df.list2[[i]]$TriggerLevel[j]) &
       csv.df.list2[[i]]$TriggerLevel[j] != 0 &
       as.numeric(csv.df.list2[[i]]$TriggerLevel[j]  < -100 | csv.df.list2[[i]]$TriggerLevel[j]  > 0)){
      if(csv.df.list2[[i]]$Recorder[j] %in% RecordersAbsoluteTrigger) 
      {
        if(Break1 == FALSE){
          print(paste0("warning: TriggerLevel has positive value"))
          print("thus I am replacing by the negative")
          Break1 = TRUE
        }
        csv.df.list2[[i]]$TriggerLevel[j]=-as.numeric(csv.df.list2[[i]]$TriggerLevel[j])
      }else{
        if(Break2 == FALSE){
          print(paste0("warning: wrong TriggerLevel"))
          Break2 = TRUE
        }
      }
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list2[[i]])){
    if(!is.na(csv.df.list2[[i]]$MinDur[j]) &
       as.numeric(csv.df.list2[[i]]$MinDur[j]) != 0 &
       (csv.df.list2[[i]]$MinDur[j] < 1 |  csv.df.list2[[i]]$MinDur[j]  > 10) & 
       Break == FALSE) {
      print(paste0("warning: wrong MinDur"))
      Break=TRUE
    }
  }
  
  Break1= FALSE
  Break2= FALSE
  for(j in 1:nrow(csv.df.list2[[i]])){
    if(!is.na(csv.df.list2[[i]]$MaxDur[j]) &
       csv.df.list2[[i]]$MaxDur[j] != 999 &
       csv.df.list2[[i]]$MaxDur[j] < 100) # Avoids printing message for each row
    {
      if(csv.df.list2[[i]]$MaxDur[j] == 0 ){
        csv.df.list2[[i]]$MaxDur = 999
        if(Break1 == FALSE){
          print(paste0("warning: MaxDur has value 0 instead of 999"))
          print("thus I am replacing by 999")
          Break1=TRUE
        }
      }else{
        if(Break2 == FALSE){
          print(paste0("warning: wrong MaxDur"))
          Break2=TRUE
        }
      }
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list2[[i]])){
    if(!is.na(csv.df.list2[[i]]$TrigWin[j]) &
       csv.df.list2[[i]]$TrigWin[j] != 999 &
       csv.df.list2[[i]]$TrigWin[j] > 60 & Break == FALSE) 
    {
      print(paste0("warning: wrong TrigWin"))
      Break = TRUE
    }
  }
  ### ADD checks for DATE, TIME, etc ### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}



#### Check that required columns are not empty ####
Required_columns = c("FirstName",	"FamilyName",	"Email",	"Affiliation",	"Country",	"Site",	"X",	"Y",	
                     "Participation",	"StartDate",	"EndDate",	"StartTime",	"EndTime",	"TypeStudy",	
                     "Recorder",	"GainRecorder",	"HPF",	"LPF",	"FreqMin",	"FreqMax",	"TriggerLevel",	
                     "MinDur",	"MaxDur",	"TrigWin",	"Pause", "NoiseFilter")

for (i in 1:length(csv.df.list2)){
  Affiliation = csv.df.list2[[i]]$Affiliation[1]
  print("")
  print(Affiliation)
  PositionCol = match(Required_columns, names(csv.df.list2[[i]]))
  for (j in 1:length(PositionCol)){
    if(all(is.na((csv.df.list2[[i]][,..PositionCol])))){
      print(paste0("warning: required column ", Required_columns[j], " is empty"))
    }
  }
}

#### Rbind data (at the IN2P3 the tables do not need to be merged) ####
csv.df.list3 = csv.df.list2 %>% 
  lapply(transform, Participation = as.character(Participation)) %>% 
  lapply(transform, GainRecorder = as.character(GainRecorder)) %>% 
  lapply(transform, TypeStudy = as.character(TypeStudy)) %>% 
  lapply(transform, Recorder = as.character(Recorder)) %>% 
  lapply(transform, Mic = as.character(Mic)) %>% 
  lapply(transform, HPF = as.character(HPF)) %>% 
  lapply(transform, TriggerLevel = as.character(TriggerLevel)) %>% 
  lapply(transform, TrigWin = as.character(TrigWin)) %>% 
  lapply(transform, Pause = as.character(Pause)) %>% 
  lapply(transform, FreqMin = as.numeric(FreqMin)) %>% 
  lapply(transform, RecorderOther = as.numeric(RecorderOther)) %>% 
  lapply(transform, MinDur = as.numeric(MinDur)) %>% 
  lapply(transform, MaxDur = as.numeric(MaxDur)) %>% 
  lapply(transform, TrigWinMax = as.numeric(TrigWinMax)) %>% 
  lapply(transform, FileSplittingLength = as.numeric(FileSplittingLength))

csv.df2 = bind_rows(csv.df.list3, .id = "column_label")

if(rowSums(as.data.frame(lapply(csv.df.list3, nrow))) != nrow(csv.df2)){
  print("warning: rbind did not succeed")
}


#### Write table ####
write.csv(csv.df2, "C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Metadonnees/Summary_ASUPPRIMER.csv")

#### Check if Site and participation folders are all at the IN2P3 #### 
Directories = list.dirs("C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Metadonnees/TEST")
Directories_Sites= Directories[which(lengths(regmatches(Directories, gregexpr("/", Directories)))==9)]
Directories_Sites = gsub(".*\\/", "", Directories_Sites)

#### Check if Site and participation folders at the IN2P3 are in the metadata table #### 
# check if there are no other file or folder that should not be there (sometimes there are unwanted subfolders)
# check if folders are not empty


#### Count n nights and n sites ####
N_Nights1 = sum(as.numeric(difftime(csv.df2$EndDate, csv.df2$StartDate, units = "days")), na.rm = T)
N_Nights2 = length(which(as.numeric(difftime(csv.df2$EndDate, csv.df2$StartDate, units = "days"))==0)) # people wrote same date for start and end
N_Nights1 + N_Nights2

N_Sites = nrow(unique(data.frame(csv.df2$X, csv.df2$Y)))
N_Sites

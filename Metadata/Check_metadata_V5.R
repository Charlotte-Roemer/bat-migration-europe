library(data.table)
library(tidyverse)
library(lubridate)
library(sf)
library(ggplot2)
library(StreamMetabolism)
library(lutz)


setwd("/home/charlotte/Documents/Post-Doc/MIGRATION/Metadonnees/Return_from_partners_FINAL")

# List and load metadata files ####
csv.list <- list.files("/home/charlotte/Documents/Post-Doc/MIGRATION/Metadonnees/Return_from_partners_FINAL",
                       pattern='*Metadata_table.csv', recursive = TRUE, full.names = T)

csv.df.list <- lapply(csv.list, fread, na.strings=c("-","NA", "N/A", "NULL","","/")) 

# /!\ some tables have a weird behaviour (e.g. when asking head(table Arnold Andreasson) after formatting time)
# This is probably due to the fread function
# There was no problem until the step of rbind
# The workaround is to do as.data.frame() for all tables

##check if the list is empty or filled with csv 
print(length(csv.df.list))

#### To know: FamilyNameTop = the Name on the top of the table and FamilyName = real familyname of the data's author 
#### FamilyName is used for the summary of missing information 


# Delete empty lines ####
for (i in 1:length(csv.df.list)){
  Break1 <- FALSE
  for(j in 1:nrow(csv.df.list[[i]])){ 
    df <- csv.df.list[[i]]
    csv.df.list[[i]] <- df[!apply(df, 1, function(row) all(is.na(row)| all(row == ""))), ]
    if(Break1 == FALSE){
      Break1<-TRUE
    }
  }
}  

# Check if number and names of columns are right####
name_col=c("FirstName",	"FamilyName",	"Email",	"Affiliation",	"Country",	"Site",	"X",	"Y",	
           "Participation",	"StartDate",	"EndDate",	"StartTime",	"EndTime", "Sunset", "Sunrise", "UTC", "TypeStudy",	
           "TypeStudyOther",	"PositionNacelle",	"MicHeight",	"Recorder",	"RecorderOther",	
           "Mic",	"MicOther",	"GainRecorder",	"HPF",	"LPF",	"FreqMin",	"FreqMax",	"TriggerLevel",	
           "MinDur",	"MaxDur",	"TrigWin",	"Pause",	"TrigWinMax",	"Quality", "FileSplittingLength",	
           "NoiseFilter",	"Partner",	"Comment", "tz", "Carre")

for (i in 1:length(csv.df.list)) {
  indices <- grepl("\\bsite\\b", colnames(csv.df.list[[i]]), ignore.case = TRUE)
  colnames(csv.df.list[[i]])[indices] <- "Site" #If "Site" is written wrong
}              

for (i in 1:length(csv.df.list)){
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1] #to refer to the document and the participant as all docs are analyzed at the same time
  print(paste0("i = ", i, " ", FamilyNameTop))
  if(ncol(csv.df.list[[i]]) != 41) {
    print(paste0("error: number of columns is incorrect for", FamilyNameTop))
    print(paste0("n columns =  ", ncol(csv.df.list[[i]]) ))
  }
  if (!identical(name_col, colnames(csv.df.list[[i]]))) {
    print(paste0("error: name of columns is incorrect", FamilyNameTop))
    print(paste0("added missing columns =", setdiff(name_col, colnames(csv.df.list[[i]]))))
    nouveaux_noms <- setdiff(name_col, colnames(csv.df.list[[i]]))#created the missing columns 
    for (nouveau_nom in nouveaux_noms) {
      csv.df.list[[i]][[nouveau_nom]] <- NA # fill with NA
    }
    csv.df.list[[i]] <- setcolorder(csv.df.list[[i]],name_col)#put new columns in order
  }
}

# Create a summary for missing information ####
for (i in 1:length(csv.df.list)){ 
  for (j in 1:nrow(csv.df.list[[i]])){
    csv.df.list = csv.df.list %>%
      lapply(transform, FamilyName <- as.character(FamilyName))%>%
      lapply(transform, Affiliation <- as.character(Affiliation))
  }
}

missing_info <- data.frame(
  FamilyName = character(),
  Information = character(),
  stringsAsFactors = FALSE
)
error_messages <- list()
print_and_stocked <- function(FamilyName = NA, Affiliation = NA, value = NA) {
  if (is.na(FamilyName) || FamilyName == "") {
    if (!is.na(Affiliation) && !is.null(Affiliation) && length(Affiliation) > 0) { #Use Affilitaion if FamilyName is missing
      initial_message <- paste("FamilyName is missing, use Affiliation name : ", Affiliation)
      if (!Affiliation %in% names(error_messages)) {
        error_messages[[Affiliation]] <<- character()
      }
      if (!(initial_message %in% error_messages[[Affiliation]])) {
        print(initial_message)
        initial_values_print <- data.frame(FamilyName = Affiliation, Information = initial_message, stringsAsFactors = FALSE)
        missing_info <<- rbind(missing_info, initial_values_print)
        error_messages[[Affiliation]] <<- c(error_messages[[Affiliation]], initial_message)
      }
      FamilyName <- Affiliation 
    } else {
      print("FamilyName and Affiliation are missing, message not stocked.")
      return(NULL)
    }
  }
  if (!FamilyName %in% names(error_messages)) { #Only one message type per FamilyName is stocked
    error_messages[[FamilyName]] <<- character()  
  }
  if (!(value %in% error_messages[[FamilyName]])) {
    print(value)  
    values_print <- data.frame(FamilyName = FamilyName, Information = value, stringsAsFactors = FALSE)
    missing_info <<- rbind(missing_info, values_print)
    error_messages[[FamilyName]] <<- c(error_messages[[FamilyName]], value)
  }
}


#if you need to check errors which are already printed
# for (file in files) {
#   for (error in errors[[file]]) {
#     print_and_stocked(file, error)
#   }
# }

# Function for extracting content between brackets in columns to put it in comment####
for (i in 1:length(csv.df.list)) {
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (!is.na(csv.df.list[[i]]$TypeStudy[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$TypeStudy[j])) {
      print(paste0("warning: brackets in TypeStudy ", FamilyName))
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(TypeStudy) & grepl(".*\\((.*)\\).*", TypeStudy),
                                paste(Comment, paste ("TypeStudy : ", str_extract(TypeStudy, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               TypeStudy = ifelse(!is.na(TypeStudy) & grepl(".*\\((.*)\\).*", TypeStudy),
                                  str_replace(TypeStudy, "\\((.*?)\\)", ""),
                                  TypeStudy))
    }
    if (!is.na(csv.df.list[[i]]$MicHeight[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$MicHeight[j])) {
      print(paste0("warning: brackets in MicHeight "))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(MicHeight) & grepl(".*\\((.*)\\).*", MicHeight),
                                paste(Comment, paste ("MicHeight: ", str_extract(MicHeight, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               MicHeight = ifelse(!is.na(MicHeight) & grepl(".*\\((.*)\\).*", MicHeight),
                                  str_replace(MicHeight, "\\((.*?)\\)", ""),
                                  MicHeight))
    }
    if (!is.na(csv.df.list[[i]]$Recorder[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$Recorder[j])) {
      print(paste0("warning: brackets in Recorder"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(Recorder) & grepl(".*\\((.*)\\).*", Recorder),
                                paste(Comment, paste ("Recorder :", str_extract(Recorder, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               Recorder = ifelse(!is.na(Recorder) & grepl(".*\\((.*)\\).*", Recorder),
                                 str_replace(Recorder, "\\((.*?)\\)", ""),
                                 Recorder))
    }
    if (!is.na(csv.df.list[[i]]$Mic[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$Mic[j])) {
      print(paste0("warning: brackets in Mic"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(Mic) & grepl(".*\\((.*)\\).*", Mic),
                                paste(Comment, paste ("Mic :", str_extract(Mic, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               Mic = ifelse(!is.na(Mic) & grepl(".*\\((.*)\\).*", Mic),
                            str_replace(Mic, "\\((.*?)\\)", ""),
                            Mic))
    }
    if (!is.na(csv.df.list[[i]]$GainRecorder[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$GainRecorder[j])) {
      print(paste0("warning: brackets in GainRecorder"))
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(GainRecorder) & grepl(".*\\((.*)\\).*", GainRecorder),
                                paste(Comment, paste ("GainRecorder : ", str_extract(GainRecorder, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               GainRecorder = ifelse(!is.na(GainRecorder) & grepl(".*\\((.*)\\).*", GainRecorder),
                                     str_replace(GainRecorder, "\\((.*?)\\)", ""),
                                     GainRecorder))
    }
    if (!is.na(csv.df.list[[i]]$HPF[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$HPF[j])) {
      print(paste0("warning: brackets in HPF"))
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(HPF) & grepl(".*\\((.*)\\).*", HPF),
                                paste(Comment, paste ("HPF : ", str_extract(HPF, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               HPF = ifelse(!is.na(HPF) & grepl(".*\\((.*)\\).*", HPF),
                            str_replace(HPF, "\\((.*?)\\)", ""),
                            HPF))
    }
    if (!is.na(csv.df.list[[i]]$LPF[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$LPF[j])) {
      print(paste0("warning: brackets in LPF"))
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(LPF) & grepl(".*\\((.*)\\).*", LPF),
                                paste(Comment, paste ("LPF : ", str_extract(LPF, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               LPF = ifelse(!is.na(LPF) & grepl(".*\\((.*)\\).*", LPF),
                            str_replace(LPF, "\\((.*?)\\)", ""),
                            LPF))
    }
    if (!is.na(csv.df.list[[i]]$FreqMin[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$FreqMin[j])) {
      print(paste0("warning: brackets in FreqMin"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(FreqMin) & grepl(".*\\((.*)\\).*", FreqMin),
                                paste(Comment, paste ("FreqMin :", str_extract(FreqMin, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               FreqMin = ifelse(!is.na(FreqMin) & grepl(".*\\((.*)\\).*", FreqMin),
                                str_replace(FreqMin, "\\((.*?)\\)", ""),
                                FreqMin))
    }
    if (!is.na(csv.df.list[[i]]$FreqMax[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$FreqMax[j])) {
      print(paste0("warning: brackets in FreqMax"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(FreqMax) & grepl(".*\\((.*)\\).*", FreqMax),
                                paste(Comment, paste ("FreqMax :", str_extract(FreqMax, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               FreqMax = ifelse(!is.na(FreqMax) & grepl(".*\\((.*)\\).*", FreqMax),
                                str_replace(FreqMax, "\\((.*?)\\)", ""),
                                FreqMax))
    }
    if (!is.na(csv.df.list[[i]]$TriggerLevel[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$TriggerLevel[j])) {
      print(paste0("warning: brackets in TriggerLevel"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(TriggerLevel) & grepl(".*\\((.*)\\).*", TriggerLevel),
                                paste(Comment, paste ("TriggerLevel :", str_extract(TriggerLevel, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               TriggerLevel = ifelse(!is.na(TriggerLevel) & grepl(".*\\((.*)\\).*", TriggerLevel),
                                     str_replace(TriggerLevel, "\\((.*?)\\)", ""),
                                     TriggerLevel))
    }
    if (!is.na(csv.df.list[[i]]$MinDur[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$MinDur[j])) {
      print(paste0("warning: brackets in MinDur"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(MinDur) & grepl(".*\\((.*)\\).*", MinDur),
                                paste(Comment, paste ("MinDur : ", str_extract(MinDur, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               MinDur = ifelse(!is.na(MinDur) & grepl(".*\\((.*)\\).*", MinDur),
                               str_replace(MinDur, "\\((.*?)\\)", ""),
                               MinDur))
    }
    if (!is.na(csv.df.list[[i]]$MaxDur[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$MaxDur[j])) {
      print(paste0("warning: brackets in MaxDur"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(MaxDur) & grepl(".*\\((.*)\\).*", MaxDur),
                                paste(Comment, paste ("MaxDur : ", str_extract(MaxDur, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               MaxDur = ifelse(!is.na(MaxDur) & grepl(".*\\((.*)\\).*", MaxDur),
                               str_replace(MaxDur, "\\((.*?)\\)", ""),
                               MaxDur))
    }
    if (!is.na(csv.df.list[[i]]$TrigWin[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$TrigWin[j])) {
      print(paste0("warning: brackets in TrigWin"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(TrigWin) & grepl(".*\\((.*)\\).*", TrigWin),
                                paste(Comment, paste ("TrigWin : ", str_extract(TrigWin, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               TrigWin = ifelse(!is.na(TrigWin) & grepl(".*\\((.*)\\).*", TrigWin),
                                str_replace(TrigWin, "\\((.*?)\\)", ""),
                                TrigWin))
    }
    if (!is.na(csv.df.list[[i]]$Pause[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$Pause[j])) {
      print(paste0("warning: brackets in Pause"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(Pause) & grepl(".*\\((.*)\\).*", Pause),
                                paste(Comment, paste ("Pause : ", str_extract(Pause, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               Pause = ifelse(!is.na(Pause) & grepl(".*\\((.*)\\).*", Pause),
                              str_replace(Pause, "\\((.*?)\\)", ""),
                              Pause))
    }
    if (!is.na(csv.df.list[[i]]$TrigWinMax[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$TrigWinMax[j])) {
      print(paste0("warning: brackets in TrigWinMax"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(TrigWinMax) & grepl(".*\\((.*)\\).*", TrigWinMax),
                                paste(Comment, paste ("TrigWinMax : ", str_extract(TrigWinMax, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               TrigWinMax = ifelse(!is.na(TrigWinMax) & grepl(".*\\((.*)\\).*", TrigWinMax),
                                   str_replace(TrigWinMax, "\\((.*?)\\)", ""),
                                   TrigWinMax))
    }
    if (!is.na(csv.df.list[[i]]$FileSplittingLength[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$FileSplittingLength[j])) {
      print(paste0("warning: brackets in FileSplittingLength"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(FileSplittingLength) & grepl(".*\\((.*)\\).*", FileSplittingLength),
                                paste(Comment, paste ("FileSplittingLength : ", str_extract(FileSplittingLength, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               FileSplittingLength = ifelse(!is.na(FileSplittingLength) & grepl(".*\\((.*)\\).*", FileSplittingLength),
                                            str_replace(FileSplittingLength, "\\((.*?)\\)", ""),
                                            FileSplittingLength))
    }
    if (!is.na(csv.df.list[[i]]$NoiseFilter[j]) & grepl(".*\\((.*)\\).*", csv.df.list[[i]]$NoiseFilter[j])) {
      print(paste0("warning: brackets in NoiseFilter"))
      
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = ifelse(!is.na(NoiseFilter) & grepl(".*\\((.*)\\).*", NoiseFilter),
                                paste(Comment, paste ("NoiseFilter : ", str_extract(NoiseFilter, "\\((.*?)\\)")), sep = ", "),
                                Comment),
               NoiseFilter = ifelse(!is.na(NoiseFilter) & grepl(".*\\((.*)\\).*", NoiseFilter),
                                    str_replace(NoiseFilter, "\\((.*?)\\)", ""),
                                    NoiseFilter))
    }
  }
}

# Comma into dot ####
for (i in 1:length(csv.df.list)) {
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (!is.na(csv.df.list[[i]]$X[j]) & grepl(",", csv.df.list[[i]]$X[j])) {
      print(paste0("warning: comma instead of period "))
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(X = ifelse(!is.na(X) & grepl(",", X),
                          gsub(",",".",X)))
    }
    if (!is.na(csv.df.list[[i]]$Y[j]) & grepl(",", csv.df.list[[i]]$Y[j])) {
      print(paste0("warning: comma instead of period "))
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Y = ifelse(!is.na(Y) & grepl(",", Y),
                          gsub(",",".",Y)))
    }
  }
}

for (i in 1:length(csv.df.list)) {
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (!is.numeric(csv.df.list[[i]]$X[j])) {
      print(paste0("warning: format change for X "))
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(X = as.numeric(X))
    }
    if (!is.numeric(csv.df.list[[i]]$Y[j])) {
      print(paste0("warning: Format change for Y "))
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Y = as.numeric(Y))
    }
    if (is.integer(csv.df.list[[i]]$RecorderOther[j])) {
      print(paste0("warning: Code instead of character "))
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(
          Comment = ifelse(!is.na(RecorderOther) & is.integer(RecorderOther),
                           paste(Comment, "RecorderOther : ", RecorderOther),
                           Comment),
          RecorderOther = ifelse(!is.na(RecorderOther) & is.integer(RecorderOther),
                                 "NA",
                                 RecorderOther)
        )
    }
  }
}

# Check column format ####

StandardFormat = c(rep("character",6), rep("numeric", 2), rep("character", 8),
                   rep("numeric", 2), "character", rep("numeric", 2), "character",
                   "integer", "character", rep("numeric", 12), rep("character",3))

for (i in 1:length(csv.df.list)){ 
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
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

csv.df.list = csv.df.list %>% 
  map(~ .x %>% 
        mutate(MicHeight = gsub("[^0-9.-]", "", MicHeight))) %>% #deletes the contents of cells that are not numbers, hyphens or dots 
  map(~ .x %>% 
        mutate(HPF = gsub("[^0-9.-]", "", HPF))) %>%
  map(~ .x %>% 
        mutate(LPF = gsub("[^0-9.-]", "", LPF))) %>%
  map(~ .x %>% 
        mutate(FreqMin = gsub("[^0-9.-]", "", FreqMin))) %>%
  map(~ .x %>% 
        mutate(FreqMax = gsub("[^0-9.-]", "", FreqMax))) %>%
  map(~ .x %>% 
        mutate(TriggerLevel = gsub("[^0-9.-]", "", TriggerLevel))) %>%
  map(~ .x %>% 
        mutate(MinDur = gsub("[^0-9.-]", "", MinDur))) %>%
  map(~ .x %>% 
        mutate(MaxDur = gsub("[^0-9.-]", "", MaxDur))) %>%
  map(~ .x %>% 
        mutate(TrigWin = gsub("[^0-9.-]", "", TrigWin))) %>%
  map(~ .x %>% 
        mutate(Pause = gsub("[^0-9.-]", "", Pause))) %>%
  map(~ .x %>% 
        mutate(TrigWinMax = gsub("[^0-9.-]", "", TrigWinMax))) %>%
  map(~ .x %>% 
        mutate(FileSplittingLength = gsub("[^0-9.-]", "", FileSplittingLength))) %>%
  lapply(transform, MicHeight = as.numeric(MicHeight)) %>% 
  lapply(transform, LPF = as.numeric(LPF)) %>% 
  lapply(transform, HPF = as.numeric(HPF)) %>% 
  lapply(transform, Participation = as.character(Participation)) %>% 
  lapply(transform, GainRecorder = as.character(GainRecorder)) %>% # because of Audiomoths
  lapply(transform, FreqMax = as.numeric(FreqMax))%>%
  lapply(transform, FreqMin = as.numeric(FreqMin))%>%
  lapply(transform, TriggerLevel = as.numeric(TriggerLevel))%>%
  lapply(transform, MinDur = as.numeric(MinDur))%>%
  lapply(transform, MaxDur = as.numeric(MaxDur))%>%
  lapply(transform, TrigWin = as.numeric(TrigWin))%>%
  lapply(transform, Pause = as.numeric(Pause))%>%
  lapply(transform, TrigWinMax = as.numeric(TrigWinMax))%>%
  lapply(transform, FileSplittingLength = as.numeric(FileSplittingLength))



# Check that required columns are not empty ####

Required_columns = c("FirstName",	"FamilyName",	"Email",	"Affiliation",	"Country",	"Site",	"X",	"Y",	
                     "Participation",	"StartDate",	"EndDate",	"StartTime",	"EndTime",	"TypeStudy",	
                     "Recorder",	"GainRecorder",	"HPF",	"LPF",	"FreqMin",	"FreqMax",	"TriggerLevel",	
                     "MinDur",	"MaxDur",	"TrigWin",	"Pause", "NoiseFilter")


for (i in 1:length(csv.df.list)){
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  PositionCol = match(Required_columns, names(csv.df.list[[i]]))
  for (j in 1:length(PositionCol)){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(all(is.na((csv.df.list[[i]][,..PositionCol])))){
      print_and_stocked(FamilyName, Affiliation, paste0("warning: required column ", Required_columns[j], " is empty"))
    }
  }
}


# Check coordinates #### 



for (i in 1:length(csv.df.list)){ # Y coordinates should be greater values than X (only first line is checked)
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  Break2 <- FALSE
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(!is.na(csv.df.list[[i]]$Y[j]) && !is.na(csv.df.list[[i]]$Y[j])){
      if(csv.df.list[[i]]$Y[j]<csv.df.list[[i]]$X[j]){
        print(paste0("warning: X coord greater than Y coord",FamilyName ))
        print("thus I am inversing the column values")
        YTemp = csv.df.list[[i]]$X[j]
        XTemp = csv.df.list[[i]]$Y[j]
        csv.df.list[[i]]$X[j] = XTemp
        csv.df.list[[i]]$Y[j] = YTemp
      }
    }
  }
  for(j in 1:nrow(csv.df.list[[i]]))
    if(is.na(csv.df.list[[i]]$Y[j])){
      print_and_stocked(FamilyName, Affiliation, paste0("warning: missing Y coordinates "))
    }
  if(is.na(csv.df.list[[i]]$X[j])){
    print_and_stocked(FamilyName, Affiliation, paste0("warning: missing X coordinates "))
  }
}

# for (i in 1:length(csv.df.list)){
#   for (j in 1:nrow(df)) {
#     if (!is.na(csv.df.list[[i]]$Y[j]) & !is.na(csv.df.list[[i]]$X[j])){
#       csv.df.list[[i]]$timezone[j] <- NA
#       lat <- as.numeric(csv.df.list[[i]]$Y[j])
#       lon <- as.numeric(csv.df.list[[i]]$X[j])
#       timezone <- tz_lookup_coords(lat = lat, lon = lon, method = "fast")
#       csv.df.list[[i]]$timezone[j] <- timezone
#     }
#   }
# }#warning: give not all timezone in the same table > advice to use accurate but longer

# # Accurate but too long-lasting function
# csv.df.list <- lapply(csv.df.list, function(df) {
#   cached_timezones <- list()#avoids repeated searches for the same coordinates
#   for (j in 1:nrow(df)) {
#     if (!is.na(df$Y[j]) & !is.na(df$X[j])) {
#       coord_key <- paste(df$Y[j], df$X[j], sep = "_")
#       if (coord_key %in% names(cached_timezones)) {
#         timezone <- cached_timezones[[coord_key]]
#       } else {
#         lat <- as.numeric(df$Y[j])
#         lon <- as.numeric(df$X[j])
#         timezone <- tz_lookup_coords(lat = lat, lon = lon, method = "accurate")
#         cached_timezones[[coord_key]] <- timezone
#       }
#         df$timezone[j] <- timezone
#     }
#   }
#   df
# })


# Make DATE format ####

# Add 0 in front of 3-9 Month
for (i in 1:length(csv.df.list)) {
  FamilyNameTop <- csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (!is.na(csv.df.list[[i]]$StartDate[j]) & grepl("[\\./]", csv.df.list[[i]]$StartDate[j])
        & nchar(gsub("[\\./].*", "", csv.df.list[[i]]$StartDate[j])) == 2) {
      if (grepl("^\\d{2}[\\./]([3-9])[\\./]\\d{4}$", csv.df.list[[i]]$StartDate[j])) {
        csv.df.list[[i]]$StartDate[j] <- gsub("^(\\d{2}[\\./])([3-9])([\\./]\\d{4})$", "\\1\\0\\2\\3", csv.df.list[[i]]$StartDate[j])
      }
    }
    if (!is.na(csv.df.list[[i]]$EndDate[j]) & grepl("[\\./]", csv.df.list[[i]]$EndDate[j])
        & nchar(gsub("[\\./].*", "", csv.df.list[[i]]$EndDate[j])) == 2) {
      if (grepl("^\\d{2}[\\./]([3-9])[\\./]\\d{4}$", csv.df.list[[i]]$EndDate[j])) {
        csv.df.list[[i]]$EndDate[j] <- gsub("^(\\d{2}[\\./])([3-9])([\\./]\\d{4})$", "\\1\\0\\2\\3", csv.df.list[[i]]$EndDate[j])
      }
    }
    if (!is.na(csv.df.list[[i]]$StartDate[j]) & grepl("[\\./]", csv.df.list[[i]]$StartDate[j])
        & nchar(gsub("[\\./].*", "", csv.df.list[[i]]$StartDate[j])) == 4) {
      if (grepl("^\\d{4}[\\./]([3-9])[\\./]\\d{2}$", csv.df.list[[i]]$StartDate[j])) {
        csv.df.list[[i]]$StartDate[j] <- gsub("^(\\d{4}[\\./])([3-9])([\\./]\\d{2})$", "\\1\\0\\2\\3", csv.df.list[[i]]$StartDate[j])
      }
    }
    if (!is.na(csv.df.list[[i]]$EndDate[j]) & grepl("[\\./]", csv.df.list[[i]]$EndDate[j])
        & nchar(gsub("[\\./].*", "", csv.df.list[[i]]$EndDate[j])) == 4) {
      if (grepl("^\\d{4}[\\./]([3-9])[\\./]\\d{2}$", csv.df.list[[i]]$EndDate[j])) {
        csv.df.list[[i]]$EndDate[j] <- gsub("^(\\d{4}[\\./])([3-9])([\\./]\\d{2})$", "\\1\\0\\2\\3", csv.df.list[[i]]$EndDate[j])
      }
    }
  }
}

print(csv.df.list[[1]])

## Format dmy: 
# Valid Date
is_valid_date <- function(date_string) {
  parsed_date <- tryCatch({
    as.Date(date_string, format = "%d.%m.%Y")
  }, error = function(e) {
    NA
  })
  if (!is.na(parsed_date) && format(parsed_date, "%d.%m.%Y") == date_string) {
    return(TRUE)
  }
  
  parsed_date <- tryCatch({
    as.Date(date_string, format = "%d/%m/%Y")
  }, error = function(e) {
    NA
  })
  !is.na(parsed_date) && format(parsed_date, "%d/%m/%Y") == date_string
}
for (i in 1:length(csv.df.list)) {
  FamilyNameTop <- csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for (j in 1:nrow(csv.df.list[[i]])) {
    FamilyName <- csv.df.list[[i]]$FamilyName[j]
    Affiliation <- csv.df.list[[i]]$Affiliation[j]
    if (!is.na(csv.df.list[[i]]$StartDate[j]) & grepl("[\\./]", csv.df.list[[i]]$StartDate[j]) 
        & nchar(gsub("[\\./].*", "", csv.df.list[[i]]$StartDate[j])) == 2) {
      if ((grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", csv.df.list[[i]]$StartDate[j]) | grepl("^\\d{2}/\\d{2}/\\d{4}$", csv.df.list[[i]]$StartDate[j]))
          & is_valid_date(csv.df.list[[i]]$StartDate[j])) {
        csv.df.list[[i]]$StartDate[j] <- as.character(dmy(csv.df.list[[i]]$StartDate[j]))
      } else {
        print_and_stocked(FamilyName, Affiliation, paste0("Failed to parse StartDate: ", csv.df.list[[i]]$StartDate[j]))
      }
    }
    if (!is.na(csv.df.list[[i]]$EndDate[j]) & grepl("[\\./]", csv.df.list[[i]]$EndDate[j]) 
        & nchar(gsub("[\\./].*", "", csv.df.list[[i]]$EndDate[j])) == 2) {
      if ((grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", csv.df.list[[i]]$EndDate[j]) | grepl("^\\d{2}/\\d{2}/\\d{4}$", csv.df.list[[i]]$EndDate[j]))
          & is_valid_date(csv.df.list[[i]]$EndDate[j])) {
        csv.df.list[[i]]$EndDate[j] <- as.character(dmy(csv.df.list[[i]]$EndDate[j]))
      } else {
        print_and_stocked(FamilyName, Affiliation, paste0("Failed to parse EndDate: ", csv.df.list[[i]]$EndDate[j]))
      }
    }
  }
}

## Format ymd: 
# Valid Date
is_valid_date2 <- function(date_string) {
  parsed_date <- tryCatch({
    as.Date(date_string, format = "%Y.%m.%d")
  }, error = function(e) {
    NA
  })
  if (!is.na(parsed_date) && format(parsed_date, "%Y.%m.%d") == date_string) {
    return(TRUE)
  }
  
  parsed_date <- tryCatch({
    as.Date(date_string, format = "%Y/%m/%d")
  }, error = function(e) {
    NA
  })
  !is.na(parsed_date) && format(parsed_date, "%Y/%m/%d") == date_string
}

for (i in 1:length(csv.df.list)) {
  FamilyNameTop <- csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for (j in 1:nrow(csv.df.list[[i]])) {
    FamilyName <- csv.df.list[[i]]$FamilyName[j]
    Affiliation <- csv.df.list[[i]]$Affiliation[j]
    if (!is.na(csv.df.list[[i]]$StartDate[j]) & grepl("[\\./]", csv.df.list[[i]]$StartDate[j]) 
        & nchar(gsub("[\\./].*", "", csv.df.list[[i]]$StartDate[j])) == 4) {
      if ((grepl("^\\d{4}\\.\\d{2}\\.\\d{2}$", csv.df.list[[i]]$StartDate[j]) | grepl("^\\d{4}/\\d{2}/\\d{2}$", csv.df.list[[i]]$StartDate[j]))
          & is_valid_date2(csv.df.list[[i]]$StartDate[j])) {
        csv.df.list[[i]]$StartDate[j] <- as.character(ymd(csv.df.list[[i]]$StartDate[j]))
      } else {
        print_and_stocked(FamilyName, Affiliation, paste0("Failed to parse StartDate: ", csv.df.list[[i]]$StartDate[j]))
      }
    }
    if (!is.na(csv.df.list[[i]]$EndDate[j]) & grepl("[\\./]", csv.df.list[[i]]$EndDate[j]) 
        & nchar(gsub("[\\./].*", "", csv.df.list[[i]]$EndDate[j])) == 4) {
      if ((grepl("^\\d{4}\\.\\d{2}\\.\\d{2}$", csv.df.list[[i]]$EndDate[j]) | grepl("^\\d{4}/\\d{2}/\\d{2}$", csv.df.list[[i]]$EndDate[j]))
          & is_valid_date2(csv.df.list[[i]]$EndDate[j])) {
        csv.df.list[[i]]$EndDate[j] <- as.character(ymd(csv.df.list[[i]]$EndDate[j]))
      } else {
        print_and_stocked(FamilyName, Affiliation, paste0("Failed to parse EndDate: ", csv.df.list[[i]]$EndDate[j]))
      }
    }
  }
}

csv.df.list = lapply(csv.df.list,function(x) { # /!\ RUN ONLY ONCE
  x$StartDate <- as.Date( x$StartDate, format="%Y-%m-%d");
  x})
csv.df.list = lapply(csv.df.list,function(x) { # /!\ RUN ONLY ONCE
  x$EndDate <- as.Date( x$EndDate, format="%Y-%m-%d");
  x})

for(i in 1:length(csv.df.list)){ # Check that date columns are not empty
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(is.na(csv.df.list[[i]]$StartDate[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column StartDate is empty"))
    }
    if(is.na(csv.df.list[[i]]$EndDate[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column EndDate is empty"))
    }
  }
}

# Make TIME format #### 

# Fonction pour vérifier la validité de l'heure
is_valid_time <- function(time_str) {
  # Vérifier si l'heure est au format HH:MM
  valid_format1 <- grepl("^\\d{2}:\\d{2}$", time_str)
  
  # Vérifier si l'heure est au format avec AM/PM
  valid_format2 <- grepl("^\\d{2}:\\d{2}\\s*(AM|PM|am|pm)$", time_str)
  
  return(valid_format1 || valid_format2)
}

for (i in 1:length(csv.df.list)) {
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (!is.na(csv.df.list[[i]]$StartTime[j]) & is_valid_time(csv.df.list[[i]]$StartTime[j])) {
      if (str_detect(csv.df.list[[i]]$StartTime[j], regex("AM|PM", ignore_case = TRUE))){
        csv.df.list[[i]]$StartTime[j] <-  parse_date_time(csv.df.list[[i]]$StartTime[j], orders = "I:M:S p") %>% format("%H:%M:%S")
      }
      if(str_count(csv.df.list[[i]]$StartTime[j], ":")==1){
        csv.df.list[[i]]$StartTime[j] <-parse_date_time(csv.df.list[[i]]$StartTime[j], orders = "H:M") %>% format("%H:%M:%S")
      }
    }
    if (!is.na(csv.df.list[[i]]$EndTime[j]) & is_valid_time(csv.df.list[[i]]$EndTime[j])) {
      if(str_detect(csv.df.list[[i]]$EndTime[j], regex("AM|PM", ignore_case = TRUE))){
        csv.df.list[[i]]$EndTime[j] <-  parse_date_time(csv.df.list[[i]]$EndTime[j], orders = "I:M:S p") %>% format("%H:%M:%S")
      } 
      if(str_count(csv.df.list[[i]]$EndTime[j], ":")==1){
      csv.df.list[[i]]$EndTime[j] <- parse_date_time(csv.df.list[[i]]$EndTime[j], orders = "H:M") %>% format("%H:%M:%S")
      }
    }
  }
}

####convert time into each format#


#replace sunset/sunrise term in the right column
for (i in 1:length(csv.df.list)) {
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (!is.na(csv.df.list[[i]]$StartTime[j]) && grepl("sunset", csv.df.list[[i]]$StartTime[j], ignore.case = TRUE)) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Sunset = ifelse(!is.na(StartTime) & grepl("sunset", StartTime, ignore.case = TRUE),
                               str_extract(StartTime, ".*sunset.*"),
                               Sunset),
               StartTime = ifelse(!is.na(StartTime) & grepl("sunset", StartTime, ignore.case = TRUE),
                                  NA,
                                  StartTime))
    }
    
    if (!is.na(csv.df.list[[i]]$EndTime[j]) && grepl("sunrise", csv.df.list[[i]]$EndTime[j], ignore.case = TRUE)) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Sunrise = ifelse(!is.na(EndTime) & grepl("sunrise", EndTime, ignore.case = TRUE),
                                str_extract(EndTime, ".*sunrise.*"),
                                Sunrise),
               EndTime = ifelse(!is.na(EndTime) & grepl("sunrise", EndTime, ignore.case = TRUE),
                                NA,
                                EndTime))
    }
  }
}

#Search Time in Comment :

for (i in 1:length(csv.df.list)) {
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (is.na(csv.df.list[[i]]$StartTime[j]) & is.na(csv.df.list[[i]]$Sunset[j]) & !is.na(csv.df.list[[i]]$Comment[j]) && grepl("sunset", csv.df.list[[i]]$Comment[j], ignore.case = TRUE)) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Sunset = ifelse(!is.na(Comment) & grepl("sunset", Comment, ignore.case = TRUE),
                               str_extract(Comment, "(?<=,|^)([^,]*sunset[^,]*)(?=,|$)"), # extract between comma even if there are other information
                               Sunset))
      if (!is.na(csv.df.list[[i]]$Sunset[j]) & grepl("sunset", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE) & grepl("sunrise", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE)) {
        chiffre <- "\\d+'?"
        chiffre1<- chiffre[1]
        place <- "before|after|\\+|-"  
        place1 <- place[1]
        time <- "sunset|surise"
        time1<- time[1]
        csv.df.list[[i]]$Sunset[j] <- if_else(!is.na(csv.df.list[[i]]$Sunset[j]) & grepl("sunset", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE) 
                                               & grepl("sunrise", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE),
                                               paste0(str_extract(csv.df.list[[i]]$Sunset[j], chiffre1), " ", str_extract(csv.df.list[[i]]$Sunset[j], place1),
                                                      " ", str_extract(csv.df.list[[i]]$Sunset[j], time1)),
                                               csv.df.list[[i]]$Sunset[j])
      }
    }
    if (is.na(csv.df.list[[i]]$EndTime[j]) & is.na(csv.df.list[[i]]$Sunrise[j]) & !is.na(csv.df.list[[i]]$Comment[j]) && grepl("sunrise", csv.df.list[[i]]$Comment[j], ignore.case = TRUE)) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Sunrise = ifelse(!is.na(Comment) & grepl("sunrise", Comment, ignore.case = TRUE),
                                str_extract(Comment, "(?<=,|^)([^,]*sunrise[^,]*)(?=,|$)"),
                                Sunrise))
      if (!is.na(csv.df.list[[i]]$Sunrise[j]) & grepl("sunset", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE) & grepl("sunrise", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE)) {
        chiffre <- "\\d+'?"
        place <- "before|after|\\+|-"
        time <- "sunset|sunrise"
        times <- str_extract_all(csv.df.list[[i]]$Sunrise[j], time)[[1]]#select the second part of the information
        if (length(times) >= 2) {
          chiffre_sel <- str_extract_all(csv.df.list[[i]]$Sunrise[j], chiffre)[[1]][2]
          place_sel <- str_extract_all(csv.df.list[[i]]$Sunrise[j], place)[[1]][2]
          time_sel <- times[2]
          csv.df.list[[i]]$Sunrise[j] <- if_else(!is.na(csv.df.list[[i]]$Sunrise[j]) & grepl("sunset", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE) & grepl("sunrise", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE),
                                                 paste0(chiffre_sel, " ", place_sel, " ", time_sel),
                                                 csv.df.list[[i]]$Sunrise[j])
        }
      }
    }
  } 
}



# change in the format sunset/sunrise+/-x
for (i in 1:length(csv.df.list)) {
  for (j in 1:nrow(csv.df.list[[i]])) {
    if(grepl("sunset|sunset", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE) 
       & grepl("before|after", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE) & 
       grepl("\\d+", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE)) {
      minutes1 <- str_extract(csv.df.list[[i]]$Sunset[j], "\\d+'?") %>% str_replace("'", "")
      position1 <- str_extract(csv.df.list[[i]]$Sunset[j], "before|after")
      csv.df.list[[i]]$Sunset[j] <- if_else(position1 == "before", paste0("sunset-", minutes1, sep=""),
                                            paste0("sunset+", minutes1, sep=""))
    }
    if(grepl("sunrise|sunset", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE) 
       &grepl("before|after", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE) & 
       grepl("\\d+", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE)) {
      minutes2 <- str_extract(csv.df.list[[i]]$Sunrise[j], "\\d+'?") %>% str_replace("'", "")
      position2 <- str_extract(csv.df.list[[i]]$Sunrise[j], "after|before")
      csv.df.list[[i]]$Sunrise[j] <- if_else(position2 == "after", paste0("sunrise+", minutes2, sep=""),
                                             paste0("sunrise-", minutes2, sep=""))
    }
  }
}

for (i in 1:length(csv.df.list)) {
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (!is.na(csv.df.list[[i]]$Sunrise[j]) && grepl("sunrise\\s*([+-])\\s*(\\d+).*", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunrise[j] <- gsub("sunrise\\s*([+-])\\s*(\\d+).*", "sunrise\\1\\2", csv.df.list[[i]]$Sunrise[j])
    }
    if (!is.na(csv.df.list[[i]]$Sunset[j]) && grepl("sunset\\s*([+-])\\s*(\\d+).*", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunset[j] <- gsub("sunset\\s*([+-])\\s*(\\d+).*", "sunset\\1\\2", csv.df.list[[i]]$Sunset[j])
    }
    if (!is.na(csv.df.list[[i]]$Sunrise[j]) && grepl("\\s*(\\d+)\\s*([+-])\\s*sunrise", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunrise[j] <- gsub("\\s*(\\d+)\\s*([+-])\\s*sunrise", "sunrise\\2\\1", csv.df.list[[i]]$Sunrise[j])
    }
    if (!is.na(csv.df.list[[i]]$Sunset[j]) && grepl("\\s*(\\d+)\\s*([+-])\\s*sunset", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunset[j] <- gsub("\\s*(\\d+)\\s*([+-])\\s*sunset", "sunset\\2\\1", csv.df.list[[i]]$Sunset[j])
    }
  }
}


for (i in 1:length(csv.df.list)) {
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (!is.na(csv.df.list[[i]]$Sunrise[j])) {
      if (grepl("\\b(sunrise|at\\ssunrise|At\\ssunrise)\\b", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE) &&
          !grepl("\\b(sunrise|at\\ssunrise|At\\ssunrise)\\b[\\s\\+\\-]", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE)) {
        csv.df.list[[i]]$Sunrise[j] <- gsub("\\b(sunrise|at\\ssunrise|At\\ssunrise)\\b", "sunrise+0", csv.df.list[[i]]$Sunrise[j])
      }
    }
    if (!is.na(csv.df.list[[i]]$Sunset[j])) {
      if (grepl("\\b(sunset|at\\ssunset|At\\ssunset)\\b", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE) &&
          !grepl("\\b(sunset|at\\ssunset|At\\ssunset)\\b[\\s\\+\\-]", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE)) {
        csv.df.list[[i]]$Sunset[j] <- gsub("\\b(sunset|at\\ssunset|At\\ssunset)\\b", "sunset+0", csv.df.list[[i]]$Sunset[j])
      }
    }
  }
}

## If other term : 

for (i in 1:length(csv.df.list)) {#vigie chiro settings
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (is.na(csv.df.list[[i]]$StartTime[j]) & is.na(csv.df.list[[i]]$Sunset[j]) & !is.na(csv.df.list[[i]]$Comment[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(StartTime = ifelse(!is.na(Comment) & grepl("(^|,)\\s*(vigie-chiro\\s*settings|Vigie-chiro\\s*settings|Vigie-Chiro\\s*settings)\\s*(,|$)", Comment, ignore.case = TRUE),
                                  str_extract(Comment, "\\b(vigie-chiro\\s*settings|Vigie-chiro\\s*settings|Vigie-Chiro\\s*settings)\\b"), 
                                  StartTime))
    }
    if (is.na(csv.df.list[[i]]$EndTime[j]) & is.na(csv.df.list[[i]]$Sunrise[j]) & !is.na(csv.df.list[[i]]$Comment[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(EndTime = ifelse(!is.na(Comment) & grepl("(^|,)\\s*(vigie-chiro\\s*settings|Vigie-chiro\\s*settings|Vigie-Chiro\\s*settings)\\s*(,|$)", Comment, ignore.case = TRUE),
                                  str_extract(Comment, "\\b(vigie-chiro\\s*settings|Vigie-chiro\\s*settings|Vigie-Chiro\\s*settings)\\b"), 
                                  EndTime))
    }
  }
}

for (i in 1:length(csv.df.list)) {
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (is.na(csv.df.list[[i]]$EndTime[j]) & !is.na(csv.df.list[[i]]$StartTime[j]) & is.na(csv.df.list[[i]]$Sunrise[j]) &
        grepl("\\b(vigie-chiro\\s*settings|Vigie-Chiro\\s*settings|Vigie-chiro\\s*settings)\\b", csv.df.list[[i]]$StartTime[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunrise[j] <- "sunrise+30"
    }
    if (!is.na(csv.df.list[[i]]$StartTime[j]) & is.na(csv.df.list[[i]]$Sunset[j]) &
        grepl("\\b(vigie-chiro\\s*settings|Vigie-Chiro\\s*settings|Vigie-chiro\\s*settings)\\b", csv.df.list[[i]]$StartTime[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunset[j] <- "sunset-30"
      csv.df.list[[i]]$StartTime[j] <- NA
    }
    if (!is.na(csv.df.list[[i]]$EndTime[j]) & is.na(csv.df.list[[i]]$Sunrise[j]) &
        grepl("\\b(vigie-chiro\\s*settings|Vigie-Chiro\\s*settings|Vigie-chiro\\s*settings)\\b", csv.df.list[[i]]$EndTime[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunrise[j] <- "sunrise+30"
      csv.df.list[[i]]$EndTime[j] <- NA
    }
  }
}

for (i in 1:length(csv.df.list)) {
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (is.na(csv.df.list[[i]]$Sunrise[j]) & !is.na(csv.df.list[[i]]$Sunset[j]) &
        grepl("\\b(vigie-chiro\\s*settings|Vigie-Chiro\\s*settings|Vigie-chiro\\s*settings)\\b", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunrise[j] <- "sunrise+30"
    }
    if (!is.na(csv.df.list[[i]]$Sunset[j]) &
        grepl("\\b(vigie-chiro\\s*settings|Vigie-Chiro\\s*settings|Vigie-chiro\\s*settings)\\b", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunset[j] <- "sunset-30"
    }
    if (!is.na(csv.df.list[[i]]$Sunrise[j]) &
        grepl("\\b(vigie-chiro\\s*settings|Vigie-Chiro\\s*settings|Vigie-chiro\\s*settings)\\b", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunrise[j] <- "sunrise+30"
    }
  }
}


for (i in 1:length(csv.df.list)) {#whole night
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (is.na(csv.df.list[[i]]$StartTime[j]) & is.na(csv.df.list[[i]]$Sunset[j]) & !is.na(csv.df.list[[i]]$Comment[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(StartTime = ifelse(!is.na(Comment) & grepl("(^|,)\\s*(whole\\s*night|Whole\\s*night|Whole\\s*Night)\\s*(,|$)", Comment, ignore.case = TRUE),
                                  str_extract(Comment, "\\b(whole\\s*night|Whole\\s*night|Whole\\s*Night)\\b"), 
                                  StartTime))
    }
    if (is.na(csv.df.list[[i]]$EndTime[j]) & is.na(csv.df.list[[i]]$Sunrise[j]) & !is.na(csv.df.list[[i]]$Comment[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(EndTime = ifelse(!is.na(Comment) & grepl("(^|,)\\s*(whole\\s*night|Whole\\s*night|Whole\\s*Night)\\s*(,|$)", Comment, ignore.case = TRUE),
                                str_extract(Comment, "\\b(whole\\s*night|Whole\\s*night|Whole\\s*Night)\\b"), 
                                EndTime))
    }
  }
}

for (i in 1:length(csv.df.list)) {
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (is.na(csv.df.list[[i]]$EndTime[j]) & !is.na(csv.df.list[[i]]$StartTime[j]) & is.na(csv.df.list[[i]]$Sunrise[j]) &
        grepl("\\b(whole\\s*night|Whole\\s*night|Whole\\s*Night)\\b", csv.df.list[[i]]$StartTime[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunrise[j] <- "sunrise+30"
    }
    if (!is.na(csv.df.list[[i]]$StartTime[j]) & is.na(csv.df.list[[i]]$Sunset[j]) &
        grepl("\\b(whole\\s*night|Whole\\s*night|Whole\\s*Night)\\b", csv.df.list[[i]]$StartTime[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunset[j] <- "sunset-30"
      csv.df.list[[i]]$StartTime[j] <- NA
    }
    if (!is.na(csv.df.list[[i]]$EndTime[j]) & is.na(csv.df.list[[i]]$Sunrise[j]) &
        grepl("\\b(whole\\s*night|Whole\\s*night|Whole\\s*Night)\\b", csv.df.list[[i]]$EndTime[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunrise[j] <- "sunrise+30"
      csv.df.list[[i]]$EndTime[j] <- NA
    }
  }
}

for (i in 1:length(csv.df.list)) {
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (is.na(csv.df.list[[i]]$Sunrise[j]) & !is.na(csv.df.list[[i]]$Sunset[j]) &
        grepl("\\b(whole\\s*night|Whole\\s*night|Whole\\s*Night)\\b", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunrise[j] <- "sunrise+30"
    }
    if (!is.na(csv.df.list[[i]]$Sunset[j]) &
        grepl("\\b(whole\\s*night|Whole\\s*night|Whole\\s*Night)\\b", csv.df.list[[i]]$Sunset[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunset[j] <- "sunset-30"
    }
    if (!is.na(csv.df.list[[i]]$Sunrise[j]) &
        grepl("\\b(whole\\s*night|Whole\\s*night|Whole\\s*Night)\\b", csv.df.list[[i]]$Sunrise[j], ignore.case = TRUE)) {
      csv.df.list[[i]]$Sunrise[j] <- "sunrise+30"
    }
  }
}


## convert sunset in hour or hour in sunset > based on Lea Mariton github #

#function to extract sunset and sunrise time depending on the localisation/timzone = local

calc_sunset <- function(latitude, longitude, date) {
  date <- as.Date(date)
  tabsun <- sunrise.set(lat = latitude, long = longitude, date = date, num.days = 1)
  sunset_time <- tabsun$sunset
  return(sunset_time)
}

calc_sunrise <- function(latitude, longitude, date) {
  date <- as.Date(date)
  tabsun <- sunrise.set(lat = latitude, long = longitude, date = date, num.days = 1)
  sunrise_time <- tabsun$sunrise
  return(sunrise_time)
}


for (i in 1:length(csv.df.list)) {
  csv.df.list[[i]]$SunsetTime <- rep(NA, nrow(csv.df.list[[i]]))
  csv.df.list[[i]]$SunriseTime <- rep(NA, nrow(csv.df.list[[i]]))
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (!is.na(csv.df.list[[i]]$Y[j]) & !is.na(csv.df.list[[i]]$X[j]) & !is.na(csv.df.list[[i]]$StartDate[j]) 
        & !is.na(csv.df.list[[i]]$EndDate[j]) & !is.na(csv.df.list[[i]]$timezone[j])) {
      sunset_time <- calc_sunset(csv.df.list[[i]]$Y[j], csv.df.list[[i]]$X[j], csv.df.list[[i]]$StartDate[j])
      sunrise_time <- calc_sunrise(csv.df.list[[i]]$Y[j], csv.df.list[[i]]$X[j], csv.df.list[[i]]$EndDate[j])
      lat <- as.numeric(csv.df.list[[i]]$Y[j])
      lon <- as.numeric(csv.df.list[[i]]$X[j])
      timezone <- as.character(csv.df.list[[i]]$timezone[j])
      if (!is.na(sunset_time)) {
        sunset_time <- as_datetime(as.numeric(sunset_time), tz = timezone)
        csv.df.list[[i]]$SunsetTime[j] <- format(sunset_time, "%H:%M:%S")
      }
      if (!is.na(sunrise_time)) {
        sunrise_time <- as_datetime(as.numeric(sunrise_time), tz = timezone)
        csv.df.list[[i]]$SunriseTime[j] <- format(sunrise_time, "%H:%M:%S")
      }
    }
  }
}



# Create sunset format depending on sunsetTime and StartTime (the same for sunrise) 

for (i in seq_along(csv.df.list)) {
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for (j in 1:nrow(csv.df.list[[i]])) {
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if (!is.na(csv.df.list[[i]]$StartTime[j]) && is.na(csv.df.list[[i]]$Sunset[j]) && !is.na(csv.df.list[[i]]$SunsetTime[j]) 
        && grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$", csv.df.list[[i]]$StartTime[j])) {
      StartTime <- csv.df.list[[i]]$StartTime[j]
      SunsetTime <- csv.df.list[[i]]$SunsetTime[j]
      Time1 <- strptime(SunsetTime, format = "%H:%M:%S")
      Time2 <- strptime(StartTime, format = "%H:%M:%S")
      sunset_diff_secs <- as.numeric(difftime(Time2, Time1, units = "secs"))
      sunset_diff <- round(sunset_diff_secs / 60)
      if (sunset_diff >= 0) {
        sunset_diff <- paste("+", sunset_diff, sep = "")
      }
      csv.df.list[[i]]$Sunset[j] <- as.character(paste0("sunset", sunset_diff, sep = ""))
    }
    if (!is.na(csv.df.list[[i]]$EndTime[j]) && is.na(csv.df.list[[i]]$Sunrise[j]) && !is.na(csv.df.list[[i]]$SunriseTime[j])
        && grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$", csv.df.list[[i]]$EndTime[j])) {
      EndTime <- csv.df.list[[i]]$EndTime[j]
      SunriseTime <- csv.df.list[[i]]$SunriseTime[j]
      Time3 <- strptime(SunriseTime, format = "%H:%M:%S")
      Time4 <- strptime(EndTime, format = "%H:%M:%S")
      sunrise_diff_secs <- as.numeric(difftime(Time4, Time3, units = "secs"))
      sunrise_diff <- round(sunrise_diff_secs / 60)
      if (sunrise_diff >= 0) {
        sunrise_diff <- paste("+", sunrise_diff, sep = "")
      }
      csv.df.list[[i]]$Sunrise[j] <- as.character(paste0("sunrise", sunrise_diff, sep = ""))
    }
    if (!is.na(csv.df.list[[i]]$EndTime[j]) && !str_count(csv.df.list[[i]]$EndTime[j], ":")==2 ) {
      print_and_stocked(FamilyName, Affiliation, paste("Non standard text in EndTime, check "))
    }
    if (!is.na(csv.df.list[[i]]$StartTime[j]) && !str_count(csv.df.list[[i]]$StartTime[j], ":")==2 ) {
      print_and_stocked(FamilyName, Affiliation,paste("Non standard text in StartTime, check"))
    }
  }
}


##Find time depending on sunset/sunrise format :

for (i in seq_along(csv.df.list)) {
  FamilyName <- csv.df.list[[i]]$FamilyName[1]
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (is.na(csv.df.list[[i]]$EndTime[j]) && !is.na(csv.df.list[[i]]$Sunrise[j])) {
      SunriseTime <- csv.df.list[[i]]$SunriseTime[j]
      Sunrise <- csv.df.list[[i]]$Sunrise[j]
      SunriseTime_hms <- strptime(SunriseTime, format = "%H:%M:%S")
      extracted_values <- str_extract(Sunrise, "[+-]\\d+")
      numeric_values <- as.numeric(extracted_values)
      EndTime <- SunriseTime_hms + minutes(numeric_values)
      csv.df.list[[i]]$EndTime[j] <- format(EndTime, "%H:%M:%S")
    }
    if (is.na(csv.df.list[[i]]$StartTime[j]) && !is.na(csv.df.list[[i]]$Sunset[j])) {
      SunsetTime <- csv.df.list[[i]]$SunsetTime[j]
      Sunset <- csv.df.list[[i]]$Sunset[j]
      SunsetTime_hms <- strptime(SunsetTime, format = "%H:%M:%S")
      extracted_values2 <- str_extract(Sunset, "[+-]\\d+")
      numeric_values2 <- as.numeric(extracted_values2)
      StartTime <- SunsetTime_hms + minutes(numeric_values2)
      csv.df.list[[i]]$StartTime[j] <- format(StartTime, "%H:%M:%S")
    }
  }
}


# Obtenez la liste des fuseaux horaires valides
valid_timezones <- OlsonNames()

for (i in seq_along(csv.df.list)) {
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))

  for (j in 1:nrow(csv.df.list[[i]])) {
    if (is.na(csv.df.list[[i]]$UTC[j]) & !is.na(csv.df.list[[i]]$X[j]) & !is.na(csv.df.list[[i]]$Y[j])
        & !is.na(csv.df.list[[i]]$StartDate[j]) & !is.na(csv.df.list[[i]]$StartTime[j])
        & !is.na(csv.df.list[[i]]$EndDate[j]) & !is.na(csv.df.list[[i]]$EndTime[j])) {

      lat <- as.numeric(csv.df.list[[i]]$Y[j])
      lon <- as.numeric(csv.df.list[[i]]$X[j])
      startdate_coord <- as.character(csv.df.list[[i]]$StartDate[j])
      enddate_coord <- as.character(csv.df.list[[i]]$EndDate[j])
      starttime_coord <- as.character(csv.df.list[[i]]$StartTime[j])
      endtime_coord <- as.character(csv.df.list[[i]]$EndTime[j])
      timezone <- as.character(csv.df.list[[i]]$timezone[j])  

      # Vérifiez si le fuseau horaire est valide, sinon utilisez un fuseau horaire par défaut
      if (!(timezone %in% valid_timezones)) {
        print(paste0("Fuseau horaire non valide: ", timezone, ". Utilisation du fuseau horaire par défaut 'UTC'."))
        timezone <- "UTC"
      }

      # Start recording UTC
      starttime_utc <- ymd_hms(paste0(startdate_coord, " ", starttime_coord), tz = "Europe/Paris")
      local_strattime <- with_tz(starttime_utc, tzone = timezone)
      start_utc <- format(local_strattime, "%Z")

      # End recording UTC
      endtime_utc <- ymd_hms(paste0(enddate_coord, " ", endtime_coord), tz = "Europe/Paris")
      local_endtime <- with_tz(endtime_utc, tzone = timezone)
      end_utc <- format(local_endtime, "%Z")

      if (start_utc == end_utc & !is.na(start_utc)) {
        csv.df.list[[i]]$UTC[j] <- start_utc
      } 
      if (start_utc == end_utc & is.na(start_utc)) {
        csv.df.list[[i]]$UTC[j] <- end_utc
      }
      if (start_utc != end_utc & !is.na(start_utc) & !is.na(end_utc)) {
        print("UTC changes between start and end of recording")
        csv.df.list[[i]]$UTC[j] <- paste0(start_utc, "/", end_utc)
      }
    }
  }
}


#Delete SunsetTime and SunriseTime 
for (i in seq_along(csv.df.list)) {
  csv.df.list[[i]] <- subset(csv.df.list[[i]], select = -c(SunsetTime, SunriseTime, 
                                                           timezone, idsite,Point, idparticipation))
}

#Check UTC and time registered
for (i in 1:length(csv.df.list)) {
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for (j in 1:nrow(csv.df.list[[i]])) {
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if (!is.na(csv.df.list[[i]]$Sunrise[j])) {
      Sunrise <- csv.df.list[[i]]$Sunrise[j]
      extracted_sunrise <- str_extract(Sunrise, "[+-]\\d+")
      extracted_sunrise <- as.numeric(extracted_sunrise)
      if (!is.na(extracted_sunrise) && (extracted_sunrise > 60 | extracted_sunrise < -60)) {
        print_and_stocked(FamilyName, Affiliation, paste0("Registered more than 1 hour before or after sunrise, check UTC or take contact"))
      }
    }
    if (!is.na(csv.df.list[[i]]$Sunset[j])) {
      Sunset <- csv.df.list[[i]]$Sunset[j]
      extracted_sunset <- str_extract(Sunset, "[+-]\\d+")
      extracted_sunset <- as.numeric(extracted_sunset)
      if (!is.na(extracted_sunset) && (extracted_sunset > 60 | extracted_sunset < -60)) {
        print_and_stocked(FamilyName, Affiliation, paste0("Registered more than 1 hour before or after sunset, check UTC or take contact"))
      }
    }
  }
}

for(i in 1:length(csv.df.list)){# Check that time columns are not empty 
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(is.na(csv.df.list[[i]]$StartTime[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column StartTime is empty"))
    }
    if(is.na(csv.df.list[[i]]$EndTime[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column EndTime is empty"))
    }
    if(is.na(csv.df.list[[i]]$Sunset[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column Sunset is empty"))
    }
    if(is.na(csv.df.list[[i]]$Sunrise[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column Sunrise is empty"))
    }
  }
}

# Replace characters by numeric if respond in character instead of code needed (e.g. Recorder = Audiomoth instead of 3) ####
for (i in 1:length(csv.df.list)){ 
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  Break1 = FALSE 
  Break2 = FALSE
  Break3 = FALSE
  for(j in 1:nrow(csv.df.list[[i]])){ 
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    # TypeStudy #
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
    # Recorder # 
    if(is.character(csv.df.list[[i]]$Recorder[j])){
      if(csv.df.list[[i]]$Recorder[j]=="Anabat Scout"){
        csv.df.list[[i]]$Recorder[j]=1
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Anabat Swift"){
        csv.df.list[[i]]$Recorder[j]=2
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="AudioMoth" | 
         csv.df.list[[i]]$Recorder[j]=="Audiomoth"){
        csv.df.list[[i]]$Recorder[j]=3
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Avisoft UltraSoundGate"){
        csv.df.list[[i]]$Recorder[j]=4
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Batcorder"){
        csv.df.list[[i]]$Recorder[j]=7
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Batlogger A"){
        csv.df.list[[i]]$Recorder[j]=8
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Batlogger A+"){
        csv.df.list[[i]]$Recorder[j]=9
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Batlogger C"){
        csv.df.list[[i]]$Recorder[j]=10
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Batmode S+"){
        csv.df.list[[i]]$Recorder[j]=11
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="GSM Batcorder"){
        csv.df.list[[i]]$Recorder[j]=12
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Mini-Batcorder"){
        csv.df.list[[i]]$Recorder[j]=13
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Passive recorder"){
        csv.df.list[[i]]$Recorder[j]=14
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Peersonic RPA3"){
        csv.df.list[[i]]$Recorder[j]=15
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Petterson D500X"){
        csv.df.list[[i]]$Recorder[j]=16
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="SM2BAT"){
        csv.df.list[[i]]$Recorder[j]=17 
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="SM2BAT+"){
        csv.df.list[[i]]$Recorder[j]=18
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="SM3BAT"){
        csv.df.list[[i]]$Recorder[j]=19
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="SM4BAT"){
        csv.df.list[[i]]$Recorder[j]=20
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Song Meter Mini Bat"){
        csv.df.list[[i]]$Recorder[j]=21
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Batlogger WE X"){
        csv.df.list[[i]]$Recorder[j]=23
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
      if(csv.df.list[[i]]$Recorder[j]=="Other" |
         csv.df.list[[i]]$Recorder[j]=="other"){
        csv.df.list[[i]]$Recorder[j]=24
        if(Break2 == FALSE){
          print(paste0("warning: replacing Recorder character by a numeric"))
          Break2=TRUE
        }
      }
    }
    # Mic #
    if(is.character(csv.df.list[[i]]$Mic[j])){
      if(csv.df.list[[i]]$Mic[j]=="SMX-US"){
        csv.df.list[[i]]$Mic[j]=1
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="SMX-UT"){
        csv.df.list[[i]]$Mic[j]=2
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="SMX-U1"){
        csv.df.list[[i]]$Mic[j]=3
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="SMM-U1"){
        csv.df.list[[i]]$Mic[j]=4
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="SMM-U2"){
        csv.df.list[[i]]$Mic[j]=5
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="UM12"){
        csv.df.list[[i]]$Mic[j]=6
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="BIO-SM2-US"){
        csv.df.list[[i]]$Mic[j]=7
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="US-O V3"){
        csv.df.list[[i]]$Mic[j]=8
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="US-D"){
        csv.df.list[[i]]$Mic[j]=9
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="ULTRASONIC MICROPHONE FG"){
        csv.df.list[[i]]$Mic[j]=10
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="BATLOGGER X MICROPHONE"){
        csv.df.list[[i]]$Mic[j]=11
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="Pole Microphone"){
        csv.df.list[[i]]$Mic[j]=12
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="Disk Microphone"){
        csv.df.list[[i]]$Mic[j]=13
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="Knowles FG"){
        csv.df.list[[i]]$Mic[j]=14
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="Knowles FG-O"){
        csv.df.list[[i]]$Mic[j]=15
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="UltraSoundGate CM16/CMPA"){
        csv.df.list[[i]]$Mic[j]=16
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="UltraSoundGate CM24/CMPA"){
        csv.df.list[[i]]$Mic[j]=17
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="GM50"){
        csv.df.list[[i]]$Mic[j]=18
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="GM90"){
        csv.df.list[[i]]$Mic[j]=19
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="TM10"){
        csv.df.list[[i]]$Mic[j]=20
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="TM20"){
        csv.df.list[[i]]$Mic[j]=21
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="Other external microphone without horn" |
         csv.df.list[[i]]$Mic[j]=="other external microphone without horn"){
        csv.df.list[[i]]$Mic[j]=22
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="Other external microphone with horn" |
         csv.df.list[[i]]$Mic[j]=="other external microphone with horn"){
        csv.df.list[[i]]$Mic[j]=23
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="Internal microphone" |
         csv.df.list[[i]]$Mic[j]=="internal microphone"){
        csv.df.list[[i]]$Mic[j]=24
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="AudioMoth" |
         csv.df.list[[i]]$Mic[j]=="Audiomoth"){
        csv.df.list[[i]]$Mic[j]=24
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
      if(csv.df.list[[i]]$Mic[j]=="Custom-made" |
         csv.df.list[[i]]$Mic[j]=="custom-made"){
        csv.df.list[[i]]$Mic[j]=25
        if(Break3 == FALSE){
          print(paste0("warning: replacing Mic character by a numeric"))
          Break3=TRUE
        }
      }
    }
  }
}

#Search information in Comment ####

#Quality / Threshold / posttrigger / Critical frenqency : 
for (i in 1:length(csv.df.list)) {
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for (j in 1:nrow(csv.df.list[[i]])) {
    if(csv.df.list[[i]]$Recorder[j] == 7 |
       csv.df.list[[i]]$Recorder[j] == 12 |
       csv.df.list[[i]]$Recorder[j] == 13){
      if (is.na(csv.df.list[[i]]$Quality[j]) & !is.na(csv.df.list[[i]]$Comment[j]) 
          & grepl("(?<=,|\\.|^)([^,.]*quality[^,.]*)(?=,|\\.|$)", csv.df.list[[i]]$Comment[j], ignore.case = TRUE)) {
        csv.df.list[[i]] <- csv.df.list[[i]] %>%
          mutate(Quality = ifelse(!is.na(Comment) & grepl("(?<=,|\\.|^)([^,.]*quality[^,.]*)(?=,|\\.|$)", Comment, ignore.case = TRUE),
                                  str_extract(Comment, "(?<=,|^)([^,]*quality[^,]*)(?=,|$)"), # extract between comma even if there are other information
                                  Quality))
        print("Quality information found in Comment")
      }
      if (is.na(csv.df.list[[i]]$TriggerLevel[j]) & !is.na(csv.df.list[[i]]$Comment[j]) 
          & grepl("(?<=,|\\.|^)([^,.]*sensitivity|threshold|hold|trigger\\lvl[^,.]*)(?=,|\\.|$)", csv.df.list[[i]]$Comment[j], ignore.case = TRUE)) {
        csv.df.list[[i]] <- csv.df.list[[i]] %>%
          mutate(TriggerLevel = ifelse(!is.na(Comment) & grepl("(?<=,|\\.|^)([^,.]*sensitivity|threshold|hold|trigger\\lvl[^,.]*)(?=,|\\.|$)", Comment, ignore.case = TRUE),
                                  str_extract(Comment, "(?<=,|\\.|^)([^,.]*sensitivity|threshold|hold|trigger\\lvl[^,.]*)(?=,|\\.|$)"), # extract between comma even if there are other information
                                  TriggerLevel))
        print("TriggerLevel/threshold information found in Comment")
      }
      if (is.na(csv.df.list[[i]]$FreqMin[j]) & !is.na(csv.df.list[[i]]$Comment[j]) 
          & grepl("(?<=,|\\.|^)([^,.]*critical\\frenquency|FreqMin|MinF|MinFreq[^,.]*)(?=,|\\.|$)", csv.df.list[[i]]$Comment[j], ignore.case = TRUE)) {
        csv.df.list[[i]] <- csv.df.list[[i]] %>%
          mutate(FreqMin = ifelse(!is.na(Comment) & grepl("(?<=,|\\.|^)([^,.]*critical\\frenquency|FreqMin|MinF|MinFreq[^,.]*)(?=,|\\.|$)", Comment, ignore.case = TRUE),
                                  str_extract(Comment, "(?<=,|\\.|^)([^,.]*critical\\frenquency|FreqMin|MinF|MinFreq[^,.]*)(?=,|\\.|$)"), # extract between comma even if there are other information
                                  FreqMin))
        print("FreqMin information found in Comment")
      }
      if (is.na(csv.df.list[[i]]$FreqMin[j]) & !is.na(csv.df.list[[i]]$Comment[j]) 
          & grepl("(?<=,|\\.|^)([^,.]*freq\\s+min\\s+max[^,.]*)(?=,|\\.|$)", csv.df.list[[i]]$Comment[j], ignore.case = TRUE)) {
        csv.df.list[[i]] <- csv.df.list[[i]] %>%
          mutate(FreqMin = ifelse(!is.na(Comment) & grepl("(?<=,|\\.|^)([^,.]*freq\\s+min\\s+max[^,.]*)(?=,|\\.|$)", Comment, ignore.case = TRUE),
                                  str_extract(Comment, "(?<=,|\\.|^)([^,.]*freq\\s+min\\s+max[^,.]*)(?=,|\\.|$)", ignore.case = TRUE), # extract between comma even if there are other information
                                  FreqMin))
        if(!is.na(csv.df.list[[i]]$FreqMin[j]) & grepl("\\b(\\d+)\\s*(?:-|freq\\s+min(?:\\s+and)?|(?:\\s+and)?\\s+freq\\s+max)\\s*(\\d+)\\b", csv.df.list[[i]]$FreqMin[j],ignore.case = TRUE )){
          freq <- "\\b(?:-|\\(khz\\))?\\s*(\\d+)(?:\\s*(?:-|\\(khz\\))?\\b)?"
          csv.df.list[[i]] <- csv.df.list[[i]] %>%
            mutate(FreqMin = ifelse(!is.na(FreqMin) & grepl("\\b(?:-|\\(khz\\))?\\s*(\\d+)(?:\\s*(?:-|\\(khz\\))?\\b)?", Comment, ignore.case = TRUE),
                                    paste0(freq),
                                    FreqMin))
        }
        print("FreqMin information found in Comment")
      }
      if (is.na(csv.df.list[[i]]$FreqMax[j]) & !is.na(csv.df.list[[i]]$Comment[j]) 
          & grepl("(?<=,|\\.|^)([^,.]*freq\\s+min\\s+max[^,.]*)(?=,|\\.|$)", csv.df.list[[i]]$Comment[j], ignore.case = TRUE)) {
        csv.df.list[[i]] <- csv.df.list[[i]] %>%
          mutate(FreqMax = ifelse(!is.na(Comment) & grepl("(?<=,|\\.|^)([^,.]*freq\\s+min\\s+max[^,.]*)(?=,|\\.|$)", Comment, ignore.case = TRUE),
                                  str_extract(Comment, "(?<=,|\\.|^)([^,.]*freq\\s+min\\s+max[^,.]*)(?=,|\\.|$)", ignore.case = TRUE), # extract between comma even if there are other information
                                  FreqMax))
        if(!is.na(csv.df.list[[i]]$FreqMax[j]) & grepl("\\b(\\d+)\\s*(?:-|freq\\s+min(?:\\s+and)?|(?:\\s+and)?\\s+freq\\s+max)\\s*(\\d+)\\b", csv.df.list[[i]]$FreqMax[j],ignore.case = TRUE )){
          freq <- "\\b(?:-|\\(khz\\))?\\s*(\\d+)(?:\\s*(?:-|\\(khz\\))?\\b)?"
          freq2 <- freq[[1]][2]
          csv.df.list[[i]] <- csv.df.list[[i]] %>%
            mutate(FreqMin = ifelse(!is.na(FreqMax) & grepl("\\b(?:-|\\(khz\\))?\\s*(\\d+)(?:\\s*(?:-|\\(khz\\))?\\b)?", Comment, ignore.case = TRUE),
                                    paste0(freq2),
                                    FreqMin))
        }
        print("FreqMax information found in Comment")
      }
    }
  }
}

  
for (i in 1:length(csv.df.list)) {
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for (j in 1:nrow(csv.df.list[[i]])) {
    if(csv.df.list[[i]]$Recorder[j] == 7 |
       csv.df.list[[i]]$Recorder[j] == 12 |
       csv.df.list[[i]]$Recorder[j] == 13){
      if(is.na(csv.df.list[[i]]$Quality[j])){
        print_and_stocked(FamilyName, Affiliation, paste0("warning: Quality for batcorder is emply"))
      }
    }
  }
}


# Remove units in values and change format to numeric ####
csv.df.list = csv.df.list %>% 
  map(~ .x %>% 
        mutate(MicHeight = gsub("[^0-9.-]", "", MicHeight))) %>% #deletes the contents of cells that are not numbers, hyphens or dots 
  map(~ .x %>% 
        mutate(HPF = gsub("[^0-9.-]", "", HPF))) %>%
  map(~ .x %>% 
        mutate(LPF = gsub("[^0-9.-]", "", LPF))) %>%
  map(~ .x %>% 
        mutate(FreqMin = gsub("[^0-9.-]", "", FreqMin))) %>%
  map(~ .x %>% 
        mutate(FreqMax = gsub("[^0-9.-]", "", FreqMax))) %>%
  map(~ .x %>% 
        mutate(TriggerLevel = gsub("[^0-9.-]", "", TriggerLevel))) %>%
  map(~ .x %>% 
        mutate(MinDur = gsub("[^0-9.-]", "", MinDur))) %>%
  map(~ .x %>% 
        mutate(MaxDur = gsub("[^0-9.-]", "", MaxDur))) %>%
  map(~ .x %>% 
        mutate(TrigWin = gsub("[^0-9.-]", "", TrigWin))) %>%
  map(~ .x %>% 
        mutate(Pause = gsub("[^0-9.-]", "", Pause))) %>%
  map(~ .x %>% 
        mutate(TrigWinMax = gsub("[^0-9.-]", "", TrigWinMax))) %>%
  map(~ .x %>% 
        mutate(Quality = gsub("[^0-9.-]", "", Quality))) %>%
  map(~ .x %>% 
        mutate(FileSplittingLength = gsub("[^0-9.-]", "", FileSplittingLength))) %>%
  lapply(transform, MicHeight = as.numeric(MicHeight)) %>% 
  lapply(transform, LPF = as.numeric(LPF)) %>% 
  lapply(transform, HPF = as.numeric(HPF)) %>% 
  lapply(transform, Participation = as.character(Participation)) %>% 
  lapply(transform, GainRecorder = as.character(GainRecorder)) %>% # because of Audiomoths
  lapply(transform, FreqMax = as.numeric(FreqMax))%>%
  lapply(transform, FreqMin = as.numeric(FreqMin))%>%
  lapply(transform, TriggerLevel = as.numeric(TriggerLevel))%>%
  lapply(transform, MinDur = as.numeric(MinDur))%>%
  lapply(transform, MaxDur = as.numeric(MaxDur))%>%
  lapply(transform, TrigWin = as.numeric(TrigWin))%>%
  lapply(transform, Pause = as.numeric(Pause))%>%
  lapply(transform, TrigWinMax = as.numeric(TrigWinMax))%>%
  lapply(transform, FileSplittingLength = as.numeric(FileSplittingLength))


for(i in 1:length(csv.df.list)){ # Check that people did not only write characters in these columns
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(is.na(csv.df.list[[i]]$MicHeight[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column MicHeight is empty"))
    }
    if(is.na(csv.df.list[[i]]$LPF[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column LPF is empty"))
    }
    if(is.na(csv.df.list[[i]]$HPF[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column HPF is empty"))
    }
    if(is.na(csv.df.list[[i]]$FreqMin[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column FreqMin is empty"))
    }
    if(is.na(csv.df.list[[i]]$FreqMax[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column FreqMax is empty"))
    }
    if(is.na(csv.df.list[[i]]$TriggerLevel[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column TriggerLevel is empty"))
    }
    if(is.na(csv.df.list[[i]]$MinDur[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column MinDur is empty"))
    }
    if(is.na(csv.df.list[[i]]$MaxDur[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column MaxDur is empty"))
    }
    if(is.na(csv.df.list[[i]]$TrigWin[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column TrigWin is empty"))
    }
    if(is.na(csv.df.list[[i]]$Pause[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column Pause is empty"))
    }
    if(is.na(csv.df.list[[i]]$NoiseFilter[j])) {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: column NoiseFilter is empty"))
    }
  }
}

# Check that values are in the expected ranges and correct mistakes between 0 and 999####
RecordersAbsoluteTrigger = c(3, 7, 12, 13, 14, 16:21)
for (i in 1:length(csv.df.list)){
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  Break= FALSE
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(is.na(csv.df.list[[i]]$TypeStudy[j])){
      print_and_stocked(FamilyName, Affiliation, paste0("warning: TypeStudy missing "))
    }else if(csv.df.list[[i]]$TypeStudy[j] > 6 & Break == FALSE) { 
      print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong TypeStudy"))
      Break = TRUE
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(!is.na(csv.df.list[[i]]$Recorder[j])){
      if(csv.df.list[[i]]$Recorder[j]> 24 & Break == FALSE) { 
        print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong Recorder"))
        Break = TRUE
      }
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(!is.na(csv.df.list[[i]]$Mic[j])){
      if(csv.df.list[[i]]$Mic[j] > 25 & Break == FALSE) { 
        print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong Mic"))
        Break = TRUE
      }
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(!is.na(csv.df.list[[i]]$GainRecorder[j]) &
       csv.df.list[[i]]$GainRecorder[j] != 999 &
       csv.df.list[[i]]$GainRecorder[j] > 100 & Break == FALSE) { 
      print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong GainRecorder"))
      Break = TRUE
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(!is.na(csv.df.list[[i]]$HPF[j]) &
       csv.df.list[[i]]$HPF[j] != 0 &
       csv.df.list[[i]]$HPF[j] > 20 & Break == FALSE) { 
      print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong HPF"))
      Break = TRUE
    }
  }
  
  Break1= FALSE
  Break2= FALSE
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(!is.na(csv.df.list[[i]]$LPF[j]) &
       csv.df.list[[i]]$LPF[j] != 999 &
       (csv.df.list[[i]]$LPF[j] < 50 | csv.df.list[[i]]$LPF[j] > 300) &
       Break2 == FALSE) # Avoids print_and_stockeding message for each row
    {
      if(csv.df.list[[i]]$LPF[j] == 0 ){
        csv.df.list[[i]]$LPF = 999
        if(Break1 == FALSE){
          print(paste0("warning: LPF has value 0 instead of 999, thus I am replacing by 999"))
          Break1=TRUE
        }
      }else{
        print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong LPF"))
        Break2=TRUE
      }
    }
  }
  
  Break= FALSE
  for (j in 1:nrow(csv.df.list[[i]])) {
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    freq_min_value <- as.numeric(csv.df.list[[i]]$FreqMin[j])
    if (!is.na(freq_min_value)) {
      if (freq_min_value != 0 & freq_min_value > 20) {
        if (Break == FALSE) {
          print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong FreqMin"))
          Break <- TRUE
        }
      }
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(!is.na(csv.df.list[[i]]$FreqMax[j]) &
       csv.df.list[[i]]$FreqMax[j] != 999 &
       (csv.df.list[[i]]$FreqMax[j] < 50 |  csv.df.list[[i]]$FreqMax[j]  > 300) &
       Break == FALSE) { 
      print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong FreqMax"))
      Break = TRUE
    }
  }
  
  Break1 = FALSE
  Break2 = FALSE
  for(j in 1:nrow(csv.df.list[[i]])) {
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(!is.na(csv.df.list[[i]]$TriggerLevel[j]) &
       csv.df.list[[i]]$TriggerLevel[j] != 0 &
       as.numeric(csv.df.list[[i]]$TriggerLevel[j]  < -100 | csv.df.list[[i]]$TriggerLevel[j]  > 0)){
      if(csv.df.list[[i]]$Recorder[j] %in% RecordersAbsoluteTrigger) 
      {
        if(Break1 == FALSE){
          print(paste0("warning: TriggerLevel has positive value"))
          Break1 = TRUE
        }
        #csv.df.list[[i]]$TriggerLevel[j]=-as.numeric(csv.df.list[[i]]$TriggerLevel[j]) # non ne pas mettre en negatif pour les SM4BAT
      }else{
        if(Break2 == FALSE){
          print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong TriggerLevel"))
          Break2 = TRUE
        }
      }
    }
  }
  
  Break= FALSE
  
  for (j in 1:nrow(csv.df.list[[i]])) {
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if (!is.na(csv.df.list[[i]]$MinDur[j])) {
      if (csv.df.list[[i]]$MinDur[j] != 0) {
        numeric_value <- as.numeric(csv.df.list[[i]]$MinDur[j])
        if (!is.na(numeric_value) & (numeric_value < 1 | numeric_value > 10)) {
          if (Break == FALSE) {
            print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong MinDur"))
            Break <- TRUE
          }
        }
      }
    }
  }  
  Break1= FALSE
  Break2= FALSE
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(!is.na(csv.df.list[[i]]$MaxDur[j]) &
       csv.df.list[[i]]$MaxDur[j] != 999 &
       csv.df.list[[i]]$MaxDur[j] < 100) # Avoids print_and_stockeding message for each row
    {
      if(csv.df.list[[i]]$MaxDur[j] == 0 ){
        csv.df.list[[i]]$MaxDur = 999
        if(Break1 == FALSE){
          print(paste0("warning: MaxDur has value 0 instead of 999, thus I am replacing by 999"))
          Break1=TRUE
        }
      }else{
        if(Break2 == FALSE){
          print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong MaxDur"))
          Break2=TRUE
        }
      }
    }
  }
  
  Break= FALSE
  for(j in 1:nrow(csv.df.list[[i]])){
    FamilyName = csv.df.list[[i]]$FamilyName[j]
    Affiliation = csv.df.list[[i]]$Affiliation[j]
    if(!is.na(csv.df.list[[i]]$TrigWin[j]) &
       csv.df.list[[i]]$TrigWin[j] != 999 &
       csv.df.list[[i]]$TrigWin[j] > 60 & Break == FALSE) 
    {
      print_and_stocked(FamilyName, Affiliation, paste0("warning: wrong TrigWin"))
      Break = TRUE
    }
  }
}

# save dataframe ####

for (i in 1:length(csv.df.list)) {
  FamilyNameTop = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyNameTop))
  for (j in 1:nrow(csv.df.list[[i]])) {
    if (!is.character(csv.df.list[[i]]$FirstName[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(FirstName = as.character(FirstName))  
    }
    if (!is.character(csv.df.list[[i]]$FamilyName[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(FamilyName = as.character(FamilyName))  
    }
    if (!is.character(csv.df.list[[i]]$Email[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Email = as.character(Email))  
    }
    if (!is.character(csv.df.list[[i]]$Affiliation[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Affiliation = as.character(Affiliation))  
    }
    if (!is.character(csv.df.list[[i]]$Country[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Country = as.character(Country))  
    }
    if (!is.character(csv.df.list[[i]]$Site[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Site = as.character(Site))  
    }
    if (!is.numeric(csv.df.list[[i]]$X[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(X = as.numeric(X))  
    }
    if (!is.numeric(csv.df.list[[i]]$Y[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Y = as.numeric(Y))  
    }
    if (!is.character(csv.df.list[[i]]$Participation[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Participation = as.character(Participation))  
    }
    if (!is.character(csv.df.list[[i]]$Sunset[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Sunset = as.character(Sunset))  
    }
    if (!is.character(csv.df.list[[i]]$Sunrise[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Sunrise = as.character(Sunrise))  
    }
    if (!is.character(csv.df.list[[i]]$StartTime[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(StartTime = as.character(StartTime))  
    }
    if (!is.character(csv.df.list[[i]]$EndTime[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(EndTime = as.character(EndTime))  
    }
    if (!is.character(csv.df.list[[i]]$UTC[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(UTC = as.character(UTC))  
    }
    if (!is.numeric(csv.df.list[[i]]$Mic[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Mic = as.numeric(Mic))  
    }
    if (!is.numeric(csv.df.list[[i]]$Recorder[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Recorder = as.numeric(Recorder))  
    }
    if (!is.numeric(csv.df.list[[i]]$TypeStudy[j])) {
        csv.df.list[[i]] <- csv.df.list[[i]] %>%
          mutate(TypeStudy = as.numeric(TypeStudy))  
    }
    if (!is.character(csv.df.list[[i]]$TypeStudyOther[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(TypeStudyOther = as.character(TypeStudyOther))
    }
    if (!is.character(csv.df.list[[i]]$PositionNacelle[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(PositionNacelle = as.character(PositionNacelle))
    }
    if (!is.numeric(csv.df.list[[i]]$MicHeight[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(MicHeight = as.numeric(MicHeight))  
    }
    if (!is.character(csv.df.list[[i]]$RecorderOther[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(RecorderOther = as.character(RecorderOther))
    }
    if (!is.character(csv.df.list[[i]]$MicOther[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(MicOther = as.character(MicOther))
    }
    if (!is.character(csv.df.list[[i]]$GainRecorder[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(GainRecorder = as.character(GainRecorder))
    }
    if (!is.numeric(csv.df.list[[i]]$HPF[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(HPF = as.numeric(HPF))  
    }
    if (!is.numeric(csv.df.list[[i]]$LPF[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(LPF = as.numeric(LPF))  
    }
    if (!is.numeric(csv.df.list[[i]]$FreqMin[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(FreqMin = as.numeric(FreqMin))  
    }
    if (!is.numeric(csv.df.list[[i]]$FreqMax[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(FreqMax = as.numeric(FreqMax))  
    }
    if (!is.character(csv.df.list[[i]]$TriggerLevel[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(TriggerLevel = as.character(TriggerLevel))
    }
    if (!is.numeric(csv.df.list[[i]]$MinDur[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(MinDur = as.numeric(MinDur))  
    }
    if (!is.numeric(csv.df.list[[i]]$MaxDur[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(MaxDur = as.numeric(MaxDur))  
    }
    if (!is.numeric(csv.df.list[[i]]$TrigWin[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(TrigWin = as.numeric(TrigWin))  
    }
    if (!is.numeric(csv.df.list[[i]]$Pause[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Pause = as.numeric(Pause))  
    }
    if (!is.numeric(csv.df.list[[i]]$TrigWinMax[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(TrigWinMax = as.numeric(TrigWinMax))  
    }
    if (!is.numeric(csv.df.list[[i]]$FileSplittingLength[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(FileSplittingLength = as.numeric(FileSplittingLength))  
    }
    if (!is.character(csv.df.list[[i]]$NoiseFilter[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(NoiseFilter = as.character(NoiseFilter))
    }
    if (!is.character(csv.df.list[[i]]$Partner[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Partner = as.character(Partner))
    }
    if (!is.character(csv.df.list[[i]]$Comment[j])) {
      csv.df.list[[i]] <- csv.df.list[[i]] %>%
        mutate(Comment = as.character(Comment))
    }
  }
}

csv.df = bind_rows(csv.df.list)

if(rowSums(as.data.frame(lapply(csv.df.list, nrow))) != nrow(csv.df)){
  print_and_stocked("warning: rbind did not succeed")
}
write.csv(csv.df, "/home/charlotte/Documents/Post-Doc/MIGRATION/Metadonnees/Return_from_partners_FINAL/Metadata_complet.csv")

# save summary missing data ####

print(missing_info)
write.csv(missing_info, "/home/charlotte/Documents/Post-Doc/MIGRATION/Metadonnees/Return_from_partners_FINAL/summary_missing_info.csv")

# Outliers #### 
#>>> Utiliser geom_jitter

csv.df.filtered_FreqMax <- csv.df %>%
  filter(FreqMax != 999)#Avoiding the value which signify that they don't use this parameters
ggplot(csv.df.filtered_FreqMax, aes(x = "",y = FreqMax)) +
  geom_boxplot()+
  geom_jitter()+
  geom_text(data = subset(csv.df.filtered_FreqMax, FreqMax < quantile(FreqMax, 0.25) - 1.5*IQR(FreqMax) | FreqMax > quantile(FreqMax, 0.75) + 1.5*IQR(FreqMax)),
            aes(label = round(FreqMax, 2)),
            vjust = -0.5, color = "red", size = 3)+
  labs(y = "Valeurs FreqMax", title = "Boxplot des valeurs FreqMax")

csv.df.filtered_FreqMin <- csv.df %>%
  filter(FreqMin != 0)
ggplot(csv.df.filtered_FreqMin, aes(x = "",y = FreqMin)) +
  geom_boxplot()+
  geom_jitter()+
  geom_text(data = subset(csv.df.filtered_FreqMin, FreqMin < quantile(FreqMin, 0.25) - 1.5*IQR(FreqMin) | FreqMin > quantile(FreqMin, 0.75) + 1.5*IQR(FreqMin)),
            aes(label = round(FreqMin, 2)),
            vjust = -0.5, color = "red", size = 3) +
  labs(y = "Valeurs FreqMin", title = "Boxplot des valeurs FreqMin")

csv.df.filtered_HPF <- csv.df %>%
  filter(HPF != 0)
ggplot(csv.df.filtered_HPF, aes(x = "",y = HPF)) +
  geom_boxplot()+
  geom_jitter()+
  geom_text(data = subset(csv.df.filtered_HPF, HPF < quantile(HPF, 0.25) - 1.5*IQR(HPF) | HPF > quantile(HPF, 0.75) + 1.5*IQR(HPF)),
            aes(label = round(HPF, 2)),
            vjust = -0.5, color = "red", size = 3) +
  labs(y = "Valeurs HPF", title = "Boxplot des valeurs HPF")

csv.df.filtered_LPF <- csv.df %>%
  filter(LPF != 999)
ggplot(csv.df.filtered_LPF, aes(x = "", y = LPF)) +
  geom_boxplot()+
  geom_jitter()+
  geom_text(data = subset(csv.df.filtered_LPF, LPF < quantile(LPF, 0.25) - 1.5*IQR(LPF) | LPF > quantile(LPF, 0.75) + 1.5*IQR(LPF)),
            aes(label = round(LPF, 2)),
            vjust = -0.5, color = "red", size = 3)+
  labs(y = "Valeurs LPF", title = "Boxplot des valeurs LPF")


csv.df.filtered_TriggerLevel <- csv.df %>%
  filter(TriggerLevel != 0) 
ggplot(csv.df.filtered_TriggerLevel, aes(x = "", y = TriggerLevel)) +
  geom_boxplot()+
  geom_jitter()+
  geom_text(data = subset(csv.df.filtered_TriggerLevel, TriggerLevel < quantile(TriggerLevel, 0.25) - 1.5*IQR(TriggerLevel) | TriggerLevel > quantile(TriggerLevel, 0.75) + 1.5*IQR(TriggerLevel)),
            aes(label = round(TriggerLevel, 2)),
            vjust = -0.5, color = "red", size = 3)+
  labs(y = "Valeurs TriggerLevel", title = "Boxplot des valeurs TriggerLevel")

csv.df.filtered_MinDur <- csv.df %>%
  filter(MinDur != 0) 
ggplot(csv.df.filtered_MinDur, aes(x = "", y = MinDur)) +
  geom_boxplot()+
  geom_jitter()+
  geom_text(data = subset(csv.df.filtered_MinDur, MinDur < quantile(MinDur, 0.25) - 1.5*IQR(MinDur) | MinDur > quantile(MinDur, 0.75) + 1.5*IQR(MinDur)),
            aes(label = round(MinDur, 2)),
            vjust = -0.5, color = "red", size = 3)+
  labs(y = "Valeurs MinDur", title = "Boxplot des valeurs MinDur")

csv.df.filtered_MaxDur <- csv.df %>%
  filter(MaxDur != 999)
ggplot(csv.df.filtered_MaxDur, aes(x = "", y = MaxDur)) +
  geom_boxplot()+
  geom_jitter()+
  geom_text(data = subset(csv.df.filtered_MaxDur, MaxDur < quantile(MaxDur, 0.25) - 1.5*IQR(MaxDur) | MaxDur > quantile(MaxDur, 0.75) + 1.5*IQR(MaxDur)),
            aes(label = round(MaxDur, 2)),
            vjust = -0.5, color = "red", size = 3)+
  labs(y = "Valeurs MaxDur", title = "Boxplot des valeurs MaxDur")

csv.df.filtered_TrigWin <- csv.df %>%
  filter(TrigWin != 999) 
ggplot(csv.df.filtered_TrigWin, aes(x = "", y = TrigWin)) +
  geom_boxplot()+
  geom_jitter()+
  geom_text(data = subset(csv.df.filtered_TrigWin, TrigWin < quantile(TrigWin, 0.25) - 1.5*IQR(TrigWin) | TrigWin > quantile(TrigWin, 0.75) + 1.5*IQR(TrigWin)),
            aes(label = round(TrigWin, 2)),
            vjust = -0.5, color = "red", size = 3)+
  labs(y = "Valeurs TrigWin", title = "Boxplot des valeurs TrigWin")

csv.df.filtered_Pause <- csv.df %>%
  filter(Pause != 0) 
ggplot(csv.df.filtered_Pause, aes(x = "", y = Pause)) +
  geom_boxplot()+
  geom_jitter()+
  geom_text(data = subset(csv.df.filtered_Pause, Pause < quantile(Pause, 0.25) - 1.5*IQR(Pause) | Pause > quantile(Pause, 0.75) + 1.5*IQR(Pause)),
            aes(label = round(Pause, 2)),
            vjust = -0.5, color = "red", size = 3)+
  labs(y = "Valeurs Pause", title = "Boxplot des valeurs Pause")

csv.df.filtered_TrigWinMax <- csv.df %>%
  filter(TrigWinMax != 999) 
ggplot(csv.df.filtered_TrigWinMax, aes(x = "", y = TrigWinMax)) +
  geom_boxplot()+
  geom_jitter()+
  geom_text(data = subset(csv.df.filtered_TrigWinMax, TrigWinMax < quantile(TrigWinMax, 0.25) - 1.5*IQR(TrigWinMax) | TrigWinMax > quantile(TrigWinMax, 0.75) + 1.5*IQR(TrigWinMax)),
            aes(label = round(TrigWinMax, 2)),
            vjust = -0.5, color = "red", size = 3)+
  labs(y = "Valeurs TrigWinMax", title = "Boxplot des valeurs TrigWinMax")



# Maps ####
library(maps)

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
  FamilyName = csv.df.list[[i]]$FamilyName[1]
  print(paste0("i = ", i, " ", FamilyName))
  if(!is.na(csv.df.list[[i]]$X[j]) & !is.na(csv.df.list[[i]]$Y[j])){
    Site_localite_sf = st_as_sf( 
      csv.df.list[[i]], coords = c("X", "Y"), crs=4326, remove=FALSE) %>% 
      st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    Site_localite_Europe_sf = Site_localite_sf %>% # Crop data in European boundaries
      st_crop(st_bbox(Europe_sf))
    if(nrow(Site_localite_Europe_sf)<nrow(Site_localite_sf)){ # check that coordinates are in boundaries
      print(paste0("warning: coordinates out of Europe for ", FamilyName))
    }
  }
}

# Plot
csv.df.coordOK<- csv.df %>%
  filter(!is.na(csv.df$X) & !is.na(csv.df$Y))
X_Y_for_plot <- csv.df.coordOK %>%
  select(X, Y)
X_Y_for_plot_sf = st_as_sf( 
  X_Y_for_plot, coords = c("X", "Y"), crs=4326, remove=FALSE) %>% 
  st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

ggplot() +
  geom_sf(data=Europe_sf, fill="white") +
  geom_sf(data=X_Y_for_plot_sf)
















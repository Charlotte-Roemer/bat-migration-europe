

library(data.table)
library(readxl)
library(tidyverse)
library(lubridate)


setwd("C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Metadonnees/TEST")

#### List and load metadata files ####
#xlsx.list <- list.files(pattern='*.xlsx', recursive = TRUE)
csv.list <- list.files("C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Metadonnees/TEST",
                       pattern='*.csv', recursive = TRUE)

#xlsx.df.list <- lapply(xlsx.list, read_excel)
csv.df.list <- lapply(csv.list, fread)

# Check if number and names of columns is right

name_col=c("FirstName",	"FamilyName",	"Email",	"Affiliation",	"Country",	"Site",	"X",	"Y",	
           "Participation",	"StartDate",	"EndDate",	"StartTime",	"EndTime",	"TypeStudy",	
           "TypeStudyOther",	"PositionNacelle",	"MicHeight",	"Recorder",	"RecorderOther",	
           "Mic",	"MicOther",	"GainRecorder",	"HPF",	"LPF",	"FreqMin",	"FreqMax",	"TriggerLevel",	
           "MinDur",	"MaxDur",	"TrigWin",	"Pause",	"TrigWinMax",	"FileSplittingLength",	
           "NoiseFilter",	"Partner",	"Comment")

# for (i in 1:length(xlsx.df.list)){
#   Affiliation = xlsx.df.list[[i]]$Affiliation[1]
#   if(ncol(xlsx.df.list[[i]]) != 36) {
#     print(paste0("error: number of columns is incorrect for ", Affiliation))
#     print(paste0("n columns =  ", ncol(xlsx.df.list[[i]]) ))
#   }
#   matchCOL=match (colnames(xlsx.df.list[[i]]), name_col)
#   if(TRUE %in% is.na(matchCOL)){
#     print(paste0("error: column names are incorrect for ", Affiliation))
#     print(paste0("missing columns: ", name_col[which(is.na(matchCOL))]))
#   }
# }

for (i in 1:length(csv.df.list)){
  Affiliation = csv.df.list[[i]]$Affiliation[1]
  if(ncol(csv.df.list[[i]]) != 36) {
    print(paste0("error: number of columns is incorrect for ", Affiliation))
    print(paste0("n columns =  ", ncol(csv.df.list[[i]]) ))
  }
  matchCOL=match (colnames(csv.df.list[[i]]), name_col)
  if(TRUE %in% is.na(matchCOL)){
    print(paste0("error: column names are incorrect for ", Affiliation))
    print(paste0("missing columns: ", name_col[which(is.na(matchCOL))]))
  }
}

#### Check column format ####
StandardFormat = c(rep("character",6), rep("numeric", 2), rep("character",5),
                   "integer",  rep("character", 2), "numeric", "integer", "character",
                   "integer", "character", rep("integer", 12), rep("character",3))
# 
# for (i in 1:length(xlsx.df.list)){
#   Affiliation = xlsx.df.list[[i]]$Affiliation[1]
#   for (j in 1:length(StandardFormat)){
#     if(!class(as.data.frame(xlsx.df.list[[i]])[,j])[1]=="POSIXct" &
#        !class(as.data.frame(xlsx.df.list[[i]])[,j])[1]=="IDate"){
#       if(names(table(as.data.frame(lapply(xlsx.df.list[[i]], class)[j]))) != StandardFormat[j]) {
#         if(!all(is.na(xlsx.df.list[[i]][,j]))  &
#            !all((xlsx.df.list[[i]][,j]) == "NA") ){ # if the column is not empty
#           if (!((names(table(as.data.frame(lapply(xlsx.df.list[[i]], class)[j]))) == "integer" &
#                  StandardFormat[j] == "numeric") | # considers that integer = numeric
#                 (names(table(as.data.frame(lapply(xlsx.df.list[[i]], class)[j]))) == "numeric" &
#                  StandardFormat[j] == "integer"))) { # considers that numeric = integer
#             print(paste0("warning: column format is incorrect for ", Affiliation))
#             print(paste0("wrong format for ",  names(as.data.frame(lapply(xlsx.df.list[[i]], class)[j])),
#                          ": found ", names(table(as.data.frame(lapply(xlsx.df.list[[i]], class)[j]))), 
#                          " instead of ", StandardFormat[j]))
#           }
#         }
#       }
#     }
#   }
# }
for (i in 1:length(csv.df.list)){
  Affiliation = csv.df.list[[i]]$Affiliation[1]
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
            print(paste0("warning for: ", Affiliation))
            print(paste0("wrong format for ",  names(as.data.frame(lapply(csv.df.list[[i]], class)[j])),
                         ": found ", names(table(as.data.frame(lapply(csv.df.list[[i]], class)[j]))), 
                         " instead of ", StandardFormat[j]))
          }
        }
      }
    }
  }
}
#### Remove units in values and change format to numeric ####
# xlsx.df.list = xlsx.df.list %>% 
#   map(~ .x %>% 
#         mutate(MicHeight = gsub("[^0-9.-]", "", MicHeight))) %>% 
#   map(~ .x %>% 
#         mutate(LPF = gsub("[^0-9.-]", "", LPF))) %>%  
#   lapply(transform, MicHeight = as.numeric(MicHeight)) %>% 
#   lapply(transform, LPF = as.numeric(LPF))
# for(i in 1:length(xlsx.df.list)){ # Check that people did not only write characters in these columns
#   Affiliation = xlsx.df.list[[i]]$Affiliation[1]
#   if(all(is.na(xlsx.df.list[[i]]$MicHeight))) {
#     print(paste0("warning: column MicHeight is empty for ", Affiliation))
#   }
#   if(all(is.na(xlsx.df.list[[i]]$LPF))) {
#     print(paste0("warning: column LPF is empty for ", Affiliation))
#   }
# }
csv.df.list = csv.df.list %>% 
  map(~ .x %>% 
        mutate(MicHeight = gsub("[^0-9.-]", "", MicHeight))) %>% 
  map(~ .x %>% 
        mutate(LPF = gsub("[^0-9.-]", "", LPF))) %>%  
  lapply(transform, MicHeight = as.numeric(MicHeight)) %>% 
  lapply(transform, LPF = as.numeric(LPF))
for(i in 1:length(csv.df.list)){ # Check that people did not only write characters in these columns
  Affiliation = csv.df.list[[i]]$Affiliation[1]
  if(all(is.na(csv.df.list[[i]]$MicHeight))) {
    print(paste0("warning: column MicHeight is empty for ", Affiliation))
  }
  if(all(is.na(csv.df.list[[i]]$LPF))) {
    print(paste0("warning: column LPF is empty for ", Affiliation))
  }
}

#### Make DATE format ####
# for(i in 1:length(xlsx.df.list)){
#   if(grepl("/", xlsx.df.list[[i]]$StartDate[1])){
#     xlsx.df.list[[i]]$StartDate = dmy(xlsx.df.list[[i]]$StartDate)
#   }
#   if(grepl("/", xlsx.df.list[[i]]$EndDate[1])){
#     xlsx.df.list[[i]]$EndDate = dmy(xlsx.df.list[[i]]$EndDate)
#   }
# }
# xlsx.df.list = lapply(xlsx.df.list,function(x) { # /!\ RUN ONLY ONCE
#   x$StartDate <- as.Date( x$StartDate, format="%Y-%m-%d");
#   x})
# xlsx.df.list = lapply(xlsx.df.list,function(x) { # /!\ RUN ONLY ONCE
#   x$EndDate <- as.Date( x$EndDate, format="%Y-%m-%d");
#   x})
# for(i in 1:length(xlsx.df.list)){ # Check that date columns are not empty
#   Affiliation = xlsx.df.list[[i]]$Affiliation[1]
#   if(all(is.na(xlsx.df.list[[i]]$StartDate))) {
#     print(paste0("warning: column StartDate is empty for ", Affiliation))
#   }
#   if(all(is.na(xlsx.df.list[[i]]$EndDate))) {
#     print(paste0("warning: column EndDate is empty for ", Affiliation))
#   }
# }
for(i in 1:length(csv.df.list)){
  if(grepl("/", csv.df.list[[i]]$StartDate[1])){
    csv.df.list[[i]]$StartDate = dmy(csv.df.list[[i]]$StartDate)
    csv.df.list[[i]]$EndDate = dmy(csv.df.list[[i]]$EndDate)
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
# for(i in 1:length(xlsx.df.list)){
#   if(grepl("PM", xlsx.df.list[[i]]$StartTime[1]) |
#      grepl("AM", xlsx.df.list[[i]]$StartTime[1])){
#     xlsx.df.list[[i]]$StartTime =  hms(format(strptime(xlsx.df.list[[i]]$StartTime, "%I:%M:%S %p"), "%H:%M:%S"))
#     xlsx.df.list[[i]]$EndTime =  hms(format(strptime(xlsx.df.list[[i]]$EndTime, "%I:%M:%S %p"), "%H:%M:%S"))
#   }else{
#     if(str_count(xlsx.df.list[[i]]$StartTime[1], ":")==1){
#       xlsx.df.list[[i]]$StartTime = hm(xlsx.df.list[[i]]$StartTime)
#       xlsx.df.list[[i]]$EndTime = hm(xlsx.df.list[[i]]$EndTime)
#     }else{
#       xlsx.df.list[[i]]$StartTime = hms(xlsx.df.list[[i]]$StartTime)
#       xlsx.df.list[[i]]$EndTime = hms(xlsx.df.list[[i]]$EndTime)
#     }
#   }
# }
# for(i in 1:length(xlsx.df.list)){ # Check that time columns are not empty
#   Affiliation = xlsx.df.list[[i]]$Affiliation[1]
#   if(all(is.na(xlsx.df.list[[i]]$StartTime))) {
#     print(paste0("warning: column StartTime is empty for ", Affiliation))
#   }
#   if(all(is.na(xlsx.df.list[[i]]$EndTime))) {
#     print(paste0("warning: column EndTime is empty for ", Affiliation))
#   }
# }
for(i in 1:length(csv.df.list)){
  if(grepl("PM", csv.df.list[[i]]$StartTime[1]) |
     grepl("AM", csv.df.list[[i]]$StartTime[1])){
    csv.df.list[[i]]$StartTime =  hms(format(strptime(csv.df.list[[i]]$StartTime, "%I:%M:%S %p"), "%H:%M:%S"))
    csv.df.list[[i]]$EndTime =  hms(format(strptime(csv.df.list[[i]]$EndTime, "%I:%M:%S %p"), "%H:%M:%S"))
  }else{
    if(str_count(csv.df.list[[i]]$StartTime[1], ":")==1){
      csv.df.list[[i]]$StartTime = hm(csv.df.list[[i]]$StartTime)
      csv.df.list[[i]]$EndTime = hm(csv.df.list[[i]]$EndTime)
    }else{
      csv.df.list[[i]]$StartTime = hms(csv.df.list[[i]]$StartTime)
      csv.df.list[[i]]$EndTime = hms(csv.df.list[[i]]$EndTime)
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


#### Rbind data ####
#xlsx.df = do.call("rbind", xlsx.df.list)
csv.df = do.call("rbind", csv.df.list)

# Check if Site and participation folders are all at the IN2P3



# Check if Site and participation folders at the IN2P3 are in the metadata table









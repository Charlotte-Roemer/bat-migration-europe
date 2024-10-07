# Predicts activity using the random forest models

library(data.table)
library(randomForest)
library(spdep)
library(tidyverse)
library(pracma)
library(sp)

Place = "PCIA" # local or PCIA



#### Options ####--------------------------------------------------------

args="Rhifer" #all species = "all", one species = e.g. "Pippip"
Threshold = "weighted" #weighted or 0 or 50 or 90
DateModel = "2024-09-17" #date of modelling (exactly same writing as the folder name)
CoordType = "EDF" # Spatial proxies in predictors: "LongLat" = X + Y ; "EDF" = X + Y + Euclidian Distance Fields ;  "noCoord" = no coordinates
#MTRY = "default" # "default" or "npred" or "2-3" for 2/3 of npred
NTREE = 500
Simulation_ALAN = F # If you want to simulate a full night everywhere
DoBoruta = F # Do variable selection?



#### Directories ####-----------------------------------------------------------

if(Place == "local"){
  ModRF_Dir = "C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/"
  args[2]="C:/Users/croemer01/Documents/Donnees vigie-chiro/SysGrid_500m_de_cote_FULL_with_habitat"
  Output=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/PredictionsModels/", Threshold, "_", DateModel, "/") #folder to copy models to 
  SpList = read_delim("C:/Users/croemer01/Documents/Donnees vigie-chiro/SpeciesList.csv", delim = ";")
  
}

if(Place == "PCIA"){
  ModRF_Dir = "/mnt/beegfs/croemer/VigieChiro/ModPred/"
  args[2]="/mnt/beegfs/croemer/VigieChiro/SIG/vieux_2023/SysGrid_500m_de_cote_FULL_with_habitat"
  Output=paste0("/mnt/beegfs/croemer/VigieChiro/PredictionsModels/", Threshold, "_", DateModel, "/") #folder to copy models to 
  SpList = read_delim("/mnt/beegfs/croemer/VigieChiro/SpeciesList.csv", delim = ";")
  
}

# Suffix to prepare loading
if(DoBoruta == T){
  suffix=paste0("_Boruta_", CoordType, "_", NTREE)
}else{
  suffix=paste0(CoordType, "_", NTREE)
}

dir.create(Output)

# Load models
if(args[1]=="all"|args[1]=="All"){
  ModRF_list = list.files(paste0(ModRF_Dir, paste0("VC", Threshold, "_"), DateModel), 
                          pattern=paste0("RFspat", ".+", "VC", Threshold, "_", DateModel, suffix, ".rds"), full.names = T)
  print(paste0(ModRF_Dir, paste0("VC", Threshold, "PG_"), DateModel))
}else{
  ModRF_file=paste0(ModRF_Dir, "VC", Threshold, "_", DateModel, "/RFspat_",
                    args[1],"VC",Threshold, "_", DateModel, "_", suffix, ".rds")
  ModRF_list = list(ModRF_file)
}

print(ModRF_list)



#### Prepare general dataset ####-------------------------------------------------------

# Load table containing habitat variables
CoordSIG=fread(paste0(args[2],".csv")) 
print(nrow(CoordSIG))
CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpAltiS)==F)
CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpBioC1)==F)
print(nrow(CoordSIG))

# Cleans coordinates
CoordSIG = CoordSIG %>%
  rename_all(~str_replace_all(.,"\\.x",""))
CoordSIG <- CoordSIG %>% select(-contains(".y"))

print(CoordType)


# Add spatial proxies
if(CoordType == "LongLat"){ # coordinates
  CoordSIG$Splatitude = CoordSIG$latitude
  CoordSIG$Splongitude = CoordSIG$longitude
}

if(CoordType == "EDF"){ # Eurclidian Distance Fields
  CoordSIG_sf = st_as_sf(CoordSIG, coords = c(x="X", y="Y"), crs = 4326) %>% 
    st_transform(2154)
  
  coords <- as.data.frame(st_coordinates(CoordSIG_sf))
  
  # sf object with 5 points: the bounding box of the grid of points + the center
  EDF <- rbind(st_sf(geom = st_sfc(st_point(c(min(coords$X),min(coords$Y))))),
               st_sf(geom = st_sfc(st_point(c(min(coords$X),max(coords$Y))))),
               st_sf(geom = st_sfc(st_point(c(max(coords$X),min(coords$Y))))),
               st_sf(geom = st_sfc(st_point(c(max(coords$X),max(coords$Y))))),
               st_sf(geom = st_sfc(st_point(c(median(coords$X),median(coords$Y))))))
  EDF <- st_set_crs(EDF, st_crs(CoordSIG_sf))
  EDF <- st_distance(CoordSIG_sf, EDF)/1000 # calculate distance between the point and each of these 5 points
  EDF <- units::drop_units(EDF)
  EDF <- as.data.frame(EDF)
  names(EDF) <- paste0("EDF", 1:5)
  CoordSIG$SpEDF1 = EDF$EDF1
  CoordSIG$SpEDF2 = EDF$EDF2
  CoordSIG$SpEDF3 = EDF$EDF3
  CoordSIG$SpEDF4 = EDF$EDF4
  CoordSIG$SpEDF5 = EDF$EDF5
  
  CoordSIG$Splatitude = CoordSIG$Y
  CoordSIG$Splongitude = CoordSIG$X
}

# Create date vector with a sample each 15 days
# Date_seq = seq.Date(from = as.Date('2018-03-01'), to = as.Date('2018-10-31'), by = 'month')
# Date_seq = c(Date_seq, (Date_seq+14))
Date_seq = c(as.Date('2018-03-01'), as.Date('2018-07-01'), as.Date('2018-10-01'))

CoordSIG$SpGite=0
CoordSIG$SpRecorder="SM2BAT+"

if(Simulation_ALAN){
  CoordSIG$SpALAN_M = 0
  CoordSIG$SpALAN_L = 0
}

print("Grid and variables loaded")



#### Make predictions ####-------------------------------------------------------

# For each species
for (k in 1:length(ModRF_list)){
  
  print(ModRF_list[k])
  Sp = SpList$Esp[which(str_detect(ModRF_list[k], SpList$Esp))]
  print(Sp)
  
  ModRF = readRDS(as.character(ModRF_list[k])) # Load random forest model
  print(rownames(ModRF$finalModel$importance)[!is.na(test)])
  
  # For each date
  for (i in 1:length(Date_seq)){
    
    Date_of_model=Date_seq[i]
    print(Date_of_model)
    
    CoordSIG2 = CoordSIG
    
    CoordSIG2$SpYear=year(Date_of_model)
    SpFDate=yday(Date_of_model)
    CoordSIG2$SpCDate=cos(SpFDate/365*2*pi) # to create a circular variable for date
    CoordSIG2$SpSDate=sin(SpFDate/365*2*pi) # to create a circular variable for date

    test=match(names(ModRF$ptype),names(CoordSIG2))
    MissingVar=subset(names(ModRF$ptype),is.na(test))
    print("missing:")
    print(MissingVar)
    if(length(MissingVar)>0)
    {
      for (j in 1:length(MissingVar))
      {
        CoordSIG2$temp=0
        names(CoordSIG2)[ncol(CoordSIG2)]=MissingVar[j]
      }
    }
    CoordSIG2[is.na(CoordSIG2)]=0
    
    
    # foreach(1:50, subCoordSIG=isplitRows(CoordSIG2, chunkSize=10), .packages='party') %dopar% 
    #   {
    
    # Make predictions ####
    print("Predicting activity...")
    #print(memory.size())
    PredLoc=predict(ModRF$finalModel,CoordSIG2)
    print("Predicting error...")
    #print(memory.size())
    PredAll=predict(ModRF$finalModel,CoordSIG2,predict.all=T)[[2]]
    #print(memory.size())
    PredErr=apply(PredAll,MARGIN=1,FUN=sd)
    print("Done!")
    # }
    
    # Transform as a spatial object
    CoordSIG_sf = CoordSIG2
    coordinates(CoordSIG_sf) <- c("X", "Y")
    proj4string(CoordSIG_sf) <- CRS("+init=epsg:4326") # WGS 84
    
    CoordSIG_sf$pred=PredLoc
    CoordSIG_sf$err=PredErr
    
    rm(PredLoc)
    rm(PredAll)
    rm(PredErr)
    
    #spplot(CoordSIG,zcol="pred",main=args[1])
    #spplot(CoordSIG,zcol="err")
    
    Coord=as.data.table(CoordSIG_sf)
    Coord=subset(Coord,select=c("X","Y","pred","err"))
    print("M76")
    
    # Save
    if(Simulation_ALAN){
      FilName=paste0(Output
                     ,Sp,"_Act_",Date_of_model, "_", suffix
                     ,"_NoALAN",basename(args[2]))
    }else{
      FilName=paste0(Output
                     ,Sp,"_Act_",Date_of_model, "_", suffix
                     ,"_",basename(args[2]))
    }
    
    print(FilName)
    
    fwrite(Coord,paste0(FilName,".csv"))
    
  }
  
}
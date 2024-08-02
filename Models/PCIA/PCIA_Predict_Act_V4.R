library(data.table)
library(randomForest)
library(spdep)
library(tidyverse)
library(pracma)

# Predicts activity using the random forest models

Threshold = "weighted" #weighted or 0 or 50 or 90
DateModel = "2024-07-15" #date of modelling (exactly same writing as the folder name)
CoordType = "rotated" # Types of coordinates? # "polar" or "rotated" or "noCoord" or "" if old model
nCoord = 50
MTRY = "default" # "default" or "npred" or "2-3" for 2/3 of npred
NTREE = 500
DuplicateDate = T # Duplicated dates
nDates = 50
Simulation_ALAN = F # If you want to simulate a full night everywhere

# List arguments #####
args="Barbar" #all species = "all", one species = e.g. "Pippip"
#args[2]="/mnt/beegfs/ybas/GI/GI_SysGrid__3e+05"
args[2]="/mnt/beegfs/croemer/VigieChiro/SIG/vieux_2023/SysGrid_500m_de_cote_FULL"
#args[2]="C:/Users/croemer01/Documents/Donnees vigie-chiro/SysGrid_500m_de_cote_FULL"
#args[2]="/mnt/beegfs/croemer/VigieChiro/SIG/GI_FR_SysGrid_500m_de_cote_part1"
#args[3]="2023-03-23" #date of modelling (exactly same writing as the folder name)
args[3]=DateModel 
args[4]=paste0("VC", Threshold, "PG_")
args[5]=Threshold 
args[6]="" # Use models from variable selection? yes : "_Boruta", no : ""
args[11]=40 #number of coordinates projections (must be a division of 360) : 20, 24, 30, 40, 90
#ModRF_Dir = "/mnt/beegfs/croemer/VigieChiro/ModPred/"
ModRF_Dir = "/mnt/beegfs/croemer/VigieChiro/ModPred/"
#ModRF_Dir = "C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/"
Output=paste0("/mnt/beegfs/croemer/VigieChiro/PredictionsModels/", args[5], "_", args[3], "/") #folder to copy models to 
#Output=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/PredictionsModels/", args[5], "_", args[3], "/") #folder to copy models to 
YearEffect=T
SpList = read_delim("/mnt/beegfs/croemer/VigieChiro/SpeciesList.csv", delim = ";")
#SpList = read_delim("C:/Users/croemer01/Documents/Donnees vigie-chiro/SpeciesList.csv", delim = ";")



if(args[6] == "_Boruta"){
  suffix=paste0("_Boruta_", CoordType, "_", NTREE, "_", MTRY)
}else{
  suffix=paste0(CoordType, "_", NTREE, "_", MTRY)
}

dir.create(Output)

if(args[1]=="all"|args[1]=="All"){
  ModRF_list = list.files(paste0(ModRF_Dir, args[4], args[3]), 
                          pattern=paste0(".+", "ActLog", ".+", args[6], suffix, ".learner"), full.names = T)
print(paste0(ModRF_Dir, args[4], args[3]))
  }else{
  ModRF_file=paste0(ModRF_Dir, "VC", args[5], "PG_", args[3], "/ModRFActLog_",
                    args[1],"VC",args[5], "_", DateModel, "_PG", args[6], suffix, ".learner")
  #args[1],"VC",args[5], "_", "2021-12-31", "_PG", args[6], CoordType, ".learner")
  ModRF_list = list(ModRF_file)
}

print(ModRF_list)

# Load table containing habitat variables
CoordSIG=fread(paste0(args[2],".csv")) 
print(nrow(CoordSIG))
CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpAltiS)==F)
CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpBioC1)==F)
print(nrow(CoordSIG))
print("M22")

# Cleans coordinates
CoordSIG = CoordSIG %>%
  rename_all(~str_replace_all(.,"\\.x",""))
CoordSIG <- CoordSIG %>% select(-contains(".y"))
#CoordDS=as.matrix(cbind(CoordSIG$Group.1,CoordSIG$Group.2))
#CoordDS=as.matrix(cbind(CoordSIG$X,CoordSIG$Y))

# for (a in 0:(as.numeric(args[11])-1))
# {
#   Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(args[11]))
#   #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
#   #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
#   CoordSIG=cbind(CoordSIG,Coordi[,1])
#   names(CoordSIG)[ncol(CoordSIG)]=paste0("SpCoord",a)
# }

print(CoordType)

if(CoordType == "rotated" | CoordType==""){
  #add several rotated coordinates
  CoordDS=as.matrix(cbind(CoordSIG$X,CoordSIG$Y)) #WGS84
  print("L230")
  
  for (a in 0:(as.numeric(args[11])-1))
  {
    Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(args[11]))
    
    CoordSIG=cbind(CoordSIG,Coordi[,1])
    names(CoordSIG)[ncol(CoordSIG)]=paste0("SpCoord",a)
  }
}

if(CoordType == "polar"){
  #add polar coordinates
  set.seed(130)
  CoordDS=as.data.frame(cbind("longitude" = CoordSIG$X, 
                              "latitude" = CoordSIG$Y)) #WGS84
  
  CoordDSL93=CoordDS %>% # convert to sf and L93
    st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
    st_crop(xmin = -5, xmax = 9, ymin = 41, ymax = 51) %>% 
    st_transform(2154) %>%
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2]) %>%
    as.data.frame %>%
    select(x, y) %>%
    as.matrix()
  
  lon = c(-5, 9)
  lat = c(41,51)
  FR_limits = data.frame(lon, lat) # define spatial delimitation of possible origins (bounding box of France)
  FR_limits_sf <- FR_limits %>%  
    st_as_sf(coords = c("lon", "lat"), 
             crs = 4326) %>% 
    st_transform(2154) %>%
    st_bbox() %>% 
    st_as_sfc() 
  
  set.seed(130) 
  SampOrigin <- st_sample(FR_limits_sf, nCoord) %>% # select an origin
    st_coordinates()

  #Origin = sample(nrow(CoordDSL93), nCoord) # select an origin
  
  for (b in 1:nCoord){
    #CoordDSL93_to_Origin = sweep(CoordDSL93, 2, CoordDSL93[Origin[b], ])
    CoordDSL93_to_Origin = sweep(CoordDSL93, 2, SampOrigin[b,])
    CoordDS_polar = cart2pol(CoordDSL93_to_Origin) %>% # calculate distance and angle from origin for each coordinates
      as.data.frame()
    SpCoordAngle = CoordDS_polar$phi
    SpCoordDistance = CoordDS_polar$r
    
    CoordSIG=cbind(CoordSIG, SpCoordAngle)
    names(CoordSIG)[ncol(CoordSIG)]=paste0("SpCoordAngle", b)
    CoordSIG=cbind(CoordSIG, SpCoordDistance)
    names(CoordSIG)[ncol(CoordSIG)]=paste0("SpCoordDistance", b)
  }
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

# For each species
for (k in 1:length(ModRF_list)){
  
  print(ModRF_list[k])
  
  Sp = SpList$Esp[which(str_detect(ModRF_list[k], SpList$Esp))]
  
  print(Sp)
  
  print("L14")
  load(as.character(ModRF_list[k])) # Load random forest model
  
  # For each date
  for (i in 1:length(Date_seq)){
    
    Date_of_model=Date_seq[i]
    print(Date_of_model)
    
    CoordSIG2 = CoordSIG
    
    SpFDate=yday(Date_of_model)
    CoordSIG2$SpCDate=cos(SpFDate/365*2*pi) # to create a circular variable for date
    CoordSIG2$SpSDate=sin(SpFDate/365*2*pi) # to create a circular variable for date
    
    # If duplicated date wanted
    if(DuplicateDate){
      sel_columns <- c(rep('SpCDate', nDates), rep('SpSDate', nDates))
      CoordSIG2 = as.data.frame(CoordSIG2)
      CoordSIG2 = cbind(CoordSIG2,
                         setNames(CoordSIG2[sel_columns], paste0(sel_columns, c(1:nDates))))
      print(dim(CoordSIG2))
      print(CoordSIG2[1,])
    }
    
    # If year effect is must be accounted for
    if(exists("YearEffect"))
    {
      if(YearEffect)
      {
        CoordSIG2$SpYear=year(Date_of_model)
      }
    }
    
    test=match(row.names(ModRF$importance),names(CoordSIG2))
    MissingVar=subset(row.names(ModRF$importance),is.na(test))
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
    print("M61")
    #print(memory.size())
    PredLoc=predict(ModRF,CoordSIG2)
    print("M63")
    #print(memory.size())
    PredAll=predict(ModRF,CoordSIG2,predict.all=T)[[2]]
    print("M65")
    #print(memory.size())
    PredErr=apply(PredAll,MARGIN=1,FUN=sd)
    print("M67")
    
    # }
    
    # Transform as a spatial object and use Group.1 and Group.2 as coordinates
    CoordSIG_sf = CoordSIG2
    #coordinates(CoordSIG_sf) <- c("Group.1", "Group.2")
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
    #Coord=subset(Coord,select=c("Group.1","Group.2","pred","err"))
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
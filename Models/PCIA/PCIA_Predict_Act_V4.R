library(data.table)
library(randomForest)
library(spdep)
library(tidyverse)
# library(itertools)
# library(foreach)

# Predicts activity using the random forest models

Threshold = "0" #weighted or 0 or 50 or 90
DateModel = "2023-11-17" #date of modelling (exactly same writing as the folder name)

# List arguments #####
args="Rhifer" #all species = "all", one species = e.g. "Pippip"
#args[2]="/mnt/beegfs/ybas/GI/GI_SysGrid__3e+05"
args[2]="/mnt/beegfs/croemer/VigieChiro/SIG/SysGrid_500m_de_cote_FULL"
#args[2]="/mnt/beegfs/croemer/VigieChiro/SIG/GI_FR_SysGrid_500m_de_cote_part1"
#args[3]="2023-03-23" #date of modelling (exactly same writing as the folder name)
args[3]=DateModel 
args[4]=paste0("VC", Threshold, "PG_")
args[5]=Threshold 
args[6]="" # Use models from variable selection? yes : "_Boruta", no : ""
args[11]=40 #number of coordinates projections (must be a division of 360) : 20, 24, 30, 40, 90
#ModRF_Dir = "/mnt/beegfs/croemer/VigieChiro/ModPred/"
ModRF_Dir = "/mnt/beegfs/croemer/VigieChiro/ModPred/"
Output=paste0("/mnt/beegfs/croemer/VigieChiro/PredictionsModels/", args[5], "_", args[3], "/") #folder to copy models to 
YearEffect=T
SpList = read_delim("/mnt/beegfs/croemer/VigieChiro/SpeciesList.csv", delim = ";")

dir.create(Output)

if(args[1]=="all"|args[1]=="All"){
  ModRF_list = list.files(paste0(ModRF_Dir, args[4], args[3]), 
                          pattern=paste0(".+", "ActLog", ".+", args[6], ".learner"), full.names = T)
}else{
  ModRF_file=paste0(ModRF_Dir, "VC", args[5], "PG_", args[3], "/ModRFActLog_",
                    args[1],"VC",args[5], "_2021-12-31_PG", args[6], ".learner")
  ModRF_list = list(ModRF_file)
}

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
CoordDS=as.matrix(cbind(CoordSIG$X,CoordSIG$Y))

for (a in 0:(as.numeric(args[11])-1))
{
  Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(args[11]))
  #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
  #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
  CoordSIG=cbind(CoordSIG,Coordi[,1])
  names(CoordSIG)[ncol(CoordSIG)]=paste0("SpCoord",a)
}

# Create date vector with a sample each 15 days
Date_seq = seq.Date(from = as.Date('2018-03-01'), to = as.Date('2018-10-31'), by = 'month')
Date_seq = c(Date_seq, (Date_seq+14))

CoordSIG$SpGite=0
CoordSIG$SpRecorder="SM2BAT+"

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
    
    CoordSIG$SpFDate=yday(Date_of_model)
    CoordSIG$SpCDate=cos(CoordSIG$SpFDate/365*2*pi) # to create a circular variable for date
    CoordSIG$SpSDate=sin(CoordSIG$SpFDate/365*2*pi) # to create a circular variable for date
    
    # If year effect is must be accounted for
    if(exists("YearEffect"))
    {
      if(YearEffect)
      {
        CoordSIG$SpYear=year(Date_of_model)
      }
    }
    
    test=match(row.names(ModRF$importance),names(CoordSIG))
    MissingVar=subset(row.names(ModRF$importance),is.na(test))
    print("missing:")
    print(MissingVar)
    if(length(MissingVar)>0)
    {
      for (j in 1:length(MissingVar))
      {
        CoordSIG$temp=0
        names(CoordSIG)[ncol(CoordSIG)]=MissingVar[j]
      }
    }
    CoordSIG[is.na(CoordSIG)]=0
    
    
    # foreach(1:50, subCoordSIG=isplitRows(CoordSIG, chunkSize=10), .packages='party') %dopar% 
    #   {
        
        # Make predictions ####
        print("M61")
        PredLoc=predict(ModRF,CoordSIG)
        print("M63")
        PredAll=predict(ModRF,CoordSIG,predict.all=T)[[2]]
        print("M65")
        PredErr=apply(PredAll,MARGIN=1,FUN=sd)
        print("M67")
        
      # }
    
    # Transform as a spatial object and use Group.1 and Group.2 as coordinates
    CoordSIG_sf = CoordSIG
    #coordinates(CoordSIG_sf) <- c("Group.1", "Group.2")
    coordinates(CoordSIG_sf) <- c("X", "Y")
    proj4string(CoordSIG_sf) <- CRS("+init=epsg:4326") # WGS 84
    
    CoordSIG_sf$pred=PredLoc
    CoordSIG_sf$err=PredErr
    
    #spplot(CoordSIG,zcol="pred",main=args[1])
    #spplot(CoordSIG,zcol="err")
    
    Coord=as.data.table(CoordSIG_sf)
    Coord=subset(Coord,select=c("X","Y","pred","err"))
    #Coord=subset(Coord,select=c("Group.1","Group.2","pred","err"))
    print("M76")
    
    # Save
    FilName=paste0(Output
                   ,Sp,"_Act_",Date_of_model
                   ,"_",basename(args[2]))
    
    print(FilName)
    
    fwrite(Coord,paste0(FilName,".csv"))
    
  }
  
}
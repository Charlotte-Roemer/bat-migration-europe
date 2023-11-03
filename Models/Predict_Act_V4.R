library(data.table)
library(randomForest)
library(spdep)
library(tidyverse)
library(beepr)

# Predicts activity using the random forest models

# List arguments #####
args="all" #all species = "all", one species = e.g. "Pippip"
args[2]="C:/Users/croemer01/Documents/Donnees vigie-chiro/GI_SysGrid__3e+05"
args[3]="2023-03-16" #date of prediction (exactly same writing as the folder name)
args[5]="weighted"
args[6]="" # Use models from variable selection? yes : "_Boruta", no : ""
args[11]=40 #number of coordinates projections (must be a division of 360)
ModRF_Dir = "C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/"
ModRF_file=paste0(ModRF_Dir, "VC", args[5], "PG_", args[3], "/ModRFActLog_",args[1],"VC",args[5],"_PG", args[6], ".learner")
Output=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/PredictionsModels/", args[5], "_", args[3], "/") #folder to copy models to 

dir.create(Output)

if(args[1]=="all"|args[1]=="All"){
  ModRF_list = list.files(paste0(ModRF_Dir, "VCweightedPG_", args[3]), 
                          pattern=paste0("*", args[6], ".learner"), full.names = T)
}else{
  ModRF_list = list(ModRF_file)
}

for (k in 1:length(ModRF_list)){
  
  Sp = substr(ModRF_list[k], 13, 18)
  print(Sp)
  
  print("L14")
  load(ModRF_list[k]) # Load random forest model
  CoordSIG=fread(paste0(args[2],".csv")) # Load table containing habitat variables
  print(nrow(CoordSIG))
  CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpAltiS)==F)
  CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpBioC1)==F)
  print(nrow(CoordSIG))
  print("M22")
  
  # Cleans coordinates
  CoordSIG = CoordSIG %>%
    rename_all(~str_replace_all(.,"\\.x",""))
  CoordSIG <- CoordSIG %>% select(-contains(".y"))
  CoordDS=as.matrix(cbind(CoordSIG$Group.1,CoordSIG$Group.2))
  
  for (a in 0:(as.numeric(args[11])-1))
  {
    Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(args[11]))
    #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
    #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
    CoordSIG=cbind(CoordSIG,Coordi[,1])
    names(CoordSIG)[ncol(CoordSIG)]=paste0("SpCoord",a)
  }
  
  # Create date vector with a sample each 15 days
  Date_seq = seq.Date(from = as.Date('2018-01-01'), to = as.Date('2018-12-31'), by = 'month')
  Date_seq = c(Date_seq, (Date_seq+14))
  
  # For each date, make a prediction
  for (i in 1:length(Date_seq)){
    
    Date_of_model=Date_seq[i]
    print(Date_of_model)
    
    CoordSIG$SpGite=0
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
    
    # Make predictions ####
    print("M61")
    PredLoc=predict(ModRF,CoordSIG)
    print("M63")
    PredAll=predict(ModRF,CoordSIG,predict.all=T)[[2]]
    print("M65")
    PredErr=apply(PredAll,MARGIN=1,FUN=sd)
    print("M67")
    
    # Transform as a spatial object and use Group.1 and Group.2 as coordinates
    CoordSIG_sf = CoordSIG
    coordinates(CoordSIG_sf) <- c("Group.1", "Group.2")
    proj4string(CoordSIG_sf) <- CRS("+init=epsg:4326") # WGS 84
    
    CoordSIG_sf$pred=PredLoc
    CoordSIG_sf$err=PredErr
    
    #spplot(CoordSIG,zcol="pred",main=args[1])
    #spplot(CoordSIG,zcol="err")
    
    Coord=as.data.table(CoordSIG_sf)
    Coord=subset(Coord,select=c("Group.1","Group.2","pred","err"))
    print("M76")
    
    # Save
    FilName=paste0(Output
                   ,Sp,"_Act_",Date_of_model
                   ,"_",basename(args[2]))
    
    print(FilName)
    
    fwrite(Coord,paste0(FilName,".csv"))
    
  }
}

beep(2)

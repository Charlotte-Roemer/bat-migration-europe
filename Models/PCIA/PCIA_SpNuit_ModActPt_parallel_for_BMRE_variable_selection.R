
# To create random forest models (.learner files) of bat activity

library(data.table)
#library(raster)
#library(rgdal)
#library(rgeos)
#library(latticeExtra)
library(randomForest)
library(gdata)
library(spdep) # for Rotation function
library(tidyverse)
library(pgirmess)
library(sp)
library(sf)
library(hydroGOF)
library(rsample)
library(foreach)
library(doParallel)
library(pracma)
#library(Boruta)

#to show milliseconds
op <- options(digits.secs=3)

# Sorting threshold (weighted, 0, 50, 90)
ThresholdSort = "weighted"
print(ThresholdSort)

# choose species
Sp = "all" # choose a species (e.g. "Pippip") or "all" or "paper"
GroupSel="bat"
#GroupSel=NA #sorting according to the group column of Specieslist (args[3), NA if no sorting
ListPaper = c("Nyclei", "Nycnoc", "Eptser", "Pipkuh", "Pipnat", 
              "Pippip", "Minsch", "Pippyg", "Barbar", "Rhifer")

# Do variable selection?
DoBoruta = F

# Duplicated dates
DuplicateDate = F

# Types of coordinates? # "polar" or "rotated"
CoordType = "rotated"

# if(ThresholdSort != "weighted"){
#   args=paste0("/mnt/beegfs/ybas/VigieChiro/Raw/SpNuit2_", ThresholdSort, "_DataLP_PF_exportTot") #bat activity table (not DI !! --> need the file where microphone quality is sorted out) . file without csv extension
# }else{
args=paste0("/mnt/beegfs/croemer/VigieChiro/Raw/SpNuit2_", ThresholdSort, "_DataLP_PF_exportTot") #bat activity table (not DI !! --> need the file where microphone quality is sorted out) . file without csv extension
# }
#args[2]="/mnt/beegfs/ybas/GI/GI_sites_localites" #table with spatial variables (habitat and climate)
args[2]="/mnt/beegfs/croemer/VigieChiro/GI_FR_sites_localites" #table with spatial variables (habitat and climate)
args[3]="/mnt/beegfs/croemer/VigieChiro/SpeciesList.csv" # Species list to build models
#args[3]=NA #NA if we want all species without filter (but specify args[5)
#args[4]="Esp" #name of taxa column (useless if args[3] is specified)
args[4]="espece" #name of taxa column (useless if args[3] is specified)
#args[4]="code_sp" #name of taxa column (useless if args[3] is specified)
#args[5]="STRTUR" #name of taxa group (useless if args[3] is specified)
DataLoc=F #TRUE if coordinates are in table args[1
#CoordinateNames=c("longitude_wgs84","latitude_wgs84") 
#CoordinateNames=c("longitude","latitude") #name of columns with coordinates in the locality table (sites_localites.txt)
CoordinateNames=c("X","Y") #name of columns with coordinates in the locality table (sites_localites.txt)
args[6]="participation" #name of sampling event
args[7]="localite" #name of locality in CoordSIG (if DataLoc=T)
args[8]="participation" #name of participation (=sampling event)
args[9]=T #if date (=day-of-year) is provided or not
#args[10]="abondance"
args[10]="nb_contacts_nd" #the name of the parameter which gives the metric to predict
args[11]=40 #number of coordinates projections (must be a division of 360) : 20, 24, 30, 40, 45, 60, 72, 90
#args[12]="C:/Users/croemer01/Documents/Donnees vigie-chiro/Tab_sounds_all_50ScriptLea.csv" # table with bat activity (bat passes)
MinData=1
DM=T #option if you also want a model to predict minimum time lapse between bat passes and sunset and sunrise
Output=paste0("/mnt/beegfs/croemer/VigieChiro/ModPred/VC", ThresholdSort, "PG_", Sys.Date()) #folder to copy models to (fichiers .learner), no "_" else bug !!!
Tag=paste0("VC", ThresholdSort) #tag which will be written in the filename, no "_", else bug !!!
effectYear=F # option to add a year effect: to predict population trends
varYear="annee" #name of the year variable (needless if effectYear=F)
W0=F #whether the table args[1 contains the 0 bat passes/night
MergedGI=F #whether habitat-climate variables are in the table args[1 
Fpar="/mnt/beegfs/ybas/VigieChiro/p_export.csv" #the file with data about participations
Fsl="/mnt/beegfs/ybas/VigieChiro/sites_localites.txt"	#the file with the data about localities
ProbThreshold=0 # a filter on the score_max parameter (takes all data superior or equal to this value)
#min_dist = 200 # geographical distance in meters to create a custom test and train dataset
#reps_process = 1 # how many trials should be made to sort train/test dataset (see buffer_CR.r)
YearEffect=T
MTRY = "npred" # "default" or "npred" ( = length(Predictors_Train) )
DateLimit = Sys.Date()  # e.g.as.Date("2021-12-31") to use only data before this date; default = Sys.Date()

dir.create(Output)

# Reads bat activity data
DataCPL2  = fread(paste0(args[1],".csv"))
DataCPL2$Nuit=as.Date(DataCPL2$Nuit)
DataCPL3= DataCPL2 %>% 
  dplyr::filter(Nuit < DateLimit)

if (!("score_max" %in% names(DataCPL3))){ # not very useful
  DataCPL3 = DataCPL3 %>% 
    group_by(participation, Nuit, num_micro) %>% 
    mutate(score_max = max(as.numeric(nb_contacts))) %>% 
    as.data.frame()
}

#Reads the spatial variables
Sys.time()
CoordSIG=fread(paste0(args[2],".csv"))
#print((CoordSIG)[1,])
CoordSIG = CoordSIG %>% 
  rename(longitude = CoordinateNames[1], latitude = CoordinateNames[2])
#print((CoordSIG)[1,])

CoordSIG = CoordSIG %>%
  rename_all(~str_replace_all(.,"\\.x",""))
CoordSIG <- CoordSIG %>% select(-contains(".y"))
Sys.time()

if(!MergedGI){ # If habitat-climate variables are not in the bat activity table
  
  if(!DataLoc) # If coordinates are not in the bat activity table
  {
    #reads participation data
    Particip=read_delim(Fpar,delim=";")
    #reads locality data
    SiteLoc=fread(Fsl)
    Gite=mapply(function(x,y)
      ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
      ,SiteLoc$commentaire
      ,SiteLoc$localite)
    SiteLoc$SpGite=as.numeric(Gite)
    SiteLoc = SiteLoc %>% 
      mutate_at(.vars = c("longitude", "latitude"), 
                .fun = function(x) as.numeric(gsub(",", "\\.", x)))
    
    #list of coordinates existing in this dataset to help add 0 in nb_contacts later
    ListPar=levels(as.factor(DataCPL3$participation))
    SelPar=subset(Particip,Particip$participation %in% ListPar)
    SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))
    CoordPar=aggregate(SelParSL$participation
                       ,by=c(list("longitude" = SelParSL$longitude),
                             list("latitude" = SelParSL$latitude),
                             list("participation" = SelParSL$participation))
                       ,FUN=length)
    CoordPar$x=NULL
    
  }else{
    print("L103")
    if(W0){ # if bat activity table contains 0
      DataTot=merge(DataCPL3,CoordSIG,by=c("longitude","latitude"))
    }else{
      ColCode2=match(args[7],names(DataCPL3))
      ColCode3=match(args[8],names(DataCPL3))
      DataCPL3$participation=as.data.frame(DataCPL3)[,ColCode3]
      DataCPL3$localite=as.data.frame(DataCPL3)[,ColCode2]
      SelParSL=subset(DataCPL3,select=c("participation","localite"))
      print("L108")
      SelParSL=unique(SelParSL)
      CoordPar0=subset(CoordSIG,select=c("longitude","latitude",args[6]))
      CoordPar=merge(CoordPar0,SelParSL,by.x=args[6],by.y="localite")
      names(CoordPar)[4]="participation"
      
    }
  }
  
  
  if(!W0){ # if bat activity table does not contains 0
    CoordPS=merge(CoordPar,CoordSIG,by=c("longitude","latitude"))
    
    test=(is.na(CoordPS))
    test2=apply(test,MARGIN=1,sum)
    test3=apply(test,MARGIN=2,sum)
    
    CoordPS[is.na(CoordPS)]=0
    
    testPar=grepl(args[6],names(CoordPS))
    numPar=subset(c(1:length(testPar)),testPar)
    print("L149")
    CoordPS$participation=as.data.frame(CoordPS)[,numPar[1]]
  }
}

print("L152")
print(args[3]!="NA")

if(args[3]!="NA") # if species list is provided
{
  SpeciesList=fread(args[3]) # read species list
  ListSp=levels(as.factor(DataCPL3$espece))
  ListSp=subset(ListSp,ListSp %in% SpeciesList$Esp)
  
}else{
  Group=args[5] # use group instead of species
  colTaxa=match(args[4],names(DataCPL3))
  DataCPL3$espece=as.data.frame(DataCPL3)[,colTaxa]
  Esp=unique(as.data.frame(DataCPL3)[,colTaxa])
  ListSp=levels(as.factor(Esp))
  Metric=args[10]
  DataCPL3$nb_contacts=subset(DataCPL3,select=Metric)
  SpeciesList=data.table(cbind(Group,Esp))
  fwrite(SpeciesList,paste0("SpeciesList_",Group,substr(Sys.time(),1,10),".csv"))
}

# Metric is the variable to predict
Metric=args[10]
DataCPL3$nb_contacts=subset(DataCPL3,select=Metric)[,1]
print("L183")
print(nrow(DataCPL3))
DataCPL3=subset(DataCPL3,!is.na(DataCPL3$nb_contacts))
print(nrow(DataCPL3))


if(!is.na(GroupSel))
{
  SpSel=subset(SpeciesList,SpeciesList$Group %in% GroupSel)
  ListSp=subset(ListSp,ListSp %in% SpSel$Esp)
}

if(Sp == "all" | Sp == "All"){
  ListSp = ListSp
}else if(Sp == "paper") {
  ListSp = ListPaper
}else{
  ListSp = Sp
}

############################################################################################
#### Select Species ####
print("L190")
print(ListSp)
for (i in 1:length(ListSp))
{
  DataSp=subset(DataCPL3,DataCPL3$espece==ListSp[i]) # subset species
  #DataSp=subset(DataCPL3,DataCPL3$espece==Sp) # subset species
  print(ListSp[i])
  print("L195")
  if(W0){ # if bat activity table contains 0
    DataSaison=DataSp
  }else{
    
    # Adds 0 counts using the observation table (avoids user errors but makes the
    # assumption that this table always contains at least 1 species per night)
    DataCPL3_unique=DataCPL3 %>% # prepares the table of the complete set of sampled nights/sites
      select(participation, Nuit, num_micro) %>% 
      unique()
    DataCPL3_unique$Nuit=as.Date(DataCPL3_unique$Nuit)
    
    DataSp$Nuit=as.Date(DataSp$Nuit)
    DataSpSL_w0_2=full_join(DataSp, DataCPL3_unique) # Adds the nights with absence
    colnames(DataSpSL_w0_2)[which(colnames(DataSpSL_w0_2)=="point")]="nom"
    
    n <- names(SelParSL) 
    DataSpSL_w0_2 = DataSpSL_w0_2[SelParSL, on=.(participation), (n):=mget(paste0("i.", n))] # performs a partial join (updates columns of DataSpSL_w0_2 with info of SelParSL)
    
    #DataSpSL_w0_2=left_join(DataSpSL_w0_2, SelParSL, by="participation") # adds all other info
    DataSpSL_w0_2$nb_contacts[is.na(DataSpSL_w0_2$nb_contacts)]=0
    DataSpSL_w0_2$score_max[is.na(DataSpSL_w0_2$score_max)]=0
    
    # Exclude sites outside France limits (square) :
    DataSpSL_w0_2=subset(DataSpSL_w0_2, DataSpSL_w0_2$longitude<10 & DataSpSL_w0_2$longitude>-6 &
                           DataSpSL_w0_2$latitude<52 & DataSpSL_w0_2$latitude>41)
    
    # Exclude data with obvious wrong date (<2010)
    DataSpSL_w0_2 = DataSpSL_w0_2[which(DataSpSL_w0_2$Nuit>as.Date("2010-01-01")),]
    
    DataSaison=full_join(DataSpSL_w0_2,CoordPS) # adds environmental variables to activity data
    
    print(Sys.time())
  }
  print("L213")
  
  #add date of year
  if(args[9])
  {
    if(grepl("/", DataSaison$Nuit[1])){
      Date1=as.Date(substr(DataSaison$Nuit,1,10)
                    ,format="%Y/%m/%Y")    
    }else{
      Date1=as.Date(DataSaison$Nuit)
    }
    
    SpFDate=yday(Date1)
    DataSaison$SpCDate=cos(SpFDate/365*2*pi) # to create a circular variable for date
    DataSaison$SpSDate=sin(SpFDate/365*2*pi) # to create a circular variable for date
  }else{
    DataSaison$SpCDate=0
    DataSaison$SpSDate=0
    print("L225")
  }
  
  # # If duplicated date wanted
  # if(DuplicateDate){
  #   sel_columns <- c(rep('SpCDate', 100), rep('SpSDate', 100))
  #   DataSaison = as.data.frame(DataSaison)
  #   DataSaison = cbind(DataSaison, 
  #                setNames(DataSaison[sel_columns], paste0(sel_columns, c(1:100))))
  #   print(dim(DataSaison))
  #   print(DataSaison[1,])
  # }
  
  # If year effect must be accounted for
  if(exists("YearEffect"))
  {
    if(YearEffect)
    {
      DataSaison$SpYear=year(Date1)
    }
  }
  
  if(CoordType == "rotated"){
    #add several rotated coordinates
    CoordDS=as.matrix(cbind(DataSaison$longitude,DataSaison$latitude)) #WGS84
    print("L230")
    
    for (a in 0:(as.numeric(args[11])-1))
    {
      Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(args[11]))
      
      DataSaison=cbind(DataSaison,Coordi[,1])
      names(DataSaison)[ncol(DataSaison)]=paste0("SpCoord",a)
    }
  }else if(CoordType == "polar"){
    #add polar coordinates
    print(CoordType)
    CoordDS=as.data.frame(cbind("longitude" = DataSaison$longitude, 
                                "latitude" = DataSaison$latitude)) #WGS84
    CoordDSL93=CoordDS %>% 
      st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
      st_transform(2154) %>%
      mutate(x = st_coordinates(.)[,1],
             y = st_coordinates(.)[,2]) %>%
      as.data.frame %>%
      select(x, y) %>%
      as.matrix()
    print("L230")
    
    CoordDS_polar = cart2pol(CoordDSL93) %>% 
      as.data.frame()
    
    SpCoordAngle = CoordDS_polar$phi
    SpCoordDistance = CoordDS_polar$r
      
    DataSaison=cbind(DataSaison, SpCoordAngle, SpCoordDistance)
  }
  
  
  # Add material as predictor
  DataSaison$SpRecorder = DataSaison$detecteur_enregistreur_type
  
  # Identify predictors
  testPred=(substr(names(DataSaison),1,2)=="Sp")
  Prednames=names(DataSaison)[testPred]
  #print(Prednames)
  
  # Do not use species distribution area yet
  ListSpeciesDistribution = c("SpBarbar",  "SpMinpal", "SpMinsch", "SpMyoalc", "SpMyobec", "SpMyobly", 
                              "SpMyobra", "SpMyocap", "SpMyodas", "SpMyodau", "SpMyodav", "SpMyoema", 
                              "SpMyoesc", "SpMyomyo", "SpMyomys", "SpMyonat", "SpMyopun", "SpMyosch", 
                              "SpNyclas", "SpNyclei", "SpNycnoc", "SpPiphan", "SpPipkuh", "SpPipnat", 
                              "SpPippip", "SpPippyg", "SpPleaur", "SpPleaus", "SpPlechr", "SpPlekol", 
                              "SpPlemac", "SpPlesar", "SpRhibla", "SpRhieur", "SpRhifer", "SpRhihip", 
                              "SpRhimeh", "SpTadten", "SpVesmur", "SpEptana", "SpEptbot", "SpEptisa",
                              "SpEptnil", "SpEptser", "SpHypsav")
  Prednames = Prednames[which(!Prednames %in% ListSpeciesDistribution )]
  
  # If duplicated date wanted
  if(DuplicateDate){
    sel_columns <- c(rep('SpCDate', 99), rep('SpSDate', 99))
    Prednames = c(Prednames, sel_columns)
    #print(Prednames)
  }
  
  Predictors=DataSaison[,..Prednames]
  
  #print(Predictors[1,])
  
  DataSaison = DataSaison %>%  
    drop_na(all_of(Prednames)) %>% #deletes rows without predictor (outdated GI table)
    drop_na(nb_contacts) #deletes rows without contacts (people did not upload their data)
  
  # Statistics for paper
  Stat1 = DataSaison %>% 
    group_by(latitude, longitude, nom) %>% 
    count()
  print(paste0("N opportunistic sites = ", length(which(grepl("Z", Stat1$nom))), 
               " over a total of ", nrow(Stat1), " sites"))
  print(paste0("N opportunistic nights = ", length(which(grepl("Z", DataSaison$nom))), 
               " over a total of ", nrow(DataSaison), " nights"))
  
  #seasonal subset
  #  DataSaison=subset(DataSaison,substr(DataSaison$`date part. debut`,4,5) %in% Saison)
  
  # print((sum(DataSaison$nb_contacts)>=MinData))
  # if(sum(DataSaison$nb_contacts)>=MinData) # if the number of observations is superior to MinData
  # {
  # DataSPos=subset(DataSaison,DataSaison$nb_contacts>0)
  
  # print("L246")
  # if(effectYear) # if you want to calculate population trends
  # {
  #   DataYear=subset(DataSaison,select=varYear)
  #   names(DataYear)="year"
  #   DataSaison$SpYear=DataYear$year
  # }
  
  testNA=apply(Predictors,MARGIN=2,FUN=function(x) sum(is.na(x)))
  print(summary(testNA))
  testNA2=apply(Predictors,MARGIN=1,FUN=function(x) sum(is.na(x)))
  print(summary(testNA2))
  
  #threshold on score
  print(summary(DataSaison$score_max))
  print(summary(DataSaison$score_max>ProbThreshold))
  DataSaison$nb_contacts=ifelse(DataSaison$score_max>ProbThreshold,DataSaison$nb_contacts,0)
  
  DataSaison$ActLog10=log10(DataSaison$nb_contacts+1) #pas sur que ce soit pertinent
  print(summary(DataSaison$ActLog10))
  Sys.time()
  
  # Find Boruta formula (variable)
  if(DoBoruta == T){
    Dataset.Boruta = data.frame("ActLog10" = DataSaison$ActLog10, DataSaison[,..Prednames])
    ModRFTemp.Boruta <- Boruta(formula("ActLog10 ~."), # Build model
                               data = Dataset.Boruta,
                               doTrace = 2, ntree = 500, maxRuns = 100)
    formula.Boruta = try(getConfirmedFormula(ModRFTemp.Boruta)) # retrieve formula of selected variables if at least one was selected (error if none is selected)
    if(inherits(formula.Boruta, "try-error")){
      formula.Boruta = formula("ActLog10 ~.")
      errorlog = data.frame("message" = paste0("Boruta ended by not selecting any predictor for model ", ListSp[i]))
      fwrite(errorlog, paste0(Output,"/", ListSp[i], "_",Tag,"_log.txt"))
    }else{
      formula.Boruta = getConfirmedFormula(ModRFTemp.Boruta)
      names.Boruta = getSelectedAttributes(ModRFTemp.Boruta)
    }
  }else{
    formula.Boruta = formula("ActLog10 ~.")
  }
  
  # Custom combine function for datasets of different lengths (used at the end of the loop)
  source(paste("/trinity/home/croemer/scripts_VC/","My_combine.R",sep=""))
  
  multiResultClass <- function(simFinal=NULL, obsFinal=NULL, Test_infoFinal=NULL, ModRF=NULL) # for parallel run
  {
    me <- list(
      simFinal = simFinal,
      obsFinal = obsFinal,
      Test_infoFinal = Test_infoFinal,
      ModRF = ModRF
    )
    
    ## Set the name for the class
    class(me) <- append(class(me),"multiResultClass")
    return(me)
  }
  
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  print("L427")
  
  obsFinal=NULL
  simFinal=NULL
  Test_infoFinal=NULL
  rm(ModRF)
  START=Sys.time()
  oper <- foreach (j =1:500) %dopar% {
    
    library(randomForest)
    
    print(j)
    
    # create train/test datasets here
    START=Sys.time()
    source(paste("/trinity/home/croemer/scripts_VC/","PCIA_use_buffer_to_create_train_and_test_datasets.R",sep=""), local = TRUE)
    END=Sys.time()
    TIMEDIFF=END-START
    TIMEDIFF
    #This script is quite long
    
    print("A")
    
    # WARNING : if lot of NA in Predictors : many sites are lacking in CoordGIS. Need to update the table.
    
    DataSaison_Train= DataSaison %>%
      left_join(xy93.3, by = c("longitude", "latitude")) %>%
      select(!c(longitude.2, latitude.2)) %>%
      filter(type=="train" & !is.na(type))
    
    DataSaison_Test= DataSaison %>%
      left_join(xy93.3, by = c("longitude", "latitude")) %>%
      select(!c(longitude.2, latitude.2)) %>%
      filter(type=="test" & !is.na(type))
    
    Predictors_Train=DataSaison_Train %>%
      select(all_of(Prednames))
    
    Predictors_Test=DataSaison_Test %>%
      select(all_of(Prednames))
    
    print("B")
    
    # # Check that the dataset is fine
    # ggplot(DataSaison_Train,
    #        aes(x=longitude, y=latitude)) +
    #   geom_point(color="blue", size=0.5) +
    #   geom_point(data= DataSaison_Test, aes(x=longitude, y=latitude), color="red", size=0.5)
    # 
    #### Random forest model for number of bat passes per night ####
    Dataset.RF = data.frame("ActLog10" = DataSaison_Train$ActLog10, Predictors_Train)
    
    if(MTRY=="default"){
      ModRFTemp<-randomForest(formula.Boruta, data=Dataset.RF, 
                              replace=F, 
                              ntree = 1, 
                              strata=paste(DataSaison_Train$id_site,DataSaison_Train$localite), 
                              importance=F)
    }else{
      ModRFTemp<-randomForest(formula.Boruta, data=Dataset.RF, 
                              replace=F, 
                              ntree = 1, 
                              mtry=length(Predictors_Train),
                              strata=paste(DataSaison_Train$id_site,DataSaison_Train$localite), 
                              importance=F)
    }
    
    print("C")
    
    sim<-predict(ModRFTemp,newdata=Predictors_Test, type="response")
    obs<-DataSaison_Test$ActLog10
    Test_info=DataSaison_Test %>%
      select(participation, Nuit, num_micro, longitude, latitude)
    
    print("D")
    
    result <- multiResultClass()
    print("E")
    result$simFinal <- sim
    result$obsFinal <- obs
    result$Test_infoFinal <- Test_info
    result$ModRF <- ModRFTemp
    result$N_tree <- rep(j, length(obs))
    
    # print("F")
    # result1 = data.frame(a=c(1,2), b=c(3,4))
    # write.table(result1, file=paste(Output, "/RF_ntree_",j,".txt", sep=""),
    #             sep="\t", row.names=F)
    
    return(result)
    
  }
  
  END=Sys.time()
  
  stopCluster(cl)
  
  print("L521")
  
  rm(ModRF)
  rm(Dataframe_simobs_Final)
  NRMSE=NULL
  # Combine trees
  for(k in 1: length(oper)){
    ModRFTemp2 = oper[[k]]$ModRF
    if (exists("ModRF")==TRUE) {ModRF=my_combine(ModRF,ModRFTemp2)} else {ModRF=ModRFTemp2}
    
    Dataframe_simobs_Temp <- data.frame("N_tree" = oper[[k]]$N_tree, "simFinal" = oper[[k]]$simFinal, 
                                        "obsFinal" = oper[[k]]$obsFinal, "Test_infoFinal" = oper[[k]]$Test_infoFinal )
    
    if (exists("Dataframe_simobs_Final")==TRUE) {
      Dataframe_simobs_Final=rbind(Dataframe_simobs_Final,Dataframe_simobs_Temp)
    } else {Dataframe_simobs_Final=Dataframe_simobs_Temp}
    
    NRMSE_Temp = nrmse(oper[[k]]$simFinal, oper[[k]]$obsFinal, norm = "maxmin")
    if (exists("NRMSE")==TRUE) {
      NRMSE=rbind(NRMSE,NRMSE_Temp)
    } else {NRMSE=NRMSE_Temp}
    
  }
  
  #Dataframe_simobs_Final<-data.frame(N_tree, simFinal, obsFinal, Test_infoFinal)
  Dataframe_simobs_Final$diffSimObs=abs(Dataframe_simobs_Final$simFinal-Dataframe_simobs_Final$obsFinal)
  Dataframe_simobs_Final$simFinalBT=10^(Dataframe_simobs_Final$simFinal)#backtransform
  Dataframe_simobs_Final$obsFinalBT=10^(Dataframe_simobs_Final$obsFinal)#backtransform
  
  
  if(DoBoruta == T){
    suffix=paste0("_Boruta_", CoordType)
  }else{
    suffix=CoordType
  }
  
  fwrite(Dataframe_simobs_Final,paste0(Output,"/ModRFActLog_", ListSp[i], "_",
                                       Tag,"_", DateLimit, "_evaluation", suffix, ".csv"))
  
  fwrite(as.data.frame(NRMSE),paste0(Output,"/ModRFActLog_", ListSp[i], "_",
                                     Tag,"_", DateLimit,"_NRMSE", suffix, ".csv"))
  
  # varImpPlot(ModRF,cex=0.5,main=paste("Act",ListSp[i]))
  # print(paste("PseudoR2: ",ModRF$rsq[ModRF$ntree]))
  # 
  save (ModRF,file=paste0(Output,"/ModRFActLog_", ListSp[i]
                          ,Tag,"_", DateLimit
                          ,"_PG", suffix, ".learner"))
  
  
  
  
}

Sys.time()

END-START

print(ListSp[i])
print(ThresholdSort)
if(DoBoruta){
  print(paste0("Variables before selection = ", length(Predictors)))
  print(paste0("Variables after selection = ", length(names.Boruta)))
}


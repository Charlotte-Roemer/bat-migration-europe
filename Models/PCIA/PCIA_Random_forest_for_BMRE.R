
# To create random forest models of bat activity
# N.B. : il faudra faire tourner le modèle sur les données non confidentielles 
# afin de pouvoir publier le script et les données

# This script is adapted from Mila et al. (preprint)

library(data.table)
library(randomForest)
library(gdata)
library(tidyverse)
library(sf)
library(CAST)
library(Boruta)
#library(pgirmess)
#library(sp)
#library(rsample)
#library(doParallel)
library(beepr)

Place = "PCIA" # local, PCIA or IN2P3

#### Options ####--------------------------------------------------------

# Sorting threshold (weighted, 0, 50, 90)
ThresholdSort = "weighted"
print(ThresholdSort)

# Species to model
Sp = "Rhifer" # choose a species (e.g. "Pippip") or "all" or "paper"
GroupSel="bat"
#GroupSel=NA #sorting according to the group column of Specieslist (args[3), NA if no sorting
ListPaper = c("Minsch", "Barbar", "Nyclei", "Nycnoc", "Eptser", "Pipkuh", "Pipnat", 
              "Pippip", "Pippyg", "Rhifer")

# Filter data by date?
DateLimit = Sys.Date()  # e.g.as.Date("2021-12-31") to use only data before this date; default = Sys.Date()

# Predictors and model specs
CoordType = "EDF" # Spatial proxies in predictors: "LongLat" = X + Y ; "EDF" = X + Y + Euclidian Distance Fields ;  "noCoord" = no coordinates
YearEffect=T # Add year?
#MTRY = "default"  # "default" or "npred" or "2-3" for 2/3 of npred
NTREE = 500

# Do variable selection?
DoBoruta = T



#### Directories ####-----------------------------------------------------------

if(Place == "local"){
  args=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/SpNuit2_", ThresholdSort, "_DataLP_PF_exportTot") #bat activity table (not DI !! --> need the file where microphone quality is sorted out) . file without csv extension
  args[2]="C:/Users/croemer01/Documents/Donnees vigie-chiro/GI_FR_sites_localites" #table with spatial variables (habitat and climate)
  args[3]="C:/Users/croemer01/Documents/Donnees vigie-chiro/SpeciesList.csv" # Species list to build models
  args[4]="C:/Users/croemer01/Documents/SIG/Delimitations_pays" # france limits
  Output=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VC", ThresholdSort, "_", Sys.Date()) #folder to copy models to (fichiers .learner), no "_" else bug !!!
  Fpar="C:/Users/croemer01/Documents/Donnees vigie-chiro/p_export.csv" #the file with data about participations
  Fsl="C:/Users/croemer01/Documents/Donnees vigie-chiro/sites_localites.txt"	#the file with the data about localities
  sfolds_source = paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VC", ThresholdSort, "_", Sys.Date(), "/temp_sfolds.rds")
  source(paste("C:/Users/croemer01/Documents/R/bat-migration-europe/bat-migration-europe/Models/PCIA/","RF_prepare_data.R",sep=""))
  source(paste("C:/Users/croemer01/Documents/R/bat-migration-europe/bat-migration-europe/Models/PCIA/","RF_functions.R",sep=""))
  
}

if(Place == "PCIA"){
  if(ThresholdSort != "weighted"){
    args=paste0("/mnt/beegfs/ybas/VigieChiro/Raw/SpNuit2_", ThresholdSort, "_DataLP_PF_exportTot") #bat activity table (not DI !! --> need the file where microphone quality is sorted out) . file without csv extension
  }else{
    args=paste0("/mnt/beegfs/croemer/VigieChiro/Raw/SpNuit2_", ThresholdSort, "_DataLP_PF_exportTot") #bat activity table (not DI !! --> need the file where microphone quality is sorted out) . file without csv extension
  }
  #args[2]="/mnt/beegfs/ybas/GI/GI_sites_localites" #table with spatial variables (habitat and climate)
  args[2]="/mnt/beegfs/croemer/VigieChiro/GI_FR_sites_localites" #table with spatial variables (habitat and climate)
  args[3]="/mnt/beegfs/croemer/VigieChiro/SpeciesList.csv" # Species list to build models
  args[4]="/mnt/beegfs/croemer/VigieChiro/SIG" # france limits
  Output=paste0("/mnt/beegfs/croemer/VigieChiro/ModPred/VC", ThresholdSort, "_", Sys.Date()) #folder to copy models to (fichiers .learner), no "_" else bug !!!
  Fpar="/mnt/beegfs/ybas/VigieChiro/p_export.csv" #the file with data about participations
  Fsl="/mnt/beegfs/ybas/VigieChiro/sites_localites.txt"	#the file with the data about localities
  sfolds_source = paste0("/trinity/home/croemer/VigieChiro/ModPred/VC", ThresholdSort, "_", Sys.Date(), "/temp_sfolds.rds")
  source(paste("/trinity/home/croemer/scripts_VC/","RF_prepare_data.R",sep=""))
  source(paste("/trinity/home/croemer/scripts_VC/","RF_functions.R",sep=""))
  
}

if(Place == "IN2P3"){
  args=paste0("/pbs/home/c/croemer/Donnees/Raw/SpNuit2_", ThresholdSort, "_DataLP_PF_exportTot") #bat activity table (not DI !! --> need the file where microphone quality is sorted out) . file without csv extension
  args[2]="/pbs/home/c/croemer/Donnees/GI_FR_sites_localites" #table with spatial variables (habitat and climate)
  args[3]="/pbs/home/c/croemer/Donnees/SpeciesList.csv" # Species list to build models
  args[4]="/pbs/home/c/croemer/Donnees/SIG" # france limits
  Output=paste0("/pbs/home/c/croemer/Donnees/ModPred/VC", ThresholdSort, "_", Sys.Date()) #folder to copy models to (fichiers .learner), no "_" else bug !!!
  Fpar="/pbs/home/c/croemer/Donnees/p_export.csv" #the file with data about participations
  Fsl="/pbs/home/c/croemer/Donnees/sites_localites.txt"	#the file with the data about localities
  sfolds_source = paste0("/pbs/home/c/croemer/Donnees/ModPred/VC", ThresholdSort, "_", Sys.Date(), "/temp_sfolds.rds")
  source(paste("/pbs/home/c/croemer/Scripts/","RF_prepare_data.R",sep=""))
  source(paste("/pbs/home/c/croemer/Scripts/","RF_functions.R",sep=""))
  
}

args[6]="participation" #name of sampling event
args[7]="localite" #name of locality in CoordSIG (if DataLoc=T)
args[8]="participation" #name of participation (=sampling event)
args[10]="nb_contacts_nd" #the name of the parameter which gives the metric to predict
Tag=paste0("VC", ThresholdSort) #tag which will be written in the filename, no "_", else bug !!!
CoordinateNames=c("X","Y") #name of columns with coordinates in the locality table (sites_localites.txt)

dir.create(Output)



#### Prepare general dataset ####-------------------------------------------------------

List_data_prepared = prepare_data(args, Fpar, Fsl)
CoordPS = List_data_prepared[[1]] # environmental variables
DataCPL3 = List_data_prepared[[2]]  # bat activity (without absence data)
SelParSL = List_data_prepared[[3]] # list of sampling sessions to know when to add absence data

print("General dataset prepared")

# Identify the variable to predict as nb_contacts
DataCPL3$nb_contacts=subset(DataCPL3, select=args[10])[,1]
test1 = nrow(DataCPL3)
DataCPL3=subset(DataCPL3,!is.na(DataCPL3$nb_contacts))
test2 = nrow(DataCPL3)
ifelse(test1==test2, print("ok"), stop("NA present in activity data!"))

# List species to model
SpeciesList=fread(args[3]) # read species list
ListSp=levels(as.factor(DataCPL3$espece))
ListSp=subset(ListSp,ListSp %in% SpeciesList$Esp)
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

#### Prepare dataset for each species ####------------------------------------------------------

print(ListSp)
for (i in 1:length(ListSp))
{
  DataSp=subset(DataCPL3,DataCPL3$espece==ListSp[i]) # subset species
  #DataSp=subset(DataCPL3,DataCPL3$espece==Sp) # subset species
  print(ListSp[i])
  START1=Sys.time()
  
  # Adds 0 counts using the observation table (avoids user errors but makes the
  # assumption that this table always contains at least 1 species per night)
  DataCPL3_unique=DataCPL3 %>% # prepares the table of the complete set of sampled nights/sites
    select(participation, Nuit, num_micro) %>% 
    unique()
  DataCPL3_unique$Nuit=as.Date(DataCPL3_unique$Nuit)
  
  DataSp$Nuit=as.Date(DataSp$Nuit)
  DataSpSL_w0_2=full_join(DataSp, DataCPL3_unique) # Adds the nights with absence
  colnames(DataSpSL_w0_2)[which(colnames(DataSpSL_w0_2)=="point")]="nom"
  
  # performs a partial join (updates columns of DataSpSL_w0_2 with info of SelParSL)
  n <- names(SelParSL) 
  DataSpSL_w0_2 = DataSpSL_w0_2[SelParSL, on=.(participation), (n):=mget(paste0("i.", n))] 
  
  DataSpSL_w0_2$nb_contacts[is.na(DataSpSL_w0_2$nb_contacts)]=0
  DataSpSL_w0_2$score_max[is.na(DataSpSL_w0_2$score_max)]=0
  
  # Exclude sites outside France limits (square) :
  DataSpSL_w0_2=subset(DataSpSL_w0_2, DataSpSL_w0_2$longitude<10 & DataSpSL_w0_2$longitude>-6 &
                         DataSpSL_w0_2$latitude<52 & DataSpSL_w0_2$latitude>41)
  
  # Exclude data with obvious wrong date (<2010)
  DataSpSL_w0_2 = DataSpSL_w0_2[which(DataSpSL_w0_2$Nuit>as.Date("2010-01-01")),]
  
  DataSaison=full_join(DataSpSL_w0_2,CoordPS) # adds environmental variables to activity data
  
  print(Sys.time())
  
  print("Absence data added")
  
  #add date of year
  if(grepl("/", DataSaison$Nuit[1])){
    Date1=as.Date(substr(DataSaison$Nuit,1,10)
                  ,format="%Y/%m/%Y")    
  }else{
    Date1=as.Date(DataSaison$Nuit)
  }
  
  SpFDate=yday(Date1)
  DataSaison$SpCDate=cos(SpFDate/365*2*pi) # to create a circular variable for date
  DataSaison$SpSDate=sin(SpFDate/365*2*pi) # to create a circular variable for date
  
  # If year effect must be accounted for
  if(YearEffect)
  {
    DataSaison$SpYear=year(Date1)
  }
  
  
  # Add spatial proxies
  if(CoordType == "LongLat"){ # coordinates
    DataSaison$Splatitude = DataSaison$latitude
    DataSaison$Splongitude = DataSaison$longitude
  }
  
  if(CoordType == "EDF"){ # Eurclidian Distance Fields
    DataSaison_sf = st_as_sf(DataSaison, coords = c(x="longitude", y="latitude"), crs = 4326) %>% 
      st_transform(2154)
    
    coords <- as.data.frame(st_coordinates(DataSaison_sf))
    
    # sf object with 5 points: the bounding box of the grid of points + the center
    EDF <- rbind(st_sf(geom = st_sfc(st_point(c(min(coords$X),min(coords$Y))))),
                 st_sf(geom = st_sfc(st_point(c(min(coords$X),max(coords$Y))))),
                 st_sf(geom = st_sfc(st_point(c(max(coords$X),min(coords$Y))))),
                 st_sf(geom = st_sfc(st_point(c(max(coords$X),max(coords$Y))))),
                 st_sf(geom = st_sfc(st_point(c(median(coords$X),median(coords$Y))))))
    EDF <- st_set_crs(EDF, st_crs(DataSaison_sf))
    EDF <- st_distance(DataSaison_sf, EDF)/1000 # calculate distance between the point and each of these 5 points
    EDF <- units::drop_units(EDF)
    EDF <- as.data.frame(EDF)
    names(EDF) <- paste0("EDF", 1:5)
    DataSaison$SpEDF1 = EDF$EDF1
    DataSaison$SpEDF2 = EDF$EDF2
    DataSaison$SpEDF3 = EDF$EDF3
    DataSaison$SpEDF4 = EDF$EDF4
    DataSaison$SpEDF5 = EDF$EDF5
    
    DataSaison$Splatitude = DataSaison$latitude
    DataSaison$Splongitude = DataSaison$longitude
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
  
  Predictors=DataSaison[,..Prednames]
  
  DataSaison = DataSaison %>%  
    drop_na(all_of(Prednames)) %>% #deletes rows without predictor (outdated GI table)
    drop_na(nb_contacts) #deletes rows without contacts (people did not upload their data)
  
  print("Predictors identified")
  
  # Statistics for paper
  Stat1 = DataSaison %>% 
    group_by(latitude, longitude, nom) %>% 
    count()
  print(paste0("N opportunistic sites = ", length(which(grepl("Z", Stat1$nom))), 
               " over a total of ", nrow(Stat1), " sites"))
  print(paste0("N opportunistic nights = ", length(which(grepl("Z", DataSaison$nom))), 
               " over a total of ", nrow(DataSaison), " nights"))
  
  testNA=apply(Predictors,MARGIN=2,FUN=function(x) sum(is.na(x)))
  print(summary(testNA))
  testNA2=apply(Predictors,MARGIN=1,FUN=function(x) sum(is.na(x)))
  print(summary(testNA2))
  
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
    print("Formula found")
  }else{
    formula.Boruta = formula("ActLog10 ~.")
    names.Boruta = Prednames
  }
  
  
  #### Modelling ####-----------------------------------------------------------
  
  # Prepare random and spatial cross-validation indices
  if(!file.exists(sfolds_source)){
    DataSaison_sf = st_as_sf(DataSaison, coords = c(x="longitude", y="latitude"), crs = 4326) %>%
      st_transform(2154)
    aoi = read_sf(dsn = args[4], layer = "France_metropolitaine") %>% # Load France contour
      st_transform(2154)
    set.seed(123)
    START=Sys.time()
    sfolds <- knndm(DataSaison_sf, aoi, k = 10, maxp = 0.5) # k = number of folds
    END=Sys.time()
    print(END-START) # 1 to 1.4 hours
    # beep(2)
    saveRDS(sfolds, paste0(Output, "/temp_sfolds.rds"))
  }else{
    sfolds = readRDS(sfolds_folder)
  }
  sindx <- CreateSpacetimeFolds(data.frame(ID = sfolds$clusters), spacevar = "ID", k = 10)
  sctrl <- caret::trainControl(method="cv", index = sindx$index, savePredictions='final')
  
  
  print("Cross-validation indices prepared")
  
  # EDF model
  proxycovs <- NULL # already coded in Predictors
  
  set.seed(123)
  EDFmod <- fitvalpred_rf(names.Boruta, 
                          proxycovs, 
                          #rctrl, 
                          sctrl, 
                          DataSaison, 
                          NTREE#,
                          #tempstack[[c(basecovs, proxycovs)]]
  )
  
  print("Model done")
  
  #### Save ####----------------------------------------------------------------
  
  if(DoBoruta == T){
    suffix=paste0("_Boruta_", CoordType, "_", NTREE)
  }else{
    suffix=paste0(CoordType, "_", NTREE)
  }
  
  write.csv(EDFmod$tab, paste0(Output, "/Evaluation_", ListSp[i]
                               ,Tag,"_", DateLimit
                               ,"_", suffix, ".csv"))
  # saveRDS(EDFmod$tunemod, paste0(Output, "/RFtune_", ListSp[i]
  #                                ,Tag,"_", DateLimit
  #                                ,"_", suffix, ".rds"))
  saveRDS(EDFmod$spatmod, paste0(Output, "/RFspat_", ListSp[i]
                                 ,Tag,"_", DateLimit
                                 ,"_", suffix, ".rds"))
  rm("EDFmod", "proxycovs")
  
  END1=Sys.time()
  print(END1-START1)
  
}


print(ListSp[i])
print(ThresholdSort)
if(DoBoruta){
  print(paste0("Variables before selection = ", length(Predictors)))
  print(paste0("Variables after selection = ", length(names.Boruta)))
}


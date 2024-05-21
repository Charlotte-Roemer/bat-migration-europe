
# To create random forest models (.learner files) of bat activity

library(data.table)
library(raster)
library(rgdal)
library(rgeos)
library(latticeExtra)
library(randomForest)
library(gdata)
library(spdep) # for Rotation function
library(tidyverse)
library(pgirmess)
library(sp)
library(rsample)
library(foreach)
library(doParallel)
library(beepr)
library(SpatialML)

#to show milliseconds
op <- options(digits.secs=3)

# Sorting threshold (weighted, 0, 50, 90)
ThresholdSort = "weighted"

# choose species
Sp = "all" # choose a species (e.g. "Pippip") or "all"
GroupSel="bat"
#GroupSel=NA #sorting according to the group column of Specieslist (args[3), NA if no sorting

# Do variable selection?
DoBoruta = F

if(ThresholdSort != "weighted"){
  args=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/SpNuit2_DI_", ThresholdSort, "_DataLP_PF_exportTot") #bat activity table. file without csv extension
}else{
  args=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/SpNuit2_DI_", ThresholdSort, "_DataLP_PF_exportTot") #bat activity table. file without csv extension 
}
args[2]="C:/Users/croemer01/Documents/Donnees vigie-chiro/GI_FR_sites_localites" #table with spatial variables (habitat and climate)
args[3]="C:/Users/croemer01/Documents/Donnees vigie-chiro/SpeciesList.csv" # Species list to build models
#args[3]=NA #NA if we want all species without filter (but specify args[5)
#args[4]="Esp" #name of taxa column (useless if args[3] is specified)
args[4]="espece" #name of taxa column (useless if args[3] is specified)
#args[4]="code_sp" #name of taxa column (useless if args[3] is specified)
#args[5]="STRTUR" #name of taxa group (useless if args[3] is specified)
DataLoc=F #TRUE if coordinates are in table args[1
#CoordinateNames=c("longitude_wgs84","latitude_wgs84") 
CoordinateNames=c("X","Y") #name of columns with coordinates in the locality table (sites_localites.txt)
args[6]="participation" #name of sampling event
args[7]="localite" #name of locality in CoordSIG (if DataLoc=T)
args[8]="participation" #name of participation (=sampling event)
args[9]=T #if date (=day-of-year) is provided or not
#args[10]="abondance"
args[10]="nb_contacts_nd" #the name of the parameter which gives the metric to predict
args[11]=40 #number of coordinates projections (must be a division of 360)
#args[12]="C:/Users/croemer01/Documents/Donnees vigie-chiro/Tab_sounds_all_50ScriptLea.csv" # table with bat activity (bat passes)
MinData=1
DM=T #option if you also want a model to predict minimum time lapse between bat passes and sunset and sunrise
Output=paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VC", ThresholdSort, "PG_", Sys.Date()) #folder to copy models to (fichiers .learner), no "_" else bug !!!
Tag=paste0("VC", ThresholdSort) #tag which will be written in the filename, no "_", else bug !!!
effectYear=F # option to add a year effect: to predict population trends
varYear="annee" #name of the year variable (needless if effectYear=F)
W0=F #whether the table args[1 contains the 0 bat passes/night
MergedGI=F #whether habitat-climate variables are in the table args[1 
Fpar="C:/Users/croemer01/Documents/Donnees vigie-chiro/p_export.csv" #the file with data about participations
Fsl="C:/Users/croemer01/Documents/Donnees vigie-chiro/sites_localites.txt"	#the file with the data about localities
ProbThreshold=0 # a filter on the score_max parameter (takes all data superior or equal to this value)
#min_dist = 200 # geographical distance in meters to create a custom test and train dataset
#reps_process = 1 # how many trials should be made to sort train/test dataset (see buffer_CR.r)
DateLimit = Sys.Date()  # to use only data before this date; default = Sys.Date() 

dir.create(Output)

# Reads bat activity data
DataCPL2  = fread(paste0(args[1],".csv"))
DataCPL2$Nuit=as.Date(DataCPL2$Nuit)
DataCPL3= DataCPL2 %>% 
  dplyr::filter(Nuit < DateLimit)

if (!("score_max" %in% names(DataCPL3))){
  DataCPL3 = DataCPL3 %>% 
    group_by(participation, Nuit, num_micro) %>% 
    mutate(score_max = max(as.numeric(nb_contacts))) %>% 
    as.data.frame()
}

#Reads the spatial variables
Sys.time()
CoordSIG=fread(paste0(args[2],".csv"))
print((CoordSIG)[1,])
CoordSIG = CoordSIG %>% 
  rename(longitude = CoordinateNames[1], latitude = CoordinateNames[2])
print((CoordSIG)[1,])

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
}else{
  ListSp = Sp
}

################################################################################
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
  
  #add several rotated coordinates
  CoordDS=as.matrix(cbind(DataSaison$longitude,DataSaison$latitude)) #WGS84
  print("L230")

  for (a in 0:(as.numeric(args[11])-1))
  {
    Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(args[11]))

    DataSaison=cbind(DataSaison,Coordi[,1])
    names(DataSaison)[ncol(DataSaison)]=paste0("SpCoord",a)
  }
  
  # Add material as predictor
  DataSaison$SpRecorder = DataSaison$detecteur_enregistreur_type
  
  print("L246")
  if(effectYear) # if you want to calculate population trends
  {
    DataYear=subset(DataSaison,select=varYear)
    names(DataYear)="year"
    DataSaison$SpYear=DataYear$year
  }
  
  testPred=(substr(names(DataSaison),1,2)=="Sp")
  Prednames=names(DataSaison)[testPred]
  
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

  #DataSaison$ActLog10=log10(DataSaison$nb_contacts+1) #pas sur que ce soit pertinent
  
  Predictors=DataSaison %>%
    select(all_of(Prednames))

  testGLS = (substr(names(Predictors),1,6)!="SpCoor")
  PrednamesGLS=names(Predictors)[testGLS]
  
  Predictors.GLS=DataSaison %>%
    select(all_of(PrednamesGLS))
  Predictors.GLS$SpRecorder=NULL
  
  Dataset.RF.GLS = data.frame("nb_contacts" = DataSaison$nb_contacts, Predictors.GLS)
  
  Dataset.RF = data.frame("nb_contacts" = DataSaison$nb_contacts, Predictors)
  
  cores=detectCores()
  
  coords = as.matrix(cbind(DataSaison$longitude, DataSaison$latitude))
  # RF_GLS <- RFGLS_estimate_spatial(coords, 
  #                                  y = Dataset.RF.GLS$nb_contacts, 
  #                                  X = as.matrix(as.data.frame(Predictors.GLS)), 
  #                                  ntree = 1, 
  #                                  mtry = length(names(Predictors.GLS)),
  #                                  cov.model = "exponential",
  #                                  nthsize = 20
  #                                  , h = cores[1]-1
  #                                  )
  
  f <- as.formula(paste("nb_contacts ~", paste(Prednames[!Prednames %in% "nb_contacts"], collapse = " + ")))
  RF_GRF = grf(f, 
               dframe=Dataset.RF, 
               bw=10,
               ntree=1,
               #mtry=length(Prednames),
               kernel="adaptive", 
               coords=coords)
  
  Dataset.RF_train = 
  
  
  RF_classic <- randomForest(nb_contacts ~.,
                             data=Dataset.RF,
                             replace=F,
                             ntree = 1,
                             mtry=length(Prednames),
                             strata=paste(DataSaison$id_site,
                                          DataSaison$localite),
                             importance=F)
  
  Grid = fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/SysGrid_500m_de_cote_FULL.csv")
  
  # Load table containing habitat variables
  CoordSIG=fread(paste0(args[2],".csv")) 
  CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpAltiS)==F)
  CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpBioC1)==F)
  CoordDS=as.matrix(cbind(CoordSIG$X,CoordSIG$Y)) #WGS84
  
  # Cleans coordinates
  CoordSIG = CoordSIG %>%
    rename_all(~str_replace_all(.,"\\.x",""))
  CoordSIG <- CoordSIG %>% select(-contains(".y"))
  CoordSIG$SpYear=year(Date_of_model)
  
  # Create date vector with a sample each 15 days
  CoordSIG$SpGite=0
  Sp = ListSp[i]
  
  Date_of_model='2018-03-01'
  CoordSIG$SpFDate=yday(Date_of_model)
  CoordSIG$SpCDate=cos(CoordSIG$SpFDate/365*2*pi) # to create a circular variable for date
  CoordSIG$SpSDate=sin(CoordSIG$SpFDate/365*2*pi) # to create a circular variable for date
  
  CoordSIG[is.na(CoordSIG)]=0

  New_coords = as.matrix(cbind(CoordSIG$longitude, CoordSIG$latitude))

  Predictors=DataSaison %>%
    select(all_of(Prednames))
  
  RFGLS_predict_spatial <- RFGLS_predict_spatial(RF_GLS, New_coords, 
                                                 matrix(x[161:200,],40,1))
  
  dataa_Month2 = RFGLS_predict_spatial %>% 
    st_as_sf(coords=c("X", "Y"), crs=4326) %>% 
    st_transform(2154) %>% 
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2]) %>% 
    as.data.frame()
  
  dataa_Month3=data.frame(x=dataa_Month2$x, y=dataa_Month2$y, z=dataa_Month2$pred)
  dataa_Month4 = rasterFromXYZ(dataa_Month3, res = 500)
  
  dataa_Month5 = dataa_Month4 %>% 
    as.data.frame(xy=T)
  dataa_Month5$z[dataa_Month5$z==0] <- NA
  
  library(viridis)
  plot1 <- ggplot()+
    
    # geom_point(data = dataa_Month, 
    #            mapping = aes(x=X, y=Y, col=pred, size=0.00000001) 
    #            ) +
    
    geom_raster(data = dataa_Month5, aes(x = x, y = y, fill = z)) +
    
    scale_fill_viridis(name = "Number of \nbat passes/night",
                       limits = ScaleLimit, 
                       #trans = "pseudo_log",
                       oob = scales::squish,
                       option = "A",
                       na.value = alpha("lightgrey", 0)) +
    
    # geom_sf(data= france_f, size=0.1, 
    #         fill=alpha("lightgrey", 0), colour = "black") +
    
    guides(scale = "none") +
    
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill='transparent'), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg,
          axis.line = element_line(colour = "black"),
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'), #transparent legend panel
          plot.subtitle = element_text(size=12)) +
    
    labs(title = paste0(full_latin_name, "\n", Day_name, " of ", Month_name),
         subtitle = paste("Number of bat passes per night : ",
                          "Mean = ",
                          round(mean(as.data.frame(dataa_Month)$pred),1),
                          ", Max = ",
                          round(max(as.data.frame(dataa_Month)$pred),1),
                          sep="")) +
    ylab("") +
    xlab("") 
  
  
}
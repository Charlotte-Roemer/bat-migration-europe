
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
library(hydroGOF)
library(rsample)
library(beepr)


#to show milliseconds
op <- options(digits.secs=3)


args="/mnt/beegfs/ybas/VigieChiro/Raw/SpNuit2_DI_0_DataLP_PF_exportTot" #bat activity table. file without csv extension

args[2]="/mnt/beegfs/ybas/GI/GI_sites_localites" #table with spatial variables (habitat and climate)
args[3]="/mnt/beegfs/croemer/VigieChiro/SpeciesList.csv" # Species list to build models
#args[3]=NA #NA if we want all species without filter (but specify args[5)
#args[4]="Esp" #name of taxa column (useless if args[3] is specified)
args[4]="espece" #name of taxa column (useless if args[3] is specified)
#args[4]="code_sp" #name of taxa column (useless if args[3] is specified)
#args[5]="STRTUR" #name of taxa group (useless if args[3] is specified)
DataLoc=F #TRUE if coordinates are in table args[1
#CoordinateNames=c("longitude_wgs84","latitude_wgs84") 
CoordinateNames=c("longitude","latitude") #name of columns with coordinates in the locality table (sites_localites.txt)
args[6]="participation" #name of sampling event
args[7]="localite" #name of locality in CoordSIG (if DataLoc=T)
args[8]="participation" #name of participation (=sampling event)
args[9]=T #if date (=day-of-year) is provided or not
#args[10]="abondance"
args[10]="nb_contacts_nd" #the name of the parameter which gives the metric to predict
args[11]=40 #number of coordinates projections (must be a division of 360)
#args[12]="C:/Users/croemer01/Documents/Donnees vigie-chiro/Tab_sounds_all_50ScriptLea.csv" # table with bat activity (bat passes)
MinData=1
#GroupSel="bat"
GroupSel=NA #sorting according to the group column of Specieslist (args[3), NA if no sorting
DM=T #option if you also want a model to predict minimum time lapse between bat passes and sunset and sunrise
Output="/mnt/beegfs/croemer/VigieChiro/ModPred/VC0PG_20220629" #folder to copy models to (fichiers .learner), no "_" else bug !!!
Tag="VC0" #tag which will be written in the filename, no "_", else bug !!!
effectYear=F # option to add a year effect: to predict population trends
varYear="annee" #name of the year variable (needless if effectYear=F)
W0=F #whether the table args[1 contains the 0 bat passes/night
MergedGI=F #whether habitat-climate variables are in the table args[1 
Fpar="/mnt/beegfs/ybas/VigieChiro/p_export.csv" #the file with data about participations
Fsl="/mnt/beegfs/ybas/VigieChiro/sites_localites.txt"	#the file with the data about localities
ProbThreshold=0 # a filter on the score_max parameter (takes all data superior or equal to this value)
min_dist = 200 # geographical distance in meters to create a custom test and train dataset
Sp = "Nyclei" # choose species
reps_process = 1 # how many trials should be made to sort train/test dataset (see buffer_CR.r)

dir.create(Output)

# Reads bat activity data
DataCPL3=fread(paste0(args[1],".csv"))

if (!("score_max" %in% names(DataCPL3))){
  DataCPL3 = DataCPL3 %>% 
    group_by(participation, Nuit, num_micro) %>% 
    mutate(score_max = max(as.numeric(nb_contacts))) %>% 
    as.data.frame()
}

#Reads the spatial variables
Sys.time()
CoordSIG=fread(paste0(args[2],".csv"))
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

#### Select Species ####
print("L190")
print(ListSp)
# for (i in 1:length(ListSp))
# {
#  DataSp=subset(DataCPL3,DataCPL3$espece==ListSp[i]) # subset species
DataSp=subset(DataCPL3,DataCPL3$espece==Sp) # subset species
print("L195")
if(W0){ # if bat activity table contains 0
  DataSaison=DataSp
}else{
  
  # # Add a line for each night
  # complete_Date=SelParSL %>%
  #   drop_na(date_fin) %>% 
  #   rowwise() %>%
  #   transmute(participation,
  #             Nuit = list(seq(as.Date(date_debut), as.Date(date_fin), 
  #                             by = "1 day"))) %>%
  #   unnest(Nuit)
  # 
  # SelParSL_complete_Date=left_join(SelParSL, complete_Date)
  # SelParSL_complete_Date$Nuit=as.Date(SelParSL_complete_Date$Nuit)
  # 
  # Adds 0 counts. 
  # A problem is that some dates_debut or date_fin seem to be wrong 
  # (end too soon although there is data in the next night)
  # thus some nights in DataSp have no match in SelParSL
  # So the assumption is that the error is in the fields users filled in (Particip) 
  # and not in the data (DataCPL3) since the selection script (Lea Mariton) 
  # should have sorted them out if they were incomplete nights
  # so two successive joins have to be done:
  # DataSp$Nuit=as.Date(DataSp$Nuit)
  # DataSpSL_w0=left_join(DataSp,SelParSL) # adds all info except the nights with absence of the species
  # DataSpSL_w0=full_join(DataSpSL_w0, SelParSL_complete_Date) # Adds the nights with absence
  # DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
  # DataSpSL_w0$score_max[is.na(DataSpSL_w0$score_max)]=0
  # 
  # DataSaison=full_join(DataSpSL_w0,CoordPS) # adds environmental variables to activity data
  
  # Adds 0 counts using the observation table (avoids user errors but makes the
  # assumption that this table always contains at least 1 species per night)
  DataCPL3_unique=DataCPL3 %>% # prepares the table of the complete set of sampled nights/sites
    select(participation, Nuit, num_micro) %>% 
    unique()
  DataCPL3_unique$Nuit=as.Date(DataCPL3_unique$Nuit)
  
  DataSp$Nuit=as.Date(DataSp$Nuit)
  DataSpSL_w0_2=full_join(DataSp, DataCPL3_unique) # Adds the nights with absence
  DataSpSL_w0_2=left_join(DataSpSL_w0_2, SelParSL) # adds all other info
  DataSpSL_w0_2$nb_contacts[is.na(DataSpSL_w0_2$nb_contacts)]=0
  DataSpSL_w0_2$score_max[is.na(DataSpSL_w0_2$score_max)]=0
  
  # Exclude sites outside France limits (square) :
  DataSpSL_w0_2=subset(DataSpSL_w0_2, DataSpSL_w0_2$longitude<10 & DataSpSL_w0_2$longitude>-6 &
                         DataSpSL_w0_2$latitude<52 & DataSpSL_w0_2$latitude>41)
  
  # Exclude data with obvious wrong date (<2010)
  DataSpSL_w0_2 = DataSpSL_w0_2[which(DataSpSL_w0_2$Nuit>as.Date("2010-01-01")),]
  
  DataSaison=full_join(DataSpSL_w0_2,CoordPS) # adds environmental variables to activity data
  
  # #################################
  # DataSpSL=merge(DataSp,SelParSL,by="participation")
  # DataSpSL_w0=merge(DataSp,SelParSL,by="participation",all.y=T)
  # DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
  # DataSpSL_w0$score_max[is.na(DataSpSL_w0$score_max)]=0
  # 
  # DataSaison=left_join(DataSpSL_w0,CoordPS)
  # ###################################
  
  
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

testPred=(substr(names(DataSaison),1,2)=="Sp")
Prednames=names(DataSaison)[testPred]
Predictors=DataSaison[,..Prednames]

DataSaison = DataSaison %>%  
  drop_na(all_of(Prednames)) %>% #deletes rows without predictor (outdated GI table)
  drop_na(nb_contacts) #deletes rows without contacts (people did not upload their data)

#seasonal subset
#  DataSaison=subset(DataSaison,substr(DataSaison$`date part. debut`,4,5) %in% Saison)

# print((sum(DataSaison$nb_contacts)>=MinData))
# if(sum(DataSaison$nb_contacts)>=MinData) # if the number of observations is superior to MinData
# {
# DataSPos=subset(DataSaison,DataSaison$nb_contacts>0)

print("L246")
if(effectYear) # if you want to calculate population trends
{
  DataYear=subset(DataSaison,select=varYear)
  names(DataYear)="year"
  DataSaison$SpYear=DataYear$year
}

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

# Custom combine function for datasets of different lengths (used at the end of the loop)
source(paste("/trinity/home/croemer/scripts_VC/","My_combine.R",sep=""))

# Si besoin, s inspirer du script d'Yves BuildClassif_HF.r (qui va avec Modified_randomforest.r)
# où un facteur définit des strates de ce qui est tiré aléatoirement ou pas
# ensuite dans RandomForest, sampsize va pouvoir prendre la valeur 0 (et donc données pas tirée)

rnsmeFinal=NULL
obsFinal=NULL
simFinal=NULL
Test_infoFinal=NULL
rm(ModRF)
for (j in 1:500){
  
  print(j)
  
  # # Recreate dataset by selecting data randomly with a weight on ID probability
  # samp_idx <- sample(seq_len(nrow(DataSaison)), nrow(DataSaison), prob=DataSaison$ww) # can't do it because need raw counts
  # DataSaison_weighted <- DataSaison[samp_idx, ]
  # 
  #create train/test datasets here
  START=Sys.time()
  source(paste("/trinity/home/croemer/scripts_VC/","PCIA_use_buffer_to_create_train_and_test_datasets.R",sep=""))
  END=Sys.time()
  TIMEDIFF=END-START
  TIMEDIFF
  #This script is quite long
  
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
    select(Prednames)
  
  Predictors_Test=DataSaison_Test %>%
    select(Prednames)
  
  # # Check that the dataset is fine
  # ggplot(DataSaison_Train,
  #        aes(x=longitude, y=latitude)) +
  #   geom_point(color="blue", size=0.5) +
  #   geom_point(data= DataSaison_Test, aes(x=longitude, y=latitude), color="red", size=0.5)
  # 
  #### Random forest model for number of bat passes per night ####
  ModRFTemp=randomForest(x=Predictors_Train,y=DataSaison_Train$ActLog10
                         ,replace=F
                         ,ntree=1
                         ,strata=paste(DataSaison_Train$id_site,DataSaison_Train$localite)
                         ,importance=F) #2.1 sec / tree
  
  #predictForest <- predict(ModRF, newdata = Predictors_Test, type="response")
  #table(DataSaison_Test$ActLog10, predictForest)
  
  
  Sys.time()
  
  sim<-predict(ModRFTemp,newdata=Predictors_Test, type="response")
  obs<-DataSaison_Test$ActLog10
  nrsme_result<-nrmse(sim, obs, norm="maxmin")
  Test_info=DataSaison_Test %>% 
    select(participation, Nuit, num_micro, longitude, latitude)
  
  simFinal=c(simFinal, sim)
  obsFinal=c(obsFinal, obs)
  Test_infoFinal=rbind(Test_infoFinal, Test_info)
  rnsmeFinal[j]<-nrsme_result
  
  # Combine trees
  if (exists("ModRF")==TRUE) {ModRF=my_combine(ModRF,ModRFTemp)} else {ModRF=ModRFTemp}
  
}

Dataframe_simobs_Final<-data.frame(simFinal, obsFinal, Test_infoFinal)
Dataframe_simobs_Final$diffSimObs=abs(Dataframe_simobs_Final$simFinal-Dataframe_simobs_Final$obsFinal)
Dataframe_simobs_Final$simFinalBT=10^(Dataframe_simobs_Final$simFinal)#backtransform
Dataframe_simobs_Final$obsFinalBT=10^(Dataframe_simobs_Final$obsFinal)#backtransform
boxplot(rnsmeFinal)
median(rnsmeFinal)

beep(2)

fwrite(Dataframe_simobs_Final,paste0(Output,"/ModRFActLog_",Sp, "_",
                                     Tag,"_evaluation.csv"))

# Predict_Mean = Dataframe_simobs_Final %>%
#   group_by(longitude, latitude, Nuit) %>%
#   summarise(Sim = mean(simFinalBT), Obs = mean(obsFinalBT), Diff=mean(diffSimObs))

# library(ggplot2)
# library(viridis)
# 
# ggplot()+
#   
#   geom_point(data = Predict_Mean, 
#              mapping = aes(x=longitude, y=latitude, col=Diff), size=0.1) +
#   
#   scale_color_viridis(name = "Number of \nbat passes/night",
#                       oob = scales::squish,
#                       option = "A")

# varImpPlot(ModRF,cex=0.5,main=paste("Act",ListSp[i]))
# print(paste("PseudoR2: ",ModRF$rsq[ModRF$ntree]))
# 
save (ModRF,file=paste0(Output,"/ModRFActLog_",Sp
                        ,Tag
                        ,"_PG.learner"))



# }

Sys.time()

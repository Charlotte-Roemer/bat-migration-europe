
# To create random forest models (.learner files) of bat activity

library(data.table)
library(rgdal)
library(raster)
library(rgeos)
library(latticeExtra)
library(randomForest)
library(gdata)
library(spdep)
library(tidyverse)
library(pgirmess)
library(sp)


#to show milliseconds
op <- options(digits.secs=3)


args="C:/Users/croemer01/Documents/Donnees vigie-chiro/SpNuit50_selection" #bat activity table. file without csv extension

args[2]="C:/Users/croemer01/Documents/Donnees vigie-chiro/GI_coordWGS84_SpNuit2_50_DataLP_PF_exportTot" #table with spatial variables (habitat and climate)
args[3]="./Donnees vigie-chiro/SpeciesList.csv" # Species list to build models
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
args[10]="nb_contacts" #the name of the parameter which gives the metric to predict
args[11]=40 #number of coordinates projections (must be a division of 360)
MinData=1
#GroupSel="bat"
GroupSel=NA #sorting according to the group column of Specieslist (args[3), NA if no sorting
DM=T #option if you also want a model to predict minimum time lapse between bat passes and sunset and sunrise
Output="C:/Users/croemer01/Documents/Donnees vigie-chiro/ModPred/VC50PG" #folder to copy models to (fichiers .learner), no "_" else bug !!!
Tag="VC50" #tag which will be written in the filename, no "_", else bug !!!
effectYear=F # option to add a year effect: to predict population trends
varYear="annee" #name of the year variable (needless if effectYear=F)
W0=F #whether the table args[1 contains the 0 bat passes/night
MergedGI=F #whether habitat-climate variables are in the table args[1 
Fpar="C:/Users/croemer01/Documents/Donnees vigie-chiro/Participations_selection.csv" #the file with data about participations
Fsl="C:/Users/croemer01/Documents/Donnees vigie-chiro/Sites_selection.csv"	#the file with the data about localities
ProbThreshold=0 # a filter on the score_max parameter (takes all data superior or equal to this value)

dir.create(Output)


# Reads bat activity data
DataCPL3=fread(paste0(args[1],".csv"))

if (!("score_max" %in% names(DataCPL3))){
  DataCPL3 = DataCPL3 %>% 
    group_by(participation, Nuit, num_micro) %>% 
    mutate(score_max = max(as.numeric(nb_contacts))) %>% 
    as.data.frame()
}


#Reads the grid of coordinates
Sys.time()
CoordSIG=fread(paste0(args[2],".csv"))

Sys.time()
if(!("Group.1" %in% names(CoordSIG)))
{
  if(("longitude" %in% names(CoordSIG))){
    
    CoordSIG$Group.1=CoordSIG$longitude
    CoordSIG$Group.2=CoordSIG$latitude
    CoordSIG$Group.1.x=NULL
    CoordSIG$Group.1.y=NULL
    CoordSIG$Group.2.x=NULL
    CoordSIG$Group.2.y=NULL
    print("case 1")
  }else{
    print("case 2")
    CoordSIG$Group.1=CoordSIG$longitude_wgs84
    CoordSIG$Group.2=CoordSIG$latitude_wgs84
    DataCPL3$Group.1=DataCPL3$longitude_wgs84.x
    DataCPL3$Group.2=DataCPL3$latitude_wgs84.x
    
  }
}else{
  print("case 0")
}

if(!("Group.1" %in% names(DataCPL3)))
{
  if(("longitude" %in% names(DataCPL3))){
    DataCPL3$Group.1=DataCPL3$longitude
    DataCPL3$Group.2=DataCPL3$latitude
  }}


if(!MergedGI){ # If habitat-climate variables are not in the bat activity table
  
  if(!DataLoc) # If coordinates are not in the bat activity table
  {
    #reads participation data
    Particip=fread(Fpar,fill=T)
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
    
    #list of coordinates existing in this dataset
    ListPar=levels(as.factor(DataCPL3$participation))
    SelPar=subset(Particip,Particip$participation %in% ListPar)
    SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))
    CoordPar=aggregate(SelParSL$participation
                       ,by=c(list(SelParSL$longitude),list(SelParSL$latitude),list(SelParSL$participation))
                       ,FUN=length)
    CoordPar$x=NULL
    CoordPar$participation=CoordPar$Group.3
    
  }else{
    print("L103")
    if(W0){ # if bat activity table contains 0
      DataTot=merge(DataCPL3,CoordSIG,by=c("Group.1","Group.2"))
    }else{
      ColCode2=match(args[7],names(DataCPL3))
      ColCode3=match(args[8],names(DataCPL3))
      DataCPL3$participation=as.data.frame(DataCPL3)[,ColCode3]
      DataCPL3$localite=as.data.frame(DataCPL3)[,ColCode2]
      SelParSL=subset(DataCPL3,select=c("participation","localite"))
      print("L108")
      SelParSL=unique(SelParSL)
      CoordPar0=subset(CoordSIG,select=c("Group.1","Group.2",args[6]))
      CoordPar=merge(CoordPar0,SelParSL,by.x=args[6],by.y="localite")
      names(CoordPar)[4]="Group.3"
      
    }
  }
  
  
  if(!W0){ # if bat activity table does not contains 0
    CoordPS=merge(CoordPar,CoordSIG,by=c("Group.1","Group.2"))
    
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

# For each species create a model
print("L190")
print(ListSp)
for (i in 1:length(ListSp))
{
  DataSp=subset(DataCPL3,DataCPL3$espece==ListSp[i]) # subset species
  print("L195")
  if(W0){ # if bat activity table contains 0
    DataSaison=DataSp
  }else{
    DataSpSL=merge(DataSp,SelParSL,by="participation")
    
    print(paste(ListSp[i],nrow(DataSp),Sys.time()))
    
    # Adds 0 counts
    DataSpSL_w0=merge(DataSp,SelParSL,by="participation",all.y=T)
    DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
    DataSpSL_w0$score_max[is.na(DataSpSL_w0$score_max)]=0
    
    DataSaison=merge(DataSpSL_w0,CoordPS
                     ,by.x=c("participation")
                     ,by.y=c("Group.3"))
    
    print(Sys.time())
  }
  print("L213")
  
  #add date of year
  if(args[9])
  {
    if(grepl("/", DataSaison$date_debut[1])){
      Date1=as.Date(substr(DataSaison$date_debut,1,10)
                    ,format="%Y/%m/%Y")    
    }else{
      Date1=as.Date(DataSaison$date_debut)
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
  CoordDS=as.matrix(cbind(DataSaison$Group.1,DataSaison$Group.2))
  print("L230")
  
  for (a in 0:(as.numeric(args[11])-1))
  {
    Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(args[11]))
    
    DataSaison=cbind(DataSaison,Coordi[,1])
    names(DataSaison)[ncol(DataSaison)]=paste0("SpCoord",a)
  }
  
  #seasonal subset
  #  DataSaison=subset(DataSaison,substr(DataSaison$`date part. debut`,4,5) %in% Saison)
  
  print((sum(DataSaison$nb_contacts)>=MinData))
  if(sum(DataSaison$nb_contacts)>=MinData) # if the number of observations is superior to MinData
  {
    DataSPos=subset(DataSaison,DataSaison$nb_contacts>0)
    
    print("L246")
    if(effectYear) # if you want to calculate population trends
    {
      DataYear=subset(DataSaison,select=varYear)
      names(DataYear)="year"
      DataSaison$SpYear=DataYear$year
    }
    
    testPred=(substr(names(DataSaison),1,2)=="Sp")
    Prednames=names(DataSaison)[testPred]
    Predictors=DataSaison[,Prednames]
    
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
    
    #### Random forest model for number of bat passes per night ####
    ModRF=randomForest(x=Predictors,y=DataSaison$ActLog10
                       ,replace=T
                       ,strata=paste(DataSaison$id_site,DataSaison$localite)
                       ,importance=T
    ) #2.1 sec / tree
    Sys.time()
    
    
    varImpPlot(ModRF,cex=0.5,main=paste("Act",ListSp[i]))
    print(paste("PseudoR2: ",ModRF$rsq[ModRF$ntree]))
    
    #test if species is a bat
    test=match(ListSp[i],SpeciesList$Esp)
    Bat=(SpeciesList$Group[test]=="bat")
    
    save (ModRF,file=paste0(Output,"/ModRFActLog_",ListSp[i]
                            ,Tag
                            ,".learner")) 
    
    
    if((Bat)&(DM)) # if species is a bat and if you also want a model to predict minimum 
      # time lapse between bat passes and sunset and sunrise
    {
      
      DataSaison$DM=DataSaison$indice_gite
      DataSaisonDM=subset(DataSaison,!is.na(DataSaison$DM))
      if(nrow(DataSaisonDM)>0){
        testPredDM=(substr(names(DataSaisonDM),1,7)=="SpCoord")
        Prednames=names(DataSaisonDM)[testPredDM]
        Prednames=c(Prednames,"SpCDate","SpSDate")
        PredictorsDM=as.data.table(DataSaisonDM)[,..Prednames]
        
        DataSaisonDM$Log10DM=DataSaisonDM$DM
        print("L330")
        Sys.time()
        
        #### Random forest model for timelapse between sunset and first bat pass (index for roost) ####
        
        ModRF_DM=randomForest(x=PredictorsDM,y=DataSaisonDM$Log10DM
                              ,replace=T
                              ,strata=paste(DataSaisonDM$id_site,DataSaisonDM$localite)
                              ,importance=T
        ) #0.1 sec / tree
        Sys.time()
        #varImpPlot(ModRF_DM,cex=0.5,main=paste("DM",ListSp[i]))
        
        save (ModRF_DM,file=paste0(Output,"/ModRFRoost_",ListSp[i]
                                   ,Tag,".learner")) 
        print("L342")
        DataSaisonDM$Log10DM=DataSaison$indice_repos_nocturne
        DataSaisonDM=subset(DataSaisonDM,!is.na(DataSaisonDM$Log10DM))
        Sys.time()
        
        #### Random forest model for timelapse between... (index for swarming) ####
        
        ModRF_DM=randomForest(x=PredictorsDM,y=DataSaisonDM$Log10DM
                              ,replace=T
                              ,strata=paste(DataSaisonDM$id_site,DataSaisonDM$localite)
                              ,importance=T
        ) #0.1 sec / tree
        Sys.time()
        #varImpPlot(ModRF_DM,cex=0.5,main=paste("DM",ListSp[i]))
        
        save (ModRF_DM,file=paste0(Output,"/ModRFSwarm_",ListSp[i]
                                   ,Tag,".learner")) 
        
        
      }
      #DataSaisonDM$predDM=ModRF_DM$predicted
      #print(spplot(DataSaisonDM,zcol="predDM",main=paste(ListSp[i],"DM")))  
      
    }
  }
}


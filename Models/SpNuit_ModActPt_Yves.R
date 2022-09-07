library(data.table)
library(rgdal)
library(raster)
#library(sp)
#library(ggplot2)
#library(MASS)
library(rgeos)
library(latticeExtra)
library(randomForest)
#library(glmmTMB)
library(gdata)
library(spdep)
#pour afficher les milisecondes
op <- options(digits.secs=3)
#Saison=c("05","06","07") #obsolete


args="mnt/VigieChiro/Raw/SpNuit2DI_0_DataLP_PF_exportTot" #fichier sans l'extension csv
args="mnt/VigieChiro/SpNuit2Valid_0_PG" #fichier sans l'extension csv
#args="SpNuit2_5090_DataLP_PF_exportTot"
#args="./VigieChiro/STOC-EPS/data_FrenchBBS_squarre_Diane_20180628_allSp_2001_2018"

args[2]="mnt/GI/GI_sites_localites" #tableau des covariables spatiales (habitat et climat)
#args[2]="mnt/GI/GI_localites_stoc"
args[3]="SpeciesList.csv"
#args[3]="SpeciesListMig2.csv" 
#args[3]=NA #NA si on veut faire tout sans filtre (mais spécifier args[5)
#args[4]="Esp" #name of taxa column (useless if args[3] is specified)
args[4]="espece" #name of taxa column (useless if args[3] is specified)
#args[4]="code_sp" #name of taxa column (useless if args[3] is specified)
#args[5]="STRTUR" #name of taxa group (useless if args[3] is specified)
DataLoc=F #TRUE si les coordonnées sont dans la table "args[1"
#CoordinateNames=c("longitude_wgs84","latitude_wgs84") 
CoordinateNames=c("longitude","latitude") #nom des colonnes de coordonnées dans la table localités (sites_localites.txt)
args[6]="participation" #name of sampling event
args[7]="localite" #name of locality in CoordSIG (if DataLoc=T)
args[8]="participation" #name of participation (=sampling event)
args[9]=T #if date (=day-of-year) is provided or not
#args[10]="abondance"
args[10]="nb_contacts_nd" #le nom du paramètre qui donne la métrique à prédire
args[11]=40 #number of coordinates projections (must be a division of 360)
MinData=1
#GroupSel="bat"
GroupSel=NA #tri en fonction de la colonne groupe de Specieslist (args[3), NA si aucun tri
DM=T #option si on veut aussi un modèle qui prédit le décalage de temps minimum entre les contacts et les coucher-lever de soleil
Output="mnt/ModPred/VC50PG" #dans quel dossier on copie les modèles (fichiers .learner), pas de "_" sinon bug !!!
Tag="VC50_PG" #tag qui sera inscrit dans le nom de fichier, pas de "_" sinon bug !!!
effectYear=F # option si on veut que le modèle intégre l'année, c'est à dire la tendance temporelle
varYear="annee" #nom de la variable année (inutile si effectYear=F)
W0=F #si la table args[1 contient les activités nulles
MergedGI=F #si les variables habitats-climat sont dans la table args[1 
Fpar="mnt/VigieChiro/p_export_forLinux.csv" #le fichier avec les données de participation
Fsl="mnt/VigieChiro/sites_localites.csv"	#le fichier avec les données de localités
ProbThreshold=0 # un filtre sur le paramètre score_max (prend toutes les données supérieures ou égales à cette valeur)

dir.create(Output)


#recup�ration des donn�es chiros
DataCPL3=fread(paste0(args[1],".csv"))

#recup gridSIG
Sys.time()
CoordSIG=fread(paste0(args[2],".csv"))
#print(names(CoordSIG))
#summary(CoordSIG)
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

if(!MergedGI){


if(!DataLoc)
{
  #r�cup�ration des donn�es participation
  Particip=fread(Fpar,fill=T)
  #r�cup�ration des localit�s
  SiteLoc=fread(Fsl)
  Gite=mapply(function(x,y) 
    ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
    ,SiteLoc$commentaire
    ,SiteLoc$localite)
  SiteLoc$SpGite=as.numeric(Gite)
  
  #liste des coordonn�es existantes dans ce jeu de donn�es
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
if(W0){
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
#SelParSL=CoordSIG
#SelParSL$participation=CoordPar$Group.3

#ColLat=match(args[7],names(DataCPL3))  
#Latitude=as.data.frame(DataCPL3)[,ColLat]  
#Latitude=as.numeric(gsub(",",".",Latitude))
}
}


if(!W0){
CoordPS=merge(CoordPar,CoordSIG,by=c("Group.1","Group.2"))

test=(is.na(CoordPS))
test2=apply(test,MARGIN=1,sum)
test3=apply(test,MARGIN=2,sum)
plot(test2)
plot(test3)

CoordPS[is.na(CoordPS)]=0

testPar=grepl(args[6],names(CoordPS))
numPar=subset(c(1:length(testPar)),testPar)
print("L149")
CoordPS$participation=as.data.frame(CoordPS)[,numPar[1]]
}
}
print("L152")
print(args[3]!="NA")
if(args[3]!="NA")
   {
SpeciesList=fread(args[3])
ListSp=levels(as.factor(DataCPL3$espece))
ListSp=subset(ListSp,ListSp %in% SpeciesList$Esp)

}else{
  Group=args[5]
  colTaxa=match(args[4],names(DataCPL3))
  DataCPL3$espece=as.data.frame(DataCPL3)[,colTaxa]
  Esp=unique(as.data.frame(DataCPL3)[,colTaxa])
  ListSp=levels(as.factor(Esp))
  Metric=args[10]
  DataCPL3$nb_contacts=subset(DataCPL3,select=Metric)
      SpeciesList=data.table(cbind(Group,Esp))
      fwrite(SpeciesList,paste0("SpeciesList_",Group,substr(Sys.time(),1,10),".csv"))
}

 Metric=args[10]
  DataCPL3$nb_contacts=subset(DataCPL3,select=Metric)
 print("L183")
 print(nrow(DataCPL3))
  DataCPL3=subset(DataCPL3,!is.na(DataCPL3$nb_contacts))
print(nrow(DataCPL3))
 
#France_departement
#FranceD= shapefile("C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp")
#Sys.time()



if(!is.na(GroupSel))
{
  SpSel=subset(SpeciesList,SpeciesList$Group %in% GroupSel)
  ListSp=subset(ListSp,ListSp %in% SpSel$Esp)
}

#ListSp=c("Barbar","Eptser","Hypsav","Minsch","Myoalc","Myodau","Myoema"
#        ,"Myomys"
#       ,"Myonat","Nyclas","Nyclei","Nycnoc","Pipkuh","Pipnat","Pippip"
#      ,"Pippyg","Pleaus"
#     ,"Pleaur","Rhifer","Rhihip","Tadten")

print("L190")
print(ListSp)
for (i in 1:length(ListSp))
{
  DataSp=subset(DataCPL3,DataCPL3$espece==ListSp[i])
  print("L195")
  if(W0){
  DataSaison=DataSp
  }else{
  DataSpSL=merge(DataSp,SelParSL,by="participation")
  #fwrite(DataSpSL,paste0("./VigieChiro/DataSp/DataSpSL_",ListSp[i],".csv"))
  
  print(paste(ListSp[i],nrow(DataSp),Sys.time()))
  
  DataSpSL_w0=merge(DataSp,SelParSL,by="participation",all.y=T)
  DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
  DataSpSL_w0$score_max[is.na(DataSpSL_w0$score_max)]=0
  
#print(names(DataSpSL_w0))
  DataSaison=merge(DataSpSL_w0,CoordPS
                   ,by.x=c("participation")
                   ,by.y=c("Group.3"))
  
  print(Sys.time())
  }
  print("L213")
    #print(names(DataSaison))
  #add date of year
  if(args[9])
  {
  Date1=as.Date(substr(DataSaison$date_debut,1,10)
                ,format="%d/%m/%Y")    
  #print(head(Date1))
  SpFDate=yday(Date1)
  DataSaison$SpCDate=cos(SpFDate/365*2*pi)
  DataSaison$SpSDate=sin(SpFDate/365*2*pi)
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
    #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
    #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
    DataSaison=cbind(DataSaison,Coordi[,1])
    names(DataSaison)[ncol(DataSaison)]=paste0("SpCoord",a)
  }
  
    #seasonal subset
  #  DataSaison=subset(DataSaison,substr(DataSaison$`date part. debut`,4,5) %in% Saison)
  
  print((sum(DataSaison$nb_contacts)>=MinData))
  if(sum(DataSaison$nb_contacts)>=MinData)
  {
    DataSPos=subset(DataSaison,DataSaison$nb_contacts>0)
    #NbReplicatsSpatiaux=nlevels(as.factor(as.character(DataSPos$Coord)))
    print("L246")
    if(effectYear)
    {
    	DataYear=subset(DataSaison,select=varYear)
    	names(DataYear)="year"
    	DataSaison$SpYear=DataYear$year
    }
    
    testPred=(substr(names(DataSaison),1,2)=="Sp")
    Prednames=names(DataSaison)[testPred]
    Predictors=DataSaison[,..Prednames]
    #print(names(Predictors))
    
    
    testNA=apply(Predictors,MARGIN=2,FUN=function(x) sum(is.na(x)))
    print(summary(testNA))
    testNA2=apply(Predictors,MARGIN=1,FUN=function(x) sum(is.na(x)))
    print(summary(testNA2))
    
    #seuillage score
    print(summary(DataSaison$score_max))
    print(summary(DataSaison$score_max>ProbThreshold))
    DataSaison$nb_contacts=ifelse(DataSaison$score_max>ProbThreshold,DataSaison$nb_contacts,0)
    
    DataSaison$ActLog10=log10(DataSaison$nb_contacts+1) #pas sur que ce soit pertinent
    print(summary(DataSaison$ActLog10))
    Sys.time()
    ModRF=randomForest(x=Predictors,y=DataSaison$ActLog10
                                ,replace=T
                                ,strata=paste(DataSaison$id_site,DataSaison$localite)
                                ,importance=T
                       ) #2.1 sec / tree
    Sys.time()
    
    varImpPlot(ModRF,cex=0.5,main=paste("Act",ListSp[i]))
    print(paste("PseudoR2: ",ModRF$rsq[ModRF$ntree]))
    #coordinates(DataSaison) <- c("Group.1", "Group.2")
    #proj4string(DataSaison) <- CRS("+init=epsg:4326") # WGS 84
    #DataSaison$pred=ModRF$predicted
    #print(spplot(DataSaison,zcol="pred",main=ListSp[i]))  
    
    #test if species is a bat
    test=match(ListSp[i],SpeciesList$Esp)
    Bat=(SpeciesList$Group[test]=="bat")
    
    save (ModRF,file=paste0(Output,"/ModRFActLog_",ListSp[i]
                            ,Tag
                            ,".learner")) 
    
    
    if((Bat)&(DM))
    {
    #DataSaison$DM=pmin(DataSaison$min_decalage_coucher,DataSaison$min_decalage_lever)
    #DataSaisonDM=subset(DataSaison,DM>0)
    DataSaison$DM=DataSaison$indice_gite
    DataSaisonDM=subset(DataSaison,!is.na(DataSaison$DM))
    if(nrow(DataSaisonDM)>0){
    testPredDM=(substr(names(DataSaisonDM),1,7)=="SpCoord")
    Prednames=names(DataSaisonDM)[testPredDM]
    Prednames=c(Prednames,"SpCDate","SpSDate")
    PredictorsDM=as.data.table(DataSaisonDM)[,..Prednames]
    
   # DataSaisonDM$Log10DM=log10(DataSaisonDM$DM/60+1)
    DataSaisonDM$Log10DM=DataSaisonDM$DM
    print("L330")
    Sys.time()
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


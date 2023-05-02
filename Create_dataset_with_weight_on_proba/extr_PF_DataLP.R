library(data.table)
library(StreamMetabolism)
#library(dplyr)


if(length(args)<2){
  
  args="PrefPart"
  args[4]="C:/Users/croemer01/Documents/Donnees vigie-chiro/export/export_5ff.csv"
  #args[4]="test.csv"
  args[10]="C:/Users/croemer01/Documents/Donnees vigie-chiro/export/"
  #ETAPE 0 - IMPORT DES TABLES
  #bien renommer les chemins en fonction de l'ordi utilise
  #et verifier les versions (date, import complet ou non)
  DataDir="C:/Users/croemer01/Documents/Donnees vigie-chiro/"
  args[3]="0"
  args[12]=""
  args[13]=""
  args[14]=""
  args[15]=F #sort out doubtful data (probable hardware problems)
  args[16]="mnt/VigieChiro/export_validtot210408.txt"
  args[17]=F #correct for validation or not
  
  
}else{
  DataDir="C:/Users/croemer01/Documents/Donnees vigie-chiro/"
}

#table "donnees"
DataE=fread(args[4])

test=("606e250b8de067001092583e" %in% DataE$participation)
print(paste("test 24",test))


Sys.time()
DataPF=subset(DataE,substr(DataE$donnee,1,3)=="Car")

test=("606e250b8de067001092583e" %in% DataPF$participation)
print(paste("test 26",test))


if(nrow(DataPF)>0)
{
  Sys.time()
  rm(DataE)
  
  #table "participations"
  Particip=fread(paste0(DataDir,"/p_export_forLinux.csv")) 
  test=("606e250b8de067001092583e" %in% Particip$participation)
  print(paste("test 43",test))
  
  Particip=as.data.frame(Particip)
  print("dim Particip :")
  print(dim(Particip))
  #table "localit�s"
  SiteLoc=fread(paste0(DataDir,"/sites_localites.txt")
                ,sep="\t")
  print("dim SiteLoc :")
  print(dim(SiteLoc))
  
  
  LatMin=0
  LatMax=90
  LongMin=-180
  LongMax=180
  
  find_modes<- function(x) {
    modes <- NULL
    for ( i in 2:(length(x)-1) ){
      if ( (x[i] > x[i-1]) & (x[i] > x[i+1]) ) {
        modes <- c(modes,i)
      }
    }
    if ( length(modes) == 0 ) {
      modes = 'This is a monotonic distribution'
    }
    return(modes)
  }
  
  find_thresholds<- function(x) {
    modes <- NULL
    for ( i in 2:(length(x)-1) ){
      if ( (x[i] < x[i-1]) & (x[i] < x[i+1]) ) {
        modes <- c(modes,i)
      }
    }
    if ( length(modes) == 0 ) {
      modes = 'This is a monotonic distribution'
    }
    return(modes)
  }
  
  
  
  f2pPF <- function(x) #get date-time data from recording file names
  {
    if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
    op <- options(digits.secs = 3)
    pretemps <- paste(substr(x, nchar(x) - 18, nchar(x)-4), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
    strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
  }
  
  microdroitPF<-function(x)
  {
    substr(x,nchar(x)-20,nchar(x)-20)=="1"
    
  }
  
  microPF<-function(x)
  {
    substr(x,nchar(x)-20,nchar(x)-20)
  }
  
  
  #pour afficher les milisecondes
  op <- options(digits.secs=3)
  #pour reset
  #options(op)
  
  
  #ETAPE 1 - formattage des tables et de leurs attributs
  
  #merge Localites et participations
  PartProt=substr(Particip$site,1,22)
  table(PartProt)
  PartPF=subset(Particip,PartProt=="Vigiechiro - Point Fix")
  table(PartPF$point)
  length(subset(PartPF$point,substr(PartPF$point,1,1)!="Z"))
  test=("606e250b8de067001092583e" %in% PartPF$participation)
  print(paste("test 123",test))
  
  test=subset(PartPF,PartPF$participation=="606e250b8de067001092583e")
  print(summary(test))
  print("L127")
  
  test=subset(SiteLoc,SiteLoc$site=="Vigiechiro - Point Fixe-890285")
  #print(test)
  
  
  m1=paste(SiteLoc$site,SiteLoc$nom)
  m2=paste(PartPF$site,PartPF$point)
  test=match(m2,m1)
  #print(summary(subset(test,PartPF$participation=="5d59ad24ac1509000fedcfaf")))
  SiteLoc=as.data.frame(SiteLoc)
  
  LocaPart=PartPF
  for (c in 1:ncol(SiteLoc))
  {
    if(!names(SiteLoc)[c] %in% names(LocaPart)){
      LocaPart=cbind(LocaPart,SiteLoc[test,c])
      names(LocaPart)[ncol(LocaPart)]=names(SiteLoc)[c]
    }
  }
  LocaPart=subset(LocaPart,!is.na(LocaPart$longitude))
  
  #PartPF$nom=PartPF$point
  # PartPF$point=NULL
  
  #LocaPart = join(x=SiteLoc,y=PartPF
  #                       ,by=c("site","nom")
  #              ,type="inner")
  
  #LocaPart=merge(SiteLoc,PartPF,by=c("site","nom"))
  #LocaPart=merge(SiteLoc,PartPF,by.x=c("site","nom"),by.y=c("site","point"))
  #LocaPart=merge(SiteLoc,PartPF,by="m1")
  #LocaPart$m1=NULL
  #LocaPart$site=LocaPart$site.x
  #LocaPart$site.x=NULL
  #LocaPart$site.y=NULL
  #LocaPart$point=NULL
  
  print(paste("nb points manquants :",nrow(PartPF)-nrow(LocaPart),"/",nrow(PartPF)))
  print(dim(LocaPart))
  test=("606e250b8de067001092583e" %in% LocaPart$participation)
  print(paste("test 129",test))
  test=subset(LocaPart,LocaPart$site=="Vigiechiro - Point Fixe-890285")
  #print(test)
  
  
  #DataE=subset(DataE,DataE$espece!="noise")
  colnames(DataPF)[10]="temps_fin"
  LocaPartData=as.factor(substr(DataPF$donnee,1,27)) #recuperation de l'identifiant du point/tron�on
  Sys.time()
  #Datamicro=as.character(sapply(DataPF$donnee,FUN=microdroitPF)) # r�cup�ration du num�ro du micro (4 min)
  Sys.time()
  Datamicro2=sapply(DataPF$donnee,FUN=microPF) # r�cup�ration du num�ro du micro (1e5 donn�es / secondes)
  Sys.time()
  Datamicro2[is.na(as.numeric(Datamicro2))]=0
  #DataPF$Datamicro2=Datamicro2
  #test=dcast(DataPF,participation~Datamicro2,fun.aggregate=length)
  Sys.time()
  DataMic0=aggregate(Datamicro2,by=list(DataPF$participation)
                     ,FUN=function(x) sum(x=="0"))
  Sys.time()
  DataMicL=aggregate(Datamicro2
                     ,by=c(list(DataPF$participation),list(Datamicro2))
                     ,FUN=length)
  Sys.time()
  DataMicN=aggregate(DataMicL$Group.2
                     ,by=list(DataMicL$Group.1)
                     ,FUN=function(x) max(as.numeric(as.character(x))))
  Sys.time()
  #test=match(DataMic0$Group.1,DataMicN$Group.1)
  #plot(test,c(1:length(test)))
  DataMic0$probleme=((DataMic0$x==0)|(DataMicN$x>3))
  test=match(DataPF$participation,DataMic0$Group.1)
  ProblemeMic=DataMic0$probleme[test]
  DataPF$DataMicFinal=as.numeric(Datamicro2)*(1-ProblemeMic)
  
  #TempsEnregistrement2=sapply(DataPF$donnee,FUN=f2pPF) #long � tourner
  
  DataSel2=cbind(DataPF,LocaPartData)
  rm(DataPF)
  rm(LocaPartData)
  rm(Datamicro2)
  
  test=("606e250b8de067001092583e" %in% DataSel2$participation)
  print(paste("test 150",test))
  
  #calcul du temps (en secondes depuis 01/01/1970)
  Sys.time()
  TempsEnregistrement2=sapply(DataSel2$donnee,FUN=f2pPF) #long � tourner (3e5 donn�es/min)
  #print(head(TempsEnregistrement2))
  test=subset(DataSel2,is.na(TempsEnregistrement2))
  #print(summary(test))
  Partbug=levels(as.factor(test$participation))
  fwrite(as.data.frame(Partbug),paste0(DataDir,"Partbug_PF_LP_",basename(args[4])))
  Sys.time()
  pourDateNuit=TempsEnregistrement2-12*3600 #bricolage-d�calage de 12 heures pour ramener � la date du d�but de nuit
  Sys.time()
  DateNuit=as.Date.POSIXct(pourDateNuit) # date of the beginning of the night
  Sys.time()
  DateJour=as.Date.POSIXct(TempsEnregistrement2) # date (UTC+0)
  DataSel2$TempsEnregistrement2=TempsEnregistrement2
  DataSel2$DateNuit=DateNuit
  DataSel2$DateJour=DateJour
  rm(DateJour)
  rm(DateNuit)
  rm(TempsEnregistrement2)
  rm(pourDateNuit)
  Sys.time()
  gc()
  
  
  bugmatch=match(LocaPart$participation,Partbug)
  test=("606e250b8de067001092583e" %in% LocaPart$participation)
  print(paste("test 183",test))
  
  LocaPart2=subset(LocaPart,is.na(bugmatch))
  test=("606e250b8de067001092583e" %in% LocaPart2$participation)
  print(paste("test 184",test))
  #print(head(LocaPart2))
  
  Sys.time()
  DataLP=merge(DataSel2,LocaPart2,by="participation") #35 sec
  Sys.time()
  rm(DataSel2)
  gc()
  
  test=("606e250b8de067001092583e" %in% DataLP$participation)
  print(paste("test 185",test))
  
  #purge des champs inutiles pour gagner de la m�moire (� remonter ?)
  ListePurge=c("proprietaire","num site",
               "observateur.x",
               "email.y",
               "id_protocole",
               "protocole",
               "localite",
               "date.y",
               "observateur.y",
               "nb_wav",
               "nb_ta",
               "nb_tc",
               "dif_wav_ta",
               "pourc_dif",
               "trait_fin",
               "detecteur_enregistreur_numero_serie",
               "canal_expansion_temps",
               "canal_enregistrement_direct",
               "micro0_numero_serie",
               "micro1_numero_serie",
               "commentaire.x",
               "commentaire.y")
  Sys.time()
  DataLP[,(ListePurge):=NULL]
  Sys.time()
  
  #compute modes
  Lpar=unique(DataLP$participation)
  
  DataLP$doubtful=NA
  
  print(paste("compute modes:",basename(args[4]),Sys.time()))
  DataLPM=DataLP[0,]
  for (c in 1:length(Lpar))
  {
    Datac=subset(DataLP,DataLP$participation==Lpar[c])
    Lspc=unique(Datac$espece)
    for (d in 1:length(Lspc))
    {
      Datai=subset(Datac,Datac$espece==Lspc[d])
      if(nrow(Datai)>1){
        Dflow=density(Datai$probabilite)
        
        #plot(Dflow,main=Lspc[d])
        modes=density(Datai$probabilite)$x[find_modes(Dflow$y)]
        thresholds=density(Datai$probabilite)$x[find_thresholds(Dflow$y)]
        modesTRUE=subset(modes,modes>0.5)
        modesFALSE=subset(modes,modes<=0.5)
        if(length(modesTRUE)>0)
        {
          if(length(modesFALSE)>0)
          {
            thresSel=subset(thresholds,(thresholds>max(modesFALSE))&
                              (thresholds<min(modesTRUE)))
            Thres=thresSel[1]
          }else{
            Thres=0
          }
        }else{
          Thres=1
        }
        
      }else{
        Thres=0.5
      }
      
      Datai$doubtful=(Datai$probabilite<Thres)
      DataLPM=rbind(DataLPM,Datai) 
    }
  }
  #barplot(table(DataLPM$doubtful,DataLPM$espece),las=2,cex.names=0.6
  #       ,ylim=c(0,10000))
  #boxplot(DataLPM$probabilite~DataLPM$doubtful)
  Sys.time()
  fwrite(DataLPM,paste0(args[10],"/DataLP_PF_",basename(args[4])),row.names=F) # 1 min
  Sys.time()
  
  test=("606e250b8de067001092583e" %in% DataLPM$participation)
  print(paste("test 265",test))
  
  
}
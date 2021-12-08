library(data.table)

# Main script calling Predict_Act and Interpole (and others if specified)


ScriptFolder="C:/Users/sarah/Desktop/Stage/Cartes" #chemin du dossier o? se trouve les scripts suivants
PredictAct="/Predict_Act_V2.R"
#PredictDM="/Predict_DM.R" #inutile pour toi
#PredictPresence="/Predict_Presence.R" #inutile pour toi
#Interpole="/Interpole.R"


Prefix="C:/Users/sarah/Desktop/Stage/Cartes/modeles/modeles_utilises" #repertoire o? se trouve les mod?les RF
args=vector()
args[2]="C:/Users/sarah/Desktop/Stage/Cartes/GI_SysGrid__3e+05" #table contenant les covariables habitats calcul?s sur la grille de pr?diction
args[3]="15/06/2020" #date of prediction
#args[3]="All" #All indicate that it must be calculated over the year (i.e 24 dates)
#args[4]="SpeciesList.csv"
args[4]=NA #NA pour prendre toutes les esp?ces, sinon fournir un csv avec la liste
#ListPattern=50 #Threshold
#ListPattern="xportTot" #Threshold
#ListPattern="_filtree" #Threshold
ListPattern="5090" #pattern permettant d'identifier les modèles ? utiliser par leur suffixe (caractères juste avant l'extension ".learner")
#Output="/mnt/beegfs/ybas/ModPred/Veolia/"
Output="C:/Users/sarah/Desktop/Stage/Cartes/output" #où les fichiers de sortie sont copiés

#args[7]="France_dep_L93_2A_2B"
args[7]="C:/Users/sarah/Desktop/Stage/Cartes/France_dep_L93.shp" #contour de la France
args[8]=100000 #raster pixel size?
args[9]=F #DM ? predire ?
args[10]=T #Act ? predire ?
args[12]=F #Presence ? pr?dire ?
args[11]=40 #number of coordinates projections (must be a division of 360)
#GroupFilter="bat"
GroupFilter=NA # tri par groupe possible en fonction de la liste d'esp?ce (NA si toutes esp?ces)
#PropGroup="Plecos"
PropGroup=NA #obsolete ?
Raster=F
MaxSample=500 #mettre à 1e12 qd ok

# Create vector of dates to be predicted
if(args[3]=="All")
{
  Month=c(1:12)
  Day=c(1,15)
}else{
  Month=substr(args[3],4,5)
  Day=substr(args[3],1,2)
}


dir.create(Output)

# If args[4] is not NA, will replace SpeciesList with the list provided
print(Prefix)
if(!is.na(args[4]))
{
  SpeciesList=fread(args[4])
}else{
  ListMod=list.files(Prefix,pattern=paste0(ListPattern[1],".learner"))
  print(ListMod)
  Species=tstrsplit(ListMod,split="_")[[2]]
  SpeciesList=data.frame(Esp=Species)
}
ListSp=SpeciesList$Esp

# To make particular filtering by groups
if(!is.na(GroupFilter))
{
  ListSp=subset(ListSp,SpeciesList$Group %in% GroupFilter)
  SpeciesList=subset(SpeciesList,SpeciesList$Group %in% GroupFilter)
}

Mois=substr(args[3],4,5)
#ListSp="thusii"
#ListSp=c("Myotis","Plecos","PipKN","NlTt","MyoAqu"
#,"RhiEH")
#ListSp="NlTt"

# ???
if(is.list(ListPattern))
{
  ListSp=substr(unlist(ListPattern),3,8)
}else{
  ListSp=tstrsplit(ListSp,split=ListPattern[1])[[1]]
  print(ListSp)
}

# For each Day/Month, for each species, find the random forest model ####
# And call associated scripts to run them (Predict_Act and Interpole)

for (f in Month)
{
  print(f)
  for (g in Day)
  {
    print(g)
    args[3]=paste(g,f,"2020",sep="/") # Create date
    
    
    for (h in 1:length(ListSp))
    {
      if(is.list(ListPattern))
      {
        Pattern=unlist(ListPattern)[h]
      }else{
        Pattern=ListPattern
      }
      
      if(!is.na(Pattern))
      {
        
        Suffix=paste0(Pattern,".learner")  
      }else{
        Suffix=paste0("NA.learner")  
        
      }
      
      print(paste(h,ListSp[h]))
      args[1]=ListSp[h]
      ModRF_file=paste0(Prefix,"/ModRFDecMin_",ListSp[h],Suffix)  
      
      if(file.exists(ModRF_file))
      {
        
        if(args[9]==T)
        {
          if (SpeciesList$Group[h]=="bat")
          {
            source(paste0(ScriptFolder,PredictDM))
            print(ListSp[h])
            args[6]=FilName
            ModRF=ModRF_DM
            source(paste0(ScriptFolder,Interpole))
          }
        }
      }
      
      ModRF_file=paste0(Prefix,"/ModRFActLog_",ListSp[h],Suffix)  
      print(ModRF_file)
      if(file.exists(ModRF_file))
      {
        print("L151")
        
        if(args[10])
        {
          print(Sys.time())
          source(paste0(ScriptFolder,PredictAct))
          print(ListSp[h])
          args[6]=FilName
          if(Raster){
            source(paste0(ScriptFolder,Interpole))
          }
          print(Sys.time())
        }
      }else{
        if(is.na(PropGroup))
        {
          ModRF_file=paste0(Prefix,"/ModRFPresence_",ListSp[h],Suffix)  
          
        }else{
          ModRF_file=paste0(Prefix,"/ModRFPresence_",PropGroup,Suffix)  
          
        }
        
        if(file.exists(ModRF_file))
        {
          if(args[12])
          {
            source(paste0(ScriptFolder,PredictPresence))
            print(as.character(ListSp[h]))
            args[6]=FilName
            #source(paste0(ScriptFolder,Interpole))
          }
        }
      }
    }
  }
}


